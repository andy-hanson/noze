#include "./checkExpr.h"

#include "../util/arrUtil.h"
#include "./checkCall.h"
#include "./inferringType.h"
#include "./typeFromAst.h"

namespace {
	template <typename Cb>
	inline auto withMessageImpl(ExprCtx* ctx, const NewAndMessageInfo info, Cb cb) {
		// Since 'new' is not a closure, discard lambdas from outside of it.
		// Note: 'messageOrFunctionLocals' and 'lambdas' intentionally not passed down.
		ExprCtx newCtx {
			ctx->checkCtx,
			ctx->structsAndAliasesMap,
			ctx->funsMap,
			ctx->commonTypes,
			ctx->outermostFun,
			some<const NewAndMessageInfo>(info)};
		return cb(&newCtx);
	}

	template <typename Cb>
	inline auto withLambda(ExprCtx* ctx, LambdaInfo* info, Cb cb) {
		push(ctx->arena(), &ctx->lambdas, info);
		auto res = cb();
		LambdaInfo* popped = mustPop(&ctx->lambdas);
		assert(ptrEquals(popped, info));
		return res;
	}

	struct ExprAndType {
		const Expr expr;
		const Type type;
	};

	const CheckedExpr checkExprWorker(ExprCtx* ctx, const ExprAst ast, Expected* expected);

	const Expr checkExpr(ExprCtx* ctx, const ExprAst* ast, Expected* expected) {
		return checkExpr(ctx, *ast, expected);
	}

	const ExprAndType checkAndInfer(ExprCtx* ctx, const ExprAst ast) {
		Expected expected = Expected::infer();
		const Expr expr = checkExpr(ctx, ast, &expected);
		return ExprAndType{expr, inferred(&expected)};
	}

	const Expr checkAndExpect(ExprCtx* ctx, const ExprAst ast, const Opt<const Type> expected) {
		Expected et = Expected{expected};
		return checkExpr(ctx, ast, &et);
	}

	const Expr checkAndExpect(ExprCtx* ctx, const ExprAst ast, const Type expected) {
		return checkAndExpect(ctx, ast, some<const Type>(expected));
	}

	const Expr checkAndExpect(ExprCtx* ctx, const ExprAst* ast, const Type expected) {
		return checkAndExpect(ctx, *ast, expected);
	}

	const Expr checkAndExpect(ExprCtx* ctx, const ExprAst* ast, const StructInst* expected) {
		return checkAndExpect(ctx, ast, Type{expected});
	}

	const CheckedExpr checkCond(ExprCtx* ctx, const SourceRange range, const CondAst ast, Expected* expected) {
		const Expr* cond = ctx->alloc(checkAndExpect(ctx, ast.cond, ctx->commonTypes->_bool));
		const Expr* then = ctx->alloc(checkExpr(ctx, ast.then, expected));
		const Expr* elze = ctx->alloc(checkExpr(ctx, ast.elze, expected));
		return CheckedExpr{Expr{range, Expr::Cond{inferred(expected), cond, then, elze}}};
	}

	struct ArrExpectedType {
		const Bool isFromExpected;
		const StructInst* arrType;
		const Type elementType;
	};

	const CheckedExpr checkCreateArr(ExprCtx* ctx, const SourceRange range, const CreateArrAst ast, Expected* expected) {
		const ArrExpectedType aet = [&]() {
			if (has(ast.elementType)) {
				const Type ta = typeFromAst(ctx, force(ast.elementType));
				const StructInst* arrType = instantiateStructNeverDelay(
					ctx->arena(),
					ctx->commonTypes->arr,
					arrLiteral<const Type>(ctx->arena(), { ta }));
				return ArrExpectedType{False, arrType, ta};
			} else {
				const Opt<const Type> opT = tryGetDeeplyInstantiatedType(ctx->arena(), expected);
				if (has(opT)) {
					const Type t = force(opT);
					if (t.isStructInst()) {
						const StructInst* si = t.asStructInst();
						if (ptrEquals(si->decl, ctx->commonTypes->arr))
							return ArrExpectedType{True, si, only<const Type>(si->typeArgs)};
					}
				}
				return todo<const ArrExpectedType>("can't get expected element type for new-arr");
			}
		}();

		const Arr<const Expr> args = map<const Expr>{}(ctx->arena(), ast.args, [&](const ExprAst it) {
			return checkAndExpect(ctx, it, aet.elementType);
		});
		const Expr expr = Expr{range, Expr::CreateArr{aet.arrType, args}};
		return aet.isFromExpected
			? CheckedExpr{expr}
			: check(ctx, expected, Type{aet.arrType}, expr);
	}

	struct RecordAndIsBuiltinByVal {
		const StructBody::Record record;
		// True if this is the 'by-val' type. (Not if it's another type that happens to be by-val.)
		const Bool isBuiltinByVal;
	};

	const CheckedExpr checkCreateRecord(ExprCtx* ctx, const SourceRange range, const CreateRecordAst ast, Expected* expected) {
		Cell<const Bool> cellTypeIsFromExpected { False };
		const Type t = [&]() {
			if (has(ast.type))
				return typeFromAst(ctx, force(ast.type));
			else {
				cellSet<const Bool>(&cellTypeIsFromExpected, True);
				const Opt<const Type> opT = tryGetDeeplyInstantiatedType(ctx->arena(), expected);
				if (!has(opT))
					todo<void>("checkCreateRecord -- no expected type");
				return force(opT);
			}
		}();
		const Bool typeIsFromExpected = cellGet(&cellTypeIsFromExpected);
		if (!t.isStructInst())
			todo<void>("checkCreateRecord -- not a struct type");
		const StructInst* si = t.asStructInst();
		const StructDecl* decl = si->decl;

		const Opt<const RecordAndIsBuiltinByVal> opRecord = decl->body().match(
			[](const StructBody::Bogus) {
				return none<const RecordAndIsBuiltinByVal>();
			},
			[&](const StructBody::Builtin) {
				if (ptrEquals(decl, ctx->commonTypes->byVal)) {
					// We know this will be deeply instantiated since we did that at the beginning of this function
					const Type inner = only<const Type>(si->typeArgs);
					if (inner.isStructInst()) {
						const StructBody body = inner.asStructInst()->body();
						if (body.isRecord())
							return some<const RecordAndIsBuiltinByVal>(RecordAndIsBuiltinByVal{body.asRecord(), True});
					}
				}
				return none<const RecordAndIsBuiltinByVal>();
			},
			[](const StructBody::Record r) {
				return some<const RecordAndIsBuiltinByVal>(RecordAndIsBuiltinByVal{r, False});
			},
			[](const StructBody::Union) {
				return none<const RecordAndIsBuiltinByVal>();
			},
			[](const StructBody::Iface) {
				return none<const RecordAndIsBuiltinByVal>();
			});

		if (has(opRecord)) {
			const RecordAndIsBuiltinByVal record = force(opRecord);
			const Arr<const StructField> fields = record.record.fields;
			if (!sizeEq(ast.args, fields)) {
				ctx->addDiag(range, Diag{Diag::WrongNumberNewStructArgs{decl, size(fields), size(ast.args)}});
				return typeIsFromExpected ? bogusWithoutAffectingExpected(range) : bogus(expected, range);
			} else {
				const Arr<const Expr> args = mapZip<const Expr>{}(
					ctx->arena(),
					fields,
					ast.args,
					[&](const StructField field, const ExprAst arg) {
						const Type expectedType = instantiateType(ctx->arena(), field.type, si);
						return checkAndExpect(ctx, arg, expectedType);
					});
				const Expr expr = Expr{range, Expr::CreateRecord{si, args}};

				if (ctx->outermostFun->noCtx() && !record.isBuiltinByVal) {
					const Opt<const ForcedByValOrRef> forcedByValOrRef = record.record.forcedByValOrRef;
					const Bool isAlwaysByVal = _or(
						isEmpty(fields),
						(has(forcedByValOrRef) && force(forcedByValOrRef) == ForcedByValOrRef::byVal));
					if (!isAlwaysByVal)
						ctx->addDiag(range, Diag{Diag::CreateRecordByRefNoCtx{decl}});
				}

				return typeIsFromExpected ? CheckedExpr{expr} : check(ctx, expected, Type(si), expr);
			}
		} else {
			if (!decl->body().isBogus())
				ctx->addDiag(range, Diag{Diag::CantCreateNonRecordStruct{decl}});
			return typeIsFromExpected ? bogusWithoutAffectingExpected(range) : bogus(expected, range);
		}
	}

	struct ExpectedLambdaType {
		const StructDecl* funStruct;
		const StructDecl* nonSendFunStruct;
		const Bool isSendFun;
		const Arr<const Type> paramTypes;
		const Type nonInstantiatedPossiblyFutReturnType;
	};

	const Opt<const ExpectedLambdaType> getExpectedLambdaType(ExprCtx* ctx, const SourceRange range, Expected* expected) {
		const Opt<const Type> expectedType = shallowInstantiateType(expected);
		if (!has(expectedType) && !force(expectedType).isStructInst()) {
			ctx->addDiag(range, Diag{Diag::ExpectedTypeIsNotALambda{expectedType}});
			return none<const ExpectedLambdaType>();
		}
		const StructInst* expectedStructInst = force(expectedType).asStructInst();
		const StructDecl* funStruct = expectedStructInst->decl;

		const Opt<const CommonTypes::LambdaInfo> opInfo = ctx->commonTypes->getFunStructInfo(funStruct);
		if (!has(opInfo)) {
			ctx->addDiag(range, Diag{Diag::ExpectedTypeIsNotALambda{expectedType}});
			return none<const ExpectedLambdaType>();
		}

		const CommonTypes::LambdaInfo info = force(opInfo);
		const Type nonInstantiatedNonFutReturnType = first(expectedStructInst->typeArgs);
		const Arr<const Type> nonInstantiatedParamTypes = tail(expectedStructInst->typeArgs);
		const Arr<const Type> paramTypes = map<const Type>{}(ctx->arena(), nonInstantiatedParamTypes, [&](const Type it) {
			const Opt<const Type> op = tryGetDeeplyInstantiatedTypeFor(ctx->arena(), expected, it);
			if (!has(op))
				todo<void>("getExpectedLambdaType");
			return force(op);
		});
		const Type nonInstantiatedReturnType = info.isSend
			? ctx->makeFutType(nonInstantiatedNonFutReturnType)
			: nonInstantiatedNonFutReturnType;
		return some<const ExpectedLambdaType>(
			ExpectedLambdaType{funStruct, info.nonSend, info.isSend, paramTypes, nonInstantiatedReturnType});
	}

	const CheckedExpr checkFunAsLambda(
		ExprCtx* ctx,
		const SourceRange range,
		const FunAsLambdaAst ast,
		Expected* expected
	) {
		const Opt<const ExpectedLambdaType> opEt = getExpectedLambdaType(ctx, range, expected);
		if (!has(opEt))
			return bogus(expected, range);

		const ExpectedLambdaType et = force(opEt);

		//TODO: overload resolution. For now, requires that only 1 fun exists.

		const Arr<const FunDecl*> funsInScope = [&]() {
			ArrBuilder<const FunDecl*> funsInScopeBuilder {};
			eachFunInScope(ctx, ast.funName, [&](const CalledDecl called) {
				called.match(
					[&](const FunDecl* f) {
						add(ctx->arena(), &funsInScopeBuilder, f);
					},
					[&](const SpecSig) {
						todo<void>("checkFunAsLambda");
					});
			});
			return finishArr(&funsInScopeBuilder);
		}();

		if (size(funsInScope) != 1)
			todo<void>("checkFunAsLambda");

		const FunDecl* fun = only(funsInScope);

		if (!sizeEq(ast.typeArgs, fun->typeParams))
			// TODO: type inference
			todo<void>("checkFunAsLambda");

		if (!isEmpty(fun->typeParams))
			todo<void>("checkFunAsLambda");

		const Type returnType = fun->returnType();
		const StructInst* type = instantiateStructNeverDelay(
			ctx->arena(),
			et.funStruct,
			prepend<const Type>(ctx->arena(), returnType, et.paramTypes));

		if (!typeEquals(returnType, et.nonInstantiatedPossiblyFutReturnType)) todo<void>("checkFunAsLambda");

		zip(et.paramTypes, fun->params(), [&](const Type paramType, const Param param) {
			if (!typeEquals(paramType, param.type))
				todo<void>("checkFunAsLambda -- param types don't match");
		});

		return check(ctx, expected, Type{type}, Expr{range, Expr::FunAsLambda{fun, type, et.isSendFun}});
	}

	const CheckedExpr checkRef(
		ExprCtx* ctx,
		const Expr expr,
		const SourceRange range,
		const Sym name,
		const Type type,
		const Arr<LambdaInfo*> passedLambdas,
		Expected* expected
	) {
		if (isEmpty(passedLambdas))
			return check(ctx, expected, type, expr);
		else {
			// First of passedLambdas is the outermost one where we found the param/local. This one can access it directly.
			// Inner ones must reference this by a closure field.
			LambdaInfo* l0 = first(passedLambdas);
			// Shouldn't have already closed over it (or we should just be using that)
			assert(!exists(tempAsArr(&l0->closureFields), [&](const ClosureField* it) {
				return symEq(it->name, name);
			}));
			const ClosureField* field = nu<const ClosureField>{}(
				ctx->arena(),
				name,
				type,
				ctx->alloc(expr),
				mutArrSize(&l0->closureFields));
			push<const ClosureField*>(ctx->arena(), &l0->closureFields, field);
			return checkRef(
				ctx,
				Expr{range, Expr::ClosureFieldRef{field}},
				range,
				name,
				type,
				tail(passedLambdas),
				expected);
		}
	}

	const CheckedExpr checkIdentifier(ExprCtx* ctx, const SourceRange range, const IdentifierAst ast, Expected* expected) {
		const Sym name = ast.name;

		if (!mutArrIsEmpty(&ctx->lambdas)) {
			// Innermost lambda first
			for (const size_t i : RangeDown{mutArrSize(&ctx->lambdas)}) {
				LambdaInfo* lambda = mutArrAt(&ctx->lambdas, i);
				// Lambdas we skipped past that need closures
				const Arr<LambdaInfo*> passedLambdas = slice(tempAsArr(&ctx->lambdas), i + 1);

				for (const Local* local : tempAsArr(&lambda->locals))
					if (symEq(local->name, name))
						return checkRef(ctx, Expr{range, Expr::LocalRef{local}}, range, name, local->type, passedLambdas, expected);

				for (const Param* param : ptrsRange(lambda->lambdaParams))
					if (symEq(param->name, name))
						return checkRef(ctx, Expr{range, Expr::ParamRef{param}}, range, name, param->type, passedLambdas, expected);

				// Check if we've already added something with this name to closureFields to avoid adding it twice.
				for (const ClosureField* field : tempAsArr(&lambda->closureFields))
					if (symEq(field->name, name))
						return checkRef(ctx, Expr{range, Expr::ClosureFieldRef{field}}, range, name, field->type, passedLambdas, expected);
			}
		}

		const Arr<LambdaInfo*> allLambdas = tempAsArr(&ctx->lambdas);

		for (const Local* local : tempAsArr(&ctx->messageOrFunctionLocals))
			if (symEq(local->name, name))
				return checkRef(ctx, Expr{range, Expr::LocalRef{local}}, range, name, local->type, allLambdas, expected);

		if (has(ctx->newAndMessageInfo)) {
			const NewAndMessageInfo nmi = force(ctx->newAndMessageInfo);
			for (const Param* param : ptrsRange(nmi.instantiatedParams))
				if (symEq(param->name, name))
					return checkRef(ctx, Expr{range, Expr::ParamRef{param}}, range, name, param->type, allLambdas, expected);
			for (const Expr::NewIfaceImpl::Field* field : ptrsRange(nmi.fields))
				if (symEq(field->name, name))
					return checkRef(ctx, Expr{range, Expr::IfaceImplFieldRef{field}}, range, name, field->type, allLambdas, expected);
		} else
			for (const Param* param : ptrsRange(ctx->outermostFun->params()))
				if (symEq(param->name, name))
					return checkRef(ctx, Expr{range, Expr::ParamRef{param}}, range, name, param->type, allLambdas, expected);

		return checkIdentifierCall(ctx, range, name, expected);
	}

	const CheckedExpr checkNoCallLiteral(ExprCtx* ctx, const SourceRange range, const Str literal, Expected* expected) {
		return check(ctx, expected, Type{ctx->commonTypes->str}, Expr{range, Expr::StringLiteral{copyStr(ctx->arena(), literal)}});
	}

	const CheckedExpr checkLiteral(ExprCtx* ctx, const SourceRange range, const LiteralAst ast, Expected* expected) {
		if (isExpectingString(expected, ctx->commonTypes->str) || !hasExpected(expected))
			return checkNoCallLiteral(ctx, range, ast.literal, expected);
		else {
			const CallAst call = CallAst{
				shortSymAlphaLiteral("literal"),
				emptyArr<const TypeAst>(),
				arrLiteral<const ExprAst>(ctx->arena(), { ExprAst{range, ExprAstKind{ast}} })};
			return checkCall(ctx, range, call, expected);
		}
	}

	template <typename Cb>
	auto checkWithLocal_worker(ExprCtx* ctx, const Local* local, Cb cb) {
		MutArr<const Local*>* locals = mutArrIsEmpty(&ctx->lambdas)
			? &ctx->messageOrFunctionLocals
			: &mustPeek(&ctx->lambdas)->locals;
		push(ctx->arena(), locals, local);
		auto res = cb();
		const Local* popped = mustPop(locals);
		assert(ptrEquals(popped, local));
		return res;
	}

	const Expr checkWithLocal(ExprCtx* ctx, const Local* local, const ExprAst ast, Expected* expected) {
		return checkWithLocal_worker(ctx, local, [&]() {
			return checkExpr(ctx, ast, expected);
		});
	}

	const Arr<const Param> checkFunOrSendFunParamsForLambda(
		Arena* arena,
		const Arr<const LambdaAst::Param> paramAsts,
		const Arr<const Type> expectedParamTypes
	) {
		return mapZipWithIndex<const Param>{}(
			arena,
			paramAsts,
			expectedParamTypes,
			[&](const LambdaAst::Param ast, const Type expectedParamType, const size_t index) {
				return Param{ast.range, ast.name, expectedParamType, index};
			});
	}

	const CheckedExpr checkLambdaWorker(
		ExprCtx* ctx,
		const SourceRange range,
		const Arr<const LambdaAst::Param> paramAsts,
		const ExprAst bodyAst,
		Expected* expected
	) {
		Arena* arena = ctx->arena();
		const Opt<const ExpectedLambdaType> opEt = getExpectedLambdaType(ctx, range, expected);
		if (!has(opEt))
			return bogus(expected, range);

		const ExpectedLambdaType et = force(opEt);

		if (!sizeEq(paramAsts, et.paramTypes)) {
			printf("Number of params should be %zu, got %zu\n", size(et.paramTypes), size(paramAsts));
			todo<void>("checkLambdaWorker -- # params is wrong");
		}

		const Arr<const Param> params = checkFunOrSendFunParamsForLambda(arena, paramAsts, et.paramTypes);
		LambdaInfo info = LambdaInfo{params};
		Expected returnTypeInferrer = copyWithNewExpectedType(expected, et.nonInstantiatedPossiblyFutReturnType);

		const Expr* body = withLambda(ctx, &info, [&]() {
			// Note: checking the body of the lambda may fill in candidate type args
			// if the expected return type contains candidate's type params
			return ctx->alloc(checkExpr(ctx, bodyAst, &returnTypeInferrer));
		});

		const Type actualPossiblyFutReturnType = inferred(&returnTypeInferrer);
		const Opt<const Type> actualNonFutReturnType = [&]() {
			if (et.isSendFun) {
				if (actualPossiblyFutReturnType.isStructInst()) {
					const StructInst* ap = actualPossiblyFutReturnType.asStructInst();
					return ptrEquals(ap->decl, ctx->commonTypes->fut)
						? some<const Type>(only(ap->typeArgs))
						: none<const Type>();
				} else
					return none<const Type>();
			} else
				return some<const Type>(actualPossiblyFutReturnType);
		}();
		if (!has(actualNonFutReturnType)) {
			ctx->addDiag(range, Diag{Diag::SendFunDoesNotReturnFut{actualPossiblyFutReturnType}});
			return bogus(expected, range);
		} else {
			const StructInst* instFunStruct = instantiateStructNeverDelay(
				arena, et.funStruct, prepend<const Type>(arena, force(actualNonFutReturnType), et.paramTypes));

			const Expr::Lambda lambda = Expr::Lambda{
				params,
				body,
				freeze(&info.closureFields),
				instFunStruct,
				et.nonSendFunStruct,
				et.isSendFun,
				actualPossiblyFutReturnType};

			return CheckedExpr{Expr{range, lambda}};
		}
	}

	const CheckedExpr checkLambda(ExprCtx* ctx, const SourceRange range, const LambdaAst ast, Expected* expected) {
		return checkLambdaWorker(ctx, range, ast.params, *ast.body, expected);
	}

	const CheckedExpr checkLet(ExprCtx* ctx, const SourceRange range, const LetAst ast, Expected* expected) {
		const ExprAndType init = checkAndInfer(ctx, *ast.initializer);
		const Local* local = nu<Local>()(
			ctx->arena(),
			ast.name,
			init.type);
		const Expr* then = ctx->alloc(checkWithLocal(ctx, local, *ast.then, expected));
		return CheckedExpr{Expr{range, Expr::Let{local, ctx->alloc(init.expr), then}}};
	}

	const Expr checkWithOptLocal(ExprCtx* ctx, Opt<const Local*> local, const ExprAst ast, Expected* expected) {
		return has(local)
			? checkWithLocal(ctx, force(local), ast, expected)
			: checkExpr(ctx, ast, expected);
	}

	struct UnionAndMembers {
		const StructInst* matchedUnion;
		const Arr<const StructInst*> members;
	};

	const Opt<const UnionAndMembers> getUnionBody(const Type t) {
		if (t.isStructInst()) {
			const StructInst* matchedUnion = t.asStructInst();
			const StructBody body = matchedUnion->body();
			return body.isUnion()
				? some<const UnionAndMembers>(UnionAndMembers{matchedUnion, body.asUnion().members})
				: none<const UnionAndMembers>();
		} else
			return none<const UnionAndMembers>();
	}

	const CheckedExpr checkMatch(ExprCtx* ctx, const SourceRange range, const MatchAst ast, Expected* expected) {
		const ExprAndType matchedAndType = checkAndInfer(ctx, *ast.matched);
		const Opt<const UnionAndMembers> unionAndMembers = getUnionBody(matchedAndType.type);
		if (!has(unionAndMembers)) {
			if (!matchedAndType.type.isBogus())
				ctx->addDiag(ast.matched->range, Diag{Diag::MatchOnNonUnion{matchedAndType.type}});
			return bogus(expected, ast.matched->range);
		} else {
			const StructInst* matchedUnion = force(unionAndMembers).matchedUnion;
			const Arr<const StructInst*> members = force(unionAndMembers).members;

			const Bool badCases = _or(
				!sizeEq(members, ast.cases),
				zipSome(members, ast.cases, [&](const StructInst* member, const MatchAst::CaseAst caseAst) {
					return !symEq(member->decl->name, caseAst.structName);
				}));
			if (badCases) {
				ctx->addDiag(range, Diag{Diag::MatchCaseStructNamesDoNotMatch{members}});
				return bogus(expected, range);
			} else {
				const Arr<const Expr::Match::Case> cases = mapZip<const Expr::Match::Case>{}(
					ctx->arena(),
					members,
					ast.cases,
					[&](const StructInst* member, const MatchAst::CaseAst caseAst) {
						const Opt<const Local*> local = has(caseAst.localName)
							? some<const Local*>(
								nu<Local>()(ctx->arena(), force(caseAst.localName), Type{member}))
							: none<const Local*>();
						const Expr then = isBogus(expected)
							? bogus(expected, range).expr
							: checkWithOptLocal(ctx, local, *caseAst.then, expected);
						return Expr::Match::Case{local, ctx->alloc(then)};
					});
				return CheckedExpr{Expr{range, Expr::Match{ctx->alloc(matchedAndType.expr), matchedUnion, cases, inferred(expected)}}};
			}
		}
	}

	const Arr<const Message> getMessages(const StructInst* ifaceInst) {
		//TODO: what to do if asIface() fails?
		return ifaceInst->body().asIface().messages;
	}

	const CheckedExpr checkMessageSend(ExprCtx* ctx, const SourceRange range, const MessageSendAst ast, Expected* expected) {
		const ExprAndType targetAndType = checkAndInfer(ctx, *ast.target);
		const Expr* target = ctx->alloc(targetAndType.expr);
		const Type targetType = targetAndType.type;

		return targetType.match(
			[&](const Type::Bogus) {
				return bogus(expected, range);
			},
			[&](const TypeParam*) {
				return todo<CheckedExpr>("checkMessageSend on type parameter -- report a diagnostic");
			},
			[&](const StructInst* instIface) {
				const Opt<const Message*> opMessage = findPtr(
					getMessages(instIface),
					[&](const Message* m) {
						return symEq(m->sig.name, ast.messageName);
					});
				if (!has(opMessage)) todo<void>("checkMessageSend");
				const Message* message = force(opMessage);
				const Sig messageSig = message->sig;
				if (size(ast.args) != arity(messageSig))
					todo<void>("checkMessageSend");
				const Arr<const Expr> args = mapZip<const Expr>{}(
					ctx->arena(),
					ast.args,
					messageSig.params,
					[&](const ExprAst argAst, const Param& param) {
						return checkAndExpect(ctx, argAst, instantiateType(ctx->arena(), param.type, instIface));
					});
				const Type returnType = instantiateType(ctx->arena(), messageSig.returnType, instIface);
				return check(ctx, expected, returnType, Expr{range, Expr::MessageSend{target, instIface, message->index, args}});
			});
	}

	const CheckedExpr checkNewActor(ExprCtx* ctx, const SourceRange range, const NewActorAst ast, Expected* expected) {
		const Opt<const Type> t = tryGetDeeplyInstantiatedType(ctx->arena(), expected);
		if (!has(t) || !force(t).isStructInst())
			todo<void>("checkNewActor");
		const StructInst* instIface = force(t).asStructInst();
		const Arr<const Expr::NewIfaceImpl::Field> fields = mapWithIndex<const Expr::NewIfaceImpl::Field>{}(
			ctx->arena(),
			ast.fields,
			[&](const NewActorAst::Field field, const size_t index) {
				const ExprAndType et = checkAndInfer(ctx, *field.expr);
				return Expr::NewIfaceImpl::Field{field.isMutable, field.name, ctx->alloc(et.expr), et.type, index};
			});
		const Arr<const Expr> messageImpls = mapZipPtrs<const Expr>{}(
			ctx->arena(),
			getMessages(instIface),
			ast.messages,
			[&](const Message* message, const NewActorAst::MessageImpl* impl) {
				if (!symEq(message->sig.name, impl->name))
					todo<void>("checkNewActor");
				zip(message->sig.params, impl->paramNames, [&](const Param param, const NameAndRange paramName) {
					if (!symEq(param.name, paramName.name))
						todo<void>("checkNewActor -- param names don't match");
				});
				return withMessageImpl(ctx, NewAndMessageInfo{message->sig.params, fields, message}, [&](ExprCtx* newCtx) {
					// message->sig.returnType is already wrapped in `fut`
					return checkAndExpect(newCtx, impl->body, message->sig.returnType);
				});
			});
		return check(ctx, expected, Type{instIface}, Expr{range, Expr::NewIfaceImpl{instIface, fields, messageImpls}});
	}

	const CheckedExpr checkSeq(ExprCtx* ctx, const SourceRange range, const SeqAst ast, Expected* expected) {
		const Expr* first = ctx->alloc(checkAndExpect(ctx, ast.first, ctx->commonTypes->_void));
		const Expr* then = ctx->alloc(checkExpr(ctx, ast.then, expected));
		return CheckedExpr{Expr{range, Expr::Seq{first, then}}};
	}

	const CheckedExpr checkStructFieldSet(ExprCtx* ctx, const SourceRange range, const StructFieldSetAst ast, Expected* expected) {
		const ExprAndType target = checkAndInfer(ctx, *ast.target);
		const Opt<const StructAndField> opStructAndField = tryGetStructField(target.type, ast.fieldName);
		if (has(opStructAndField)) {
			const StructAndField structAndField = force(opStructAndField);
			const StructInst* structInst = structAndField.structInst;
			const StructField* field = structAndField.field;
			if (!field->isMutable) {
				ctx->addDiag(range, Diag{Diag::WriteToNonMutableField{field}});
				return bogusWithType(expected, range, Type{ctx->commonTypes->_void});
			} else {
				const Expr value = checkAndExpect(ctx, ast.value, field->type);
				const Expr::StructFieldSet sfs = Expr::StructFieldSet{ctx->alloc(target.expr), structInst, field, ctx->alloc(value)};
				return check(ctx, expected, Type{ctx->commonTypes->_void}, Expr{range, sfs});
			}
		} else {
			ctx->addDiag(range, Diag{Diag::WriteToNonExistentField{target.type, ast.fieldName}});
			return bogusWithType(expected, range, Type{ctx->commonTypes->_void});
		}
	}

	const CheckedExpr checkThen(ExprCtx* ctx, const SourceRange range, const ThenAst ast, Expected* expected) {
		const ExprAst lambda = ExprAst{
			range,
			ExprAstKind{LambdaAst{
				arrLiteral<const LambdaAst::Param>(ctx->arena(), { ast.left }),
				ast.then}}};
		const CallAst call = CallAst{
			shortSymAlphaLiteral("then"),
			emptyArr<const TypeAst>(),
			arrLiteral<const ExprAst>(ctx->arena(), { *ast.futExpr, lambda })};
		return checkCall(ctx, range, call, expected);
	}

	const CheckedExpr checkExprWorker(ExprCtx* ctx, const ExprAst ast, Expected* expected) {
		const SourceRange range = ast.range;
		return ast.kind.match(
			[&](const CallAst a) {
				return checkCall(ctx, range, a, expected);
			},
			[&](const CondAst a) {
				return checkCond(ctx, range, a, expected);
			},
			[&](const CreateArrAst a) {
				return checkCreateArr(ctx, range, a, expected);
			},
			[&](const CreateRecordAst a) {
				return checkCreateRecord(ctx, range, a, expected);
			},
			[&](const FunAsLambdaAst a) {
				return checkFunAsLambda(ctx, range, a, expected);
			},
			[&](const IdentifierAst a) {
				return checkIdentifier(ctx, range, a, expected);
			},
			[&](const LambdaAst a) {
				return checkLambda(ctx, range, a, expected);
			},
			[&](const LetAst a) {
				return checkLet(ctx, range, a, expected);
			},
			[&](const LiteralAst a) {
				return checkLiteral(ctx, range, a, expected);
			},
			[&](const MatchAst a) {
				return checkMatch(ctx, range, a, expected);
			},
			[&](const MessageSendAst a) {
				return checkMessageSend(ctx, range, a, expected);
			},
			[&](const NewActorAst a) {
				return checkNewActor(ctx, range, a, expected);
			},
			[&](const SeqAst a) {
				return checkSeq(ctx, range, a, expected);
			},
			[&](const StructFieldSetAst a) {
				return checkStructFieldSet(ctx, range, a, expected);
			},
			[&](const ThenAst a) {
				return checkThen(ctx, range, a, expected);
			});
	}
}

const Expr* checkFunctionBody(
	CheckCtx* checkCtx,
	const ExprAst ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const FunsMap funsMap,
	const FunDecl* fun,
	const CommonTypes* commonTypes
) {
	ExprCtx exprCtx {checkCtx, structsAndAliasesMap, funsMap, commonTypes, fun, none<const NewAndMessageInfo>()};
	return exprCtx.alloc(checkAndExpect(&exprCtx, ast, fun->returnType()));
}

const Expr checkExpr(ExprCtx* ctx, const ExprAst ast, Expected* expected) {
	return checkExprWorker(ctx, ast, expected).expr;
}
