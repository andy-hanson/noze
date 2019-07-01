#include "./checkExpr.h"

#include "../util/arrUtil.h"
#include "./checkCall.h"
#include "./inferringType.h"
#include "./typeFromAst.h"

namespace {
	template <typename Cb>
	inline auto withMessageImpl(ExprCtx& ctx, const NewAndMessageInfo info, Cb cb) {
		// Since 'new' is not a closure, discard lambdas from outside of it.
		// Note: 'messageOrFunctionLocals' and 'lambdas' intentionally not passed down.
		ExprCtx newCtx {
			ctx.checkCtx,
			ctx.structsAndAliasesMap,
			ctx.funsMap,
			ctx.commonTypes,
			ctx.outermostFun,
			some<const NewAndMessageInfo>(info)};
		return cb(newCtx);
	}

	template <typename Cb>
	inline auto withLambda(ExprCtx& ctx, LambdaInfo* info, Cb cb) {
		push(ctx.arena(), ctx.lambdas, info);
		auto res = cb();
		LambdaInfo* popped = mustPop(ctx.lambdas);
		assert(ptrEquals(popped, info));
		return res;
	}

	struct ExprAndType {
		const Expr expr;
		const Type type;
	};

	const CheckedExpr checkExprWorker(ExprCtx& ctx, const ExprAst ast, Expected& expected);

	const Expr checkExpr(ExprCtx& ctx, const ExprAst* ast, Expected& expected) {
		return checkExpr(ctx, *ast, expected);
	}

	const ExprAndType checkAndInfer(ExprCtx& ctx, const ExprAst ast) {
		Expected expected = Expected::infer();
		const Expr expr = checkExpr(ctx, ast, expected);
		return ExprAndType{expr, expected.inferred()};
	}

	const Expr checkAndExpect(ExprCtx& ctx, const ExprAst ast, const Opt<const Type> expected) {
		Expected et = Expected{expected};
		return checkExpr(ctx, ast, et);
	}

	const Expr checkAndExpect(ExprCtx& ctx, const ExprAst ast, const Type expected) {
		return checkAndExpect(ctx, ast, some<const Type>(expected));
	}

	const Expr checkAndExpect(ExprCtx& ctx, const ExprAst* ast, const Type expected) {
		return checkAndExpect(ctx, *ast, expected);
	}

	const Expr checkAndExpect(ExprCtx& ctx, const ExprAst* ast, const StructInst* expected) {
		return checkAndExpect(ctx, ast, Type{expected});
	}

	const CheckedExpr checkCond(ExprCtx& ctx, const SourceRange range, const CondAst ast, Expected& expected) {
		const Expr* cond = ctx.alloc(checkAndExpect(ctx, ast.cond, ctx.commonTypes._bool));
		const Expr* then = ctx.alloc(checkExpr(ctx, ast.then, expected));
		const Expr* elze = ctx.alloc(checkExpr(ctx, ast.elze, expected));
		return CheckedExpr{Expr{range, Expr::Cond{expected.inferred(), cond, then, elze}}};
	}

	struct ArrExpectedType {
		const Bool isFromExpected;
		const StructInst* arrType;
		const Type elementType;
	};

	const CheckedExpr checkCreateArr(ExprCtx& ctx, const SourceRange range, const CreateArrAst ast, Expected& expected) {
		const ArrExpectedType aet = [&]() {
			if (ast.elementType.has()) {
				const Type ta = typeFromAst(ctx, ast.elementType.force());
				const StructInst* arrType = instantiateStructNeverDelay(ctx.arena(), ctx.commonTypes.arr, arrLiteral<const Type>(ctx.arena(), ta));
				return ArrExpectedType{False, arrType, ta};
			} else {
				const Opt<const Type> opT = expected.tryGetDeeplyInstantiatedType(ctx.arena());
				if (opT.has()) {
					const Type t = opT.force();
					if (t.isStructInst()) {
						const StructInst* si = t.asStructInst();
						if (ptrEquals(si->decl, ctx.commonTypes.arr))
							return ArrExpectedType{True, si, only<const Type>(si->typeArgs)};
					}
				}
				return todo<const ArrExpectedType>("can't get expected element type for new-arr");
			}
		}();

		const Arr<const Expr> args = map<const Expr>{}(ctx.arena(), ast.args, [&](const ExprAst it) {
			return checkAndExpect(ctx, it, aet.elementType);
		});
		const Expr expr = Expr{range, Expr::CreateArr{aet.arrType, args}};
		return aet.isFromExpected
			? CheckedExpr{expr}
			: expected.check(ctx, Type{aet.arrType}, expr);
	}

	const CheckedExpr checkCreateRecord(ExprCtx& ctx, const SourceRange range, const CreateRecordAst ast, Expected& expected) {
		Cell<const Bool> typeIsFromExpected { False };
		const Type t = [&]() {
			if (ast.type.has())
				return typeFromAst(ctx, ast.type.force());
			else {
				typeIsFromExpected.set(True);
				const Opt<const Type> opT = expected.tryGetDeeplyInstantiatedType(ctx.arena());
				if (!opT.has())
					todo<void>("checkCreateRecord -- no expected type");
				return opT.force();
			}
		}();
		if (!t.isStructInst())
			todo<void>("checkCreateRecord -- not a struct type");
		const StructInst* si = t.asStructInst();
		const StructDecl* decl = si->decl;

		const Opt<const StructBody::Fields> opFields = decl->body().match(
			[](const StructBody::Bogus) {
				return none<const StructBody::Fields>();
			},
			[&](const StructBody::Builtin) {
				if (ptrEquals(decl, ctx.commonTypes.byVal)) {
					// We know this will be deeply instantiated since we did that at the beginning of this function
					const Type inner = only<const Type>(si->typeArgs);
					if (inner.isStructInst()) {
						const StructBody& body = inner.asStructInst()->body();
						if (body.isFields())
							return some<const StructBody::Fields>(body.asFields());
					}
				}
				return none<const StructBody::Fields>();
			},
			[](const StructBody::Fields f) {
				return some<const StructBody::Fields>(f);
			},
			[](const StructBody::Union) {
				return none<const StructBody::Fields>();
			},
			[](const StructBody::Iface) {
				return none<const StructBody::Fields>();
			});

		if (opFields.has()) {
			const Arr<const StructField> fields = opFields.force().fields;
			if (ast.args.size != fields.size) {
				ctx.addDiag(range, Diag{Diag::WrongNumberNewStructArgs{decl, fields.size, ast.args.size}});
				return typeIsFromExpected.get() ? bogusWithoutAffectingExpected(range) : expected.bogus(range);
			} else {
				//TODO: mapzip
				const Arr<const Expr> args = fillArr<const Expr>{}(ctx.arena(), fields.size, [&](const size_t i) {
					const Type expectedType = instantiateType(ctx.arena(), at(fields, i).type, si);
					return checkAndExpect(ctx, at(ast.args, i), expectedType);
				});
				const Expr expr = Expr{range, Expr::CreateRecord{si, args}};
				return typeIsFromExpected.get() ? CheckedExpr{expr} : expected.check(ctx, Type(si), expr);
			}
		} else {
			if (!decl->body().isBogus())
				ctx.addDiag(range, Diag{Diag::CantCreateNonRecordStruct{decl}});
			return typeIsFromExpected.get() ? bogusWithoutAffectingExpected(range) : expected.bogus(range);
		}
	}

	struct ExpectedLambdaType {
		const StructDecl* funStruct;
		const StructDecl* nonRemoteFunStruct;
		const Bool isRemoteFun;
		const Arr<const Type> paramTypes;
		const Type nonInstantiatedPossiblyFutReturnType;
	};

	const Opt<const ExpectedLambdaType> getExpectedLambdaType(ExprCtx& ctx, const SourceRange range, Expected& expected) {
		const Opt<const Type> expectedType = expected.shallowInstantiateType();
		if (!expectedType.has() && !expectedType.force().isStructInst()) {
			ctx.addDiag(range, Diag{Diag::ExpectedTypeIsNotALambda{expectedType}});
			return none<const ExpectedLambdaType>();
		}
		const StructInst* expectedStructInst = expectedType.force().asStructInst();
		const StructDecl* funStruct = expectedStructInst->decl;

		const Opt<const CommonTypes::LambdaInfo> opInfo = ctx.commonTypes.getFunStructInfo(funStruct);
		if (!opInfo.has()) {
			ctx.addDiag(range, Diag{Diag::ExpectedTypeIsNotALambda{expectedType}});
			return none<const ExpectedLambdaType>();
		}

		const CommonTypes::LambdaInfo info = opInfo.force();
		const Type nonInstantiatedNonFutReturnType = first(expectedStructInst->typeArgs);
		const Arr<const Type> nonInstantiatedParamTypes = tail(expectedStructInst->typeArgs);
		const Arr<const Type> paramTypes = map<const Type>{}(ctx.arena(), nonInstantiatedParamTypes, [&](const Type it) {
			const Opt<const Type> op = expected.tryGetDeeplyInstantiatedTypeFor(ctx.arena(), it);
			if (!op.has())
				todo<void>("getExpectedLambdaType");
			return op.force();
		});
		const Type nonInstantiatedReturnType = info.isRemote ? ctx.makeFutType(nonInstantiatedNonFutReturnType) : nonInstantiatedNonFutReturnType;
		return some<const ExpectedLambdaType>(ExpectedLambdaType{funStruct, info.nonRemote, info.isRemote, paramTypes, nonInstantiatedReturnType});
	}

	const CheckedExpr checkFunAsLambda(ExprCtx& ctx, const SourceRange range, const FunAsLambdaAst ast, Expected& expected) {
		const Opt<const ExpectedLambdaType> opEt = getExpectedLambdaType(ctx, range, expected);
		if (!opEt.has())
			return expected.bogus(range);

		const ExpectedLambdaType et = opEt.force();

		//TODO: overload resolution. For now, requires that only 1 fun exists.

		const Arr<const FunDecl*> funsInScope = [&]() {
			ArrBuilder<const FunDecl*> funsInScopeBuilder {};
			eachFunInScope(ctx, ast.funName, [&](const CalledDecl called) {
				called.match(
					[&](const FunDecl* f) {
						funsInScopeBuilder.add(ctx.arena(), f);
					},
					[&](const SpecSig) {
						todo<void>("checkFunAsLambda");
					});
			});
			return funsInScopeBuilder.finish();
		}();

		if (funsInScope.size != 1)
			todo<void>("checkFunAsLambda");

		const FunDecl* fun = only(funsInScope);

		if (ast.typeArgs.size != fun->typeParams.size)
			// TODO: type inference
			todo<void>("checkFunAsLambda");

		if (!isEmpty(fun->typeParams))
			todo<void>("checkFunAsLambda");

		const Type returnType = fun->returnType();
		const StructInst* type = instantiateStructNeverDelay(ctx.arena(), et.funStruct, prepend<const Type>(ctx.arena(), returnType, et.paramTypes));

		if (!typeEquals(returnType, et.nonInstantiatedPossiblyFutReturnType)) todo<void>("checkFunAsLambda");

		zip(et.paramTypes, fun->params(), [&](const Type paramType, const Param param) {
			if (!typeEquals(paramType, param.type))
				todo<void>("checkFunAsLambda -- param types don't match");
		});

		return expected.check(ctx, Type{type}, Expr{range, Expr::FunAsLambda{fun, type, et.isRemoteFun}});
	}

	const CheckedExpr checkRef(
		ExprCtx& ctx,
		const Expr expr,
		const SourceRange range,
		const Str name,
		const Type type,
		const Arr<LambdaInfo*> passedLambdas,
		Expected& expected
	) {
		if (isEmpty(passedLambdas))
			return expected.check(ctx, type, expr);
		else {
			// First of passedLambdas is the outermost one where we found the param/local. This one can access it directly.
			// Inner ones must reference this by a closure field.
			LambdaInfo* l0 = first(passedLambdas);
			// Shouldn't have already closed over it (or we should just be using that)
			assert(!exists(tempAsArr(l0->closureFields), [&](const ClosureField* it) {
				return strEq(it->name, name);
			}));
			const ClosureField* field = ctx.arena().nu<const ClosureField>()(
				ctx.copyStr(name),
				type,
				ctx.alloc(expr),
				l0->closureFields.size());
			push<const ClosureField*>(ctx.arena(), l0->closureFields, field);
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

	const CheckedExpr checkIdentifier(ExprCtx& ctx, const SourceRange range, const IdentifierAst ast, Expected& expected) {
		const Str name = ast.name;

		if (strEqLiteral(name, "pred"))
			debugger();

		if (!isEmpty(ctx.lambdas)) {
			// Innermost lambda first
			for (const size_t i : RangeDown{ctx.lambdas.size()}) {
				LambdaInfo* lambda = at(ctx.lambdas, i);
				// Lambdas we skipped past that need closures
				const Arr<LambdaInfo*> passedLambdas = slice(tempAsArr(ctx.lambdas), i + 1);

				for (const Local* local : tempAsArr(lambda->locals))
					if (strEq(local->name, name))
						return checkRef(ctx, Expr{range, Expr::LocalRef{local}}, range, name, local->type, passedLambdas, expected);

				for (const Param* param : ptrsRange(lambda->lambdaParams))
					if (strEq(param->name, name))
						return checkRef(ctx, Expr{range, Expr::ParamRef{param}}, range, name, param->type, passedLambdas, expected);

				// Check if we've already added something with this name to closureFields to avoid adding it twice.
				for (const ClosureField* field : tempAsArr(lambda->closureFields))
					if (strEq(field->name, name))
						return checkRef(ctx, Expr{range, Expr::ClosureFieldRef{field}}, range, name, field->type, passedLambdas, expected);
			}
		}

		const Arr<LambdaInfo*> allLambdas = tempAsArr(ctx.lambdas);

		for (const Local* local : tempAsArr(ctx.messageOrFunctionLocals))
			if (strEq(local->name, name))
				return checkRef(ctx, Expr{range, Expr::LocalRef{local}}, range, name, local->type, allLambdas, expected);

		if (ctx.newAndMessageInfo.has()) {
			const NewAndMessageInfo nmi = ctx.newAndMessageInfo.force();
			for (const Param* param : ptrsRange(nmi.instantiatedParams))
				if (strEq(param->name, name))
					return checkRef(ctx, Expr{range, Expr::ParamRef{param}}, range, name, param->type, allLambdas, expected);
			for (const Expr::NewIfaceImpl::Field* field : ptrsRange(nmi.fields))
				if (strEq(field->name, name))
					return checkRef(ctx, Expr{range, Expr::IfaceImplFieldRef{field}}, range, name, field->type, allLambdas, expected);
		} else
			for (const Param* param : ptrsRange(ctx.outermostFun->params()))
				if (strEq(param->name, name))
					return checkRef(ctx, Expr{range, Expr::ParamRef{param}}, range, name, param->type, allLambdas, expected);

		return checkIdentifierCall(ctx, range, name, expected);
	}

	const CheckedExpr checkNoCallLiteral(ExprCtx& ctx, const SourceRange range, const Str literal, Expected& expected) {
		return expected.check(ctx, Type{ctx.commonTypes.str}, Expr{range, Expr::StringLiteral{ctx.copyStr(literal)}});
	}

	const CheckedExpr checkLiteral(ExprCtx& ctx, const SourceRange range, const LiteralAst ast, Expected& expected) {
		if (expected.isExpectingString(ctx.commonTypes.str) || !expected.hasExpected())
			return checkNoCallLiteral(ctx, range, ast.literal, expected);
		else {
			const CallAst call = CallAst{
				strLiteral("literal"),
				emptyArr<const TypeAst>(),
				arrLiteral<const ExprAst>(ctx.arena(), ExprAst{range, ExprAstKind{ast}})};
			return checkCall(ctx, range, call, expected);
		}
	}

	template <typename Cb>
	auto checkWithLocal_worker(ExprCtx& ctx, const Local* local, Cb cb) {
		MutArr<const Local*>& locals = isEmpty(ctx.lambdas)
			? ctx.messageOrFunctionLocals
			: mustPeek(ctx.lambdas)->locals;
		push(ctx.arena(), locals, local);
		auto res = cb();
		const Local* popped = mustPop(locals);
		assert(ptrEquals(popped, local));
		return res;
	}

	const Expr checkWithLocal(ExprCtx& ctx, const Local* local, const ExprAst ast, Expected& expected) {
		return checkWithLocal_worker(ctx, local, [&]() {
			return checkExpr(ctx, ast, expected);
		});
	}

	const Arr<const Param> checkFunOrRemoteFunParamsForLambda(
		Arena& arena,
		const Arr<const LambdaAst::Param> paramAsts,
		const Arr<const Type> expectedParamTypes
	) {
		return mapZipWithIndex<const Param>{}(
			arena,
			paramAsts,
			expectedParamTypes,
			[&](const LambdaAst::Param ast, const Type expectedParamType, const size_t index) {
				return Param{ast.range, copyStr(arena, ast.name), expectedParamType, index};
			});
	}

	const CheckedExpr checkLambdaWorker(
		ExprCtx& ctx,
		const SourceRange range,
		const Arr<const LambdaAst::Param> paramAsts,
		const ExprAst bodyAst,
		Expected& expected
	) {
		Arena& arena = ctx.arena();
		const Opt<const ExpectedLambdaType> opEt = getExpectedLambdaType(ctx, range, expected);
		if (!opEt.has())
			return expected.bogus(range);

		const ExpectedLambdaType et = opEt.force();

		if (paramAsts.size != et.paramTypes.size) {
			printf("Number of params should be %zu, got %zu\n", et.paramTypes.size, paramAsts.size);
			todo<void>("checkLambdaWorker -- # params is wrong");
		}

		const Arr<const Param> params = checkFunOrRemoteFunParamsForLambda(arena, paramAsts, et.paramTypes);
		LambdaInfo info = LambdaInfo{params};
		Expected returnTypeInferrer = expected.copyWithNewExpectedType(et.nonInstantiatedPossiblyFutReturnType);

		const Expr* body = withLambda(ctx, &info, [&]() {
			// Note: checking the body of the lambda may fill in candidate type args if the expected return type contains candidate's type params
			return ctx.alloc(checkExpr(ctx, bodyAst, returnTypeInferrer));
		});

		const Type actualPossiblyFutReturnType = returnTypeInferrer.inferred();
		const Opt<const Type> actualNonFutReturnType = [&]() {
			if (et.isRemoteFun) {
				if (actualPossiblyFutReturnType.isStructInst()) {
					const StructInst* ap = actualPossiblyFutReturnType.asStructInst();
					return ptrEquals(ap->decl, ctx.commonTypes.fut)
						? some<const Type>(only(ap->typeArgs))
						: none<const Type>();
				} else
					return none<const Type>();
			} else
				return some<const Type>(actualPossiblyFutReturnType);
		}();
		if (!actualNonFutReturnType.has()) {
			ctx.addDiag(range, Diag{Diag::RemoteFunDoesNotReturnFut{actualPossiblyFutReturnType}});
			return expected.bogus(range);
		} else {
			const StructInst* instFunStruct = instantiateStructNeverDelay(
				arena, et.funStruct, prepend<const Type>(arena, actualNonFutReturnType.force(), et.paramTypes));

			const Expr::Lambda lambda = Expr::Lambda{
				params,
				body,
				freeze(info.closureFields),
				instFunStruct,
				et.nonRemoteFunStruct,
				et.isRemoteFun,
				actualPossiblyFutReturnType};

			return CheckedExpr{Expr{range, lambda}};
		}
	}

	const CheckedExpr checkLambda(ExprCtx& ctx, const SourceRange range, const LambdaAst ast, Expected& expected) {
		return checkLambdaWorker(ctx, range, ast.params, *ast.body, expected);
	}

	const CheckedExpr checkLet(ExprCtx& ctx, const SourceRange range, const LetAst ast, Expected& expected) {
		const ExprAndType init = checkAndInfer(ctx, *ast.initializer);
		const Local* local = ctx.arena().nu<Local>()(ctx.copyStr(ast.name), init.type);
		const Expr* then = ctx.alloc(checkWithLocal(ctx, local, *ast.then, expected));
		return CheckedExpr{Expr{range, Expr::Let{local, ctx.alloc(init.expr), then}}};
	}

	const Expr checkWithOptLocal(ExprCtx& ctx, Opt<const Local*> local, const ExprAst ast, Expected& expected) {
		return local.has()
			? checkWithLocal(ctx, local.force(), ast, expected)
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

	const CheckedExpr checkMatch(ExprCtx& ctx, const SourceRange range, const MatchAst ast, Expected& expected) {
		const ExprAndType matchedAndType = checkAndInfer(ctx, *ast.matched);
		const Opt<const UnionAndMembers> unionAndMembers = getUnionBody(matchedAndType.type);
		if (!unionAndMembers.has()) {
			if (!matchedAndType.type.isBogus())
				ctx.addDiag(ast.matched->range, Diag{Diag::MatchOnNonUnion{matchedAndType.type}});
			return expected.bogus(ast.matched->range);
		} else {
			const StructInst* matchedUnion = unionAndMembers.force().matchedUnion;
			const Arr<const StructInst*> members = unionAndMembers.force().members;

			const Bool badCases = _or(
				members.size != ast.cases.size,
				zipSome(members, ast.cases, [&](const StructInst* member, const MatchAst::CaseAst caseAst) {
					return !strEq(member->decl->name, caseAst.structName);
				}));
			if (badCases) {
				ctx.addDiag(range, Diag{Diag::MatchCaseStructNamesDoNotMatch{members}});
				return expected.bogus(range);
			} else {
				const Arr<const Expr::Match::Case> cases = mapZip<const Expr::Match::Case>{}(
					ctx.arena(),
					members,
					ast.cases,
					[&](const StructInst* member, const MatchAst::CaseAst caseAst) {
						const Opt<const Local*> local = caseAst.localName.has()
							? some<const Local*>(
								ctx.arena().nu<Local>()(ctx.copyStr(caseAst.localName.force()), Type{member}))
							: none<const Local*>();
						const Expr then = expected.isBogus()
							? expected.bogus(range).expr
							: checkWithOptLocal(ctx, local, *caseAst.then, expected);
						return Expr::Match::Case{local, ctx.alloc(then)};
					});
				return CheckedExpr{Expr{range, Expr::Match{ctx.alloc(matchedAndType.expr), matchedUnion, cases, expected.inferred()}}};
			}
		}
	}

	const Arr<const Message> getMessages(const StructInst* ifaceInst) {
		//TODO: what to do if asIface() fails?
		return ifaceInst->body().asIface().messages;
	}

	const CheckedExpr checkMessageSend(ExprCtx& ctx, const SourceRange range, const MessageSendAst ast, Expected& expected) {
		const ExprAndType targetAndType = checkAndInfer(ctx, *ast.target);
		const Expr* target = ctx.alloc(targetAndType.expr);
		const Type targetType = targetAndType.type;

		return targetType.match(
			[&](const Type::Bogus) {
				return expected.bogus(range);
			},
			[&](const TypeParam*) {
				return todo<CheckedExpr>("checkMessageSend on type parameter -- report a diagnostic");
			},
			[&](const StructInst* instIface) {
				const Opt<const Message*> opMessage = findPtr(
					getMessages(instIface),
					[&](const Message* m) {
						return strEq(m->sig.name, ast.messageName);
					});
				if (!opMessage.has()) todo<void>("checkMessageSend");
				const Message* message = opMessage.force();
				const Sig& messageSig = message->sig;
				if (ast.args.size != messageSig.arity())
					todo<void>("checkMessageSend");
				const Arr<const Expr> args = mapZip<const Expr>{}(ctx.arena(), ast.args, messageSig.params, [&](const ExprAst argAst, const Param& param) {
					return checkAndExpect(ctx, argAst, instantiateType(ctx.arena(), param.type, instIface));
				});
				const Type returnType = instantiateType(ctx.arena(), messageSig.returnType, instIface);
				return expected.check(ctx, returnType, Expr{range, Expr::MessageSend{target, instIface, message->index, args}});
			});
	}

	const CheckedExpr checkNewActor(ExprCtx& ctx, const SourceRange range, const NewActorAst ast, Expected& expected) {
		const Opt<const Type> t = expected.tryGetDeeplyInstantiatedType(ctx.arena());
		if (!t.has() || !t.force().isStructInst())
			todo<void>("checkNewActor");
		const StructInst* instIface = t.force().asStructInst();
		const Arr<const Expr::NewIfaceImpl::Field> fields = mapWithIndex<const Expr::NewIfaceImpl::Field>{}(
			ctx.arena(),
			ast.fields,
			[&](const NewActorAst::Field field, const size_t index) {
				const ExprAndType et = checkAndInfer(ctx, *field.expr);
				return Expr::NewIfaceImpl::Field{field.isMutable, ctx.copyStr(field.name), ctx.alloc(et.expr), et.type, index};
			});
		const Arr<const Expr> messageImpls = mapZipPtrs<const Expr>{}(
			ctx.arena(),
			getMessages(instIface),
			ast.messages,
			[&](const Message* message, const NewActorAst::MessageImpl* impl) {
				if (!strEq(message->sig.name, impl->name))
					todo<void>("checkNewActor");
				zip(message->sig.params, impl->paramNames, [&](const Param param, const NameAndRange paramName) {
					if (!strEq(param.name, paramName.name))
						todo<void>("checkNewActor -- param names don't match");
				});
				return withMessageImpl(ctx, NewAndMessageInfo{message->sig.params, fields, message}, [&](ExprCtx& newCtx) {
					// message->sig.returnType is already wrapped in `fut`
					return checkAndExpect(newCtx, impl->body, message->sig.returnType);
				});
			});
		return expected.check(ctx, Type{instIface}, Expr{range, Expr::NewIfaceImpl{instIface, fields, messageImpls}});
	}

	const CheckedExpr checkSeq(ExprCtx& ctx, const SourceRange range, const SeqAst ast, Expected& expected) {
		const Expr* first = ctx.alloc(checkAndExpect(ctx, ast.first, ctx.commonTypes._void));
		const Expr* then = ctx.alloc(checkExpr(ctx, ast.then, expected));
		return CheckedExpr{Expr{range, Expr::Seq{first, then}}};
	}

	const CheckedExpr checkStructFieldSet(ExprCtx& ctx, const SourceRange range, const StructFieldSetAst ast, Expected& expected) {
		const ExprAndType target = checkAndInfer(ctx, *ast.target);
		const Opt<const StructAndField> opStructAndField = tryGetStructField(target.type, ast.fieldName);
		if (opStructAndField.has()) {
			const StructAndField structAndField = opStructAndField.force();
			const StructInst* structInst = structAndField.structInst;
			const StructField* field = structAndField.field;
			if (!field->isMutable) {
				ctx.addDiag(range, Diag{Diag::WriteToNonMutableField{field}});
				return expected.bogusWithType(range, Type{ctx.commonTypes._void});
			} else {
				const Expr value = checkAndExpect(ctx, ast.value, field->type);
				const Expr::StructFieldSet sfs = Expr::StructFieldSet{ctx.alloc(target.expr), structInst, field, ctx.alloc(value)};
				return expected.check(ctx, Type{ctx.commonTypes._void}, Expr{range, sfs});
			}
		} else {
			ctx.addDiag(range, Diag{Diag::WriteToNonExistentField{target.type, ast.fieldName}});
			return expected.bogusWithType(range, Type{ctx.commonTypes._void});
		}
	}

	const CheckedExpr checkThen(ExprCtx& ctx, const SourceRange range, const ThenAst ast, Expected& expected) {
		const ExprAst lambda = ExprAst{range, ExprAstKind{LambdaAst{arrLiteral<const LambdaAst::Param>(ctx.arena(), ast.left), ast.then}}};
		const CallAst call = CallAst{
			strLiteral("then"),
			emptyArr<const TypeAst>(),
			arrLiteral<const ExprAst>(ctx.arena(), *ast.futExpr, lambda)};
		return checkCall(ctx, range, call, expected);
	}

	const CheckedExpr checkExprWorker(ExprCtx& ctx, const ExprAst ast, Expected& expected) {
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
	CheckCtx& checkCtx,
	const ExprAst ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const FunsMap funsMap,
	const FunDecl* fun,
	const CommonTypes& commonTypes
) {
	ExprCtx exprCtx {checkCtx, structsAndAliasesMap, funsMap, commonTypes, fun, none<const NewAndMessageInfo>()};
	return exprCtx.alloc(checkAndExpect(exprCtx, ast, fun->returnType()));
}

const Expr checkExpr(ExprCtx& ctx, const ExprAst ast, Expected& expected) {
	return checkExprWorker(ctx, ast, expected).expr;
}
