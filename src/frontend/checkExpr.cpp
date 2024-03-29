#include "./checkExpr.h"

#include "../util/arrUtil.h"
#include "./checkCall.h"
#include "./inferringType.h"
#include "./typeFromAst.h"

namespace {
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

	const CheckedExpr checkCreateArr(
		ExprCtx* ctx,
		const SourceRange range,
		const CreateArrAst ast, Expected* expected
	) {
		const Opt<const ArrExpectedType> opAet = [&]() {
			if (has(ast.elementType)) {
				const Type ta = typeFromAst(ctx, force(ast.elementType));
				const StructInst* arrType = instantiateStructNeverDelay(
					ctx->arena(),
					ctx->commonTypes->arr,
					arrLiteral<const Type>(ctx->arena(), { ta }));
				return some<const ArrExpectedType>(ArrExpectedType{False, arrType, ta});
			} else {
				const Opt<const Type> opT = tryGetDeeplyInstantiatedType(ctx->arena(), expected);
				if (has(opT)) {
					const Type t = force(opT);
					if (t.isStructInst()) {
						const StructInst* si = t.asStructInst();
						if (ptrEquals(si->decl, ctx->commonTypes->arr))
							return some<const ArrExpectedType>(
								ArrExpectedType{True, si, only<const Type>(si->typeArgs)});
					}
				}
				ctx->addDiag(range, Diag{Diag::CreateArrNoExpectedType{}});
				return none<const ArrExpectedType>();
			}
		}();

		if (has(opAet)) {
			const ArrExpectedType aet = force(opAet);
			const Arr<const Expr> args = map<const Expr>{}(ctx->arena(), ast.args, [&](const ExprAst it) {
				return checkAndExpect(ctx, it, aet.elementType);
			});
			const Expr expr = Expr{range, Expr::CreateArr{aet.arrType, args}};
			return aet.isFromExpected
				? CheckedExpr{expr}
				: check(ctx, expected, Type{aet.arrType}, expr);
		} else
			return bogusWithoutChangingExpected(expected, range);
	}

	struct RecordAndIsBuiltinByVal {
		const StructBody::Record record;
		// True if this is the 'by-val' type. (Not if it's another type that happens to be by-val.)
		const Bool isBuiltinByVal;
	};

	template <typename CbCheckFields>
	const CheckedExpr checkCreateRecordCommon(
		ExprCtx* ctx,
		const SourceRange range,
		const Opt<const TypeAst> type,
		Expected* expected,
		CbCheckFields cbCheckFields
	) {
		Cell<const Bool> cellTypeIsFromExpected { False };
		const Opt<const Type> opT = [&]() {
			if (has(type))
				return some<const Type>(typeFromAst(ctx, force(type)));
			else {
				cellSet<const Bool>(&cellTypeIsFromExpected, True);
				const Opt<const Type> opT = tryGetDeeplyInstantiatedType(ctx->arena(), expected);
				if (!has(opT))
					ctx->addDiag(range, Diag{Diag::CantCreateRecordWithoutExpectedType{}});
				return opT;
			}
		}();
		if (!has(opT))
			return bogusWithoutChangingExpected(expected, range);
		const Type t = force(opT);
		if (!t.isStructInst()) {
			if (!t.isBogus())
				ctx->addDiag(range, Diag{Diag::CantCreateNonRecordType{t}});
			return bogusWithoutChangingExpected(expected, range);
		}

		const Bool typeIsFromExpected = cellGet(&cellTypeIsFromExpected);
		const StructInst* si = t.asStructInst();
		const StructDecl* decl = si->decl;

		const Opt<const RecordAndIsBuiltinByVal> opRecord = si->body().match(
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
			[&](const StructBody::Record r) {
				return some<const RecordAndIsBuiltinByVal>(RecordAndIsBuiltinByVal{r, False});
			},
			[](const StructBody::Union) {
				return none<const RecordAndIsBuiltinByVal>();
			});

		if (!has(opRecord) && !decl->body().isBogus())
			ctx->addDiag(range, Diag{Diag::CantCreateNonRecordType{t}});

		if (has(opRecord)) {
			const RecordAndIsBuiltinByVal record = force(opRecord);
			const Arr<const RecordField> fields = record.record.fields;
			const Opt<const Arr<const Expr>> args = cbCheckFields(decl, fields);
			if (has(args)) {
				const Expr expr = Expr{range, Expr::CreateRecord{si, force(args)}};

				if (ctx->outermostFun->noCtx() && !record.isBuiltinByVal) {
					const Opt<const ForcedByValOrRef> forcedByValOrRef = record.record.forcedByValOrRef;
					const Bool isAlwaysByVal = _or(
						isEmpty(fields),
						(has(forcedByValOrRef) && force(forcedByValOrRef) == ForcedByValOrRef::byVal));
					if (!isAlwaysByVal)
						ctx->addDiag(range, Diag{Diag::CreateRecordByRefNoCtx{decl}});
				}
				return typeIsFromExpected ? CheckedExpr{expr} : check(ctx, expected, Type(si), expr);
			} else
				return bogusWithoutChangingExpected(expected, range);
		} else
			return bogusWithoutChangingExpected(expected, range);
	}

	const CheckedExpr checkCreateRecord(
		ExprCtx* ctx,
		const SourceRange range,
		const CreateRecordAst ast,
		Expected* expected
	) {
		return checkCreateRecordCommon(
			ctx,
			range,
			ast.type,
			expected,
			[&](const StructDecl* decl, const Arr<const RecordField> fields) {
				if (!sizeEq(ast.args, fields)) {
					ctx->addDiag(range, Diag{Diag::WrongNumberNewStructArgs{decl, size(fields), size(ast.args)}});
					return none<const Arr<const Expr>>();
				} else {
					return some<const Arr<const Expr>>(mapZip<const Expr>{}(
						ctx->arena(),
						fields,
						ast.args,
						[&](const RecordField field, const ExprAst arg) {
							return checkAndExpect(ctx, arg, field.type);
						}));
				}
			});
	}

	const CheckedExpr checkCreateRecordMultiLine(
		ExprCtx* ctx,
		const SourceRange range,
		const CreateRecordMultiLineAst ast,
		Expected* expected
	) {
		return checkCreateRecordCommon(
			ctx,
			range,
			ast.type,
			expected,
			[&](const StructDecl* decl, const Arr<const RecordField> fields) {
				const Opt<const Arr<const Expr>> res = sizeEq(ast.lines, fields)
					? mapZipOrNone<const Expr>{}(
						ctx->arena(),
						fields,
						ast.lines,
						[&](const RecordField field, const CreateRecordMultiLineAst::Line line) {
							return symEq(line.name.name, field.name)
								? some<const Expr>(checkAndExpect(ctx, line.value, field.type))
								: none<const Expr>();
						})
					: none<const Arr<const Expr>>();
				if (!has(res)) {
					const Arr<const Sym> names = map<const Sym>{}(
						ctx->arena(),
						ast.lines,
						[](const CreateRecordMultiLineAst::Line line) {
							return line.name.name;
						});
					ctx->addDiag(range, Diag{Diag::CreateRecordMultiLineWrongFields{decl, fields, names}});
				}
				return res;
			});
	}

	struct ExpectedLambdaType {
		const StructDecl* funStruct;
		const FunKind kind;
		const Arr<const Type> paramTypes;
		const Type nonInstantiatedPossiblyFutReturnType;
	};

	const Opt<const ExpectedLambdaType> getExpectedLambdaType(
		ExprCtx* ctx,
		const SourceRange range,
		Expected* expected
	) {
		const Opt<const Type> expectedType = shallowInstantiateType(expected);
		if (!has(expectedType) || !force(expectedType).isStructInst()) {
			ctx->addDiag(range, Diag{Diag::ExpectedTypeIsNotALambda{expectedType}});
			return none<const ExpectedLambdaType>();
		}
		const StructInst* expectedStructInst = force(expectedType).asStructInst();
		const StructDecl* funStruct = expectedStructInst->decl;

		const Opt<const FunKind> opKind = ctx->commonTypes->getFunStructInfo(funStruct);
		if (!has(opKind)) {
			ctx->addDiag(range, Diag{Diag::ExpectedTypeIsNotALambda{expectedType}});
			return none<const ExpectedLambdaType>();
		} else {
			const FunKind kind = force(opKind);
			const Type nonInstantiatedNonFutReturnType = first(expectedStructInst->typeArgs);
			const Arr<const Type> nonInstantiatedParamTypes = tail(expectedStructInst->typeArgs);
			const Opt<const Arr<const Type>> paramTypes = mapOrNone<const Type>{}(
				ctx->arena(),
				nonInstantiatedParamTypes,
				[&](const Type it) {
					return tryGetDeeplyInstantiatedTypeFor(ctx->arena(), expected, it);
				});
			if (has(paramTypes)) {
				const Type nonInstantiatedReturnType = kind == FunKind::ref
					? ctx->makeFutType(nonInstantiatedNonFutReturnType)
					: nonInstantiatedNonFutReturnType;
				return some<const ExpectedLambdaType>(
					ExpectedLambdaType{funStruct, kind, force(paramTypes), nonInstantiatedReturnType});
			} else {
				ctx->addDiag(range, Diag{Diag::LambdaCantInferParamTypes{}});
				return none<const ExpectedLambdaType>();
			}
		}
	}

	const Diag callNoMatchFromExpectedLambdaType(
		const Sym funName,
		const ExpectedLambdaType et,
		const size_t nTypeArgs,
		const Arr<const CalledDecl> candidates
	) {
		return Diag{Diag::CallNoMatch{
			funName,
			some<const Type>(et.nonInstantiatedPossiblyFutReturnType),
			nTypeArgs,
			size(et.paramTypes),
			et.paramTypes,
			candidates}};
	}

	const Opt<const FunDecl*> getOnlyFunInScope(
		ExprCtx* ctx,
		const SourceRange range,
		const Sym funName,
		const ExpectedLambdaType et,
		const size_t nTypeArgs
	) {
		const Arr<const FunDecl*> funsInScope = [&]() {
			ArrBuilder<const FunDecl*> funsInScopeBuilder {};
			eachFunInScope(ctx, funName, [&](const CalledDecl called) {
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

		//TODO: overload resolution. For now, requires that only 1 fun exists.
		if (size(funsInScope) != 1) {
			ctx->addDiag(range, size(funsInScope) == 0
				? callNoMatchFromExpectedLambdaType(funName, et, nTypeArgs, emptyArr<const CalledDecl>())
				: Diag{Diag::FunAsLambdaCantOverload{funName}});
			return none<const FunDecl*>();
		} else
			return some<const FunDecl*>(only(funsInScope));
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

		const Opt<const FunDecl*> opFunDecl = getOnlyFunInScope(ctx, range, ast.funName, et, size(ast.typeArgs));
		if (!has(opFunDecl))
			return bogus(expected, range);
		const FunDecl* funDecl = force(opFunDecl);

		if (size(ast.typeArgs) != typeArity(funDecl)) {
			ctx->addDiag(range, Diag{
				Diag::FunAsLambdaWrongNumberTypeArgs{funDecl, size(ast.typeArgs)}});
			return bogus(expected, range);
		}

		if (!isEmpty(funDecl->specs))
			todo<void>("Provide specs for fun-as-lambda");

		const FunInst* funInst = instantiateFun(
			ctx->arena(),
			funDecl,
			typeArgsFromAsts(ctx, ast.typeArgs),
			emptyArr<const Called>());

		const Type returnType = funInst->returnType();
		const StructInst* type = instantiateStructNeverDelay(
			ctx->arena(),
			et.funStruct,
			prepend<const Type>(ctx->arena(), returnType, et.paramTypes));

		if (!typeEquals(returnType, et.nonInstantiatedPossiblyFutReturnType)) {
			ctx->addDiag(
				range,
				Diag{Diag::FunAsLambdaWrongReturnType{returnType, et.nonInstantiatedPossiblyFutReturnType}});
			return bogus(expected, range);
		}

		if (et.kind == FunKind::ptr && !funInst->noCtx())
			todo<void>("fun-as-lambda for fun-ptr must take a noctx fun");

		if (size(et.paramTypes) != arity(funInst)) {
			ctx->addDiag(range, callNoMatchFromExpectedLambdaType(
				ast.funName,
				et,
				size(ast.typeArgs),
				arrLiteral<const CalledDecl>(ctx->arena(), { CalledDecl{funDecl} })));
			return bogus(expected, range);
		}

		zip(et.paramTypes, funInst->params(), [&](const Type paramType, const Param param) {
			if (!typeEquals(paramType, param.type))
				todo<void>("checkFunAsLambda -- param types don't match");
		});

		return check(ctx, expected, Type{type}, Expr{range, Expr::FunAsLambda{funInst, type, et.kind}});
	}

	const Opt<const Expr> getIdentifierInLambda(
		const SourceRange range,
		const Sym name,
		LambdaInfo* lambda
	) {
		for (const Local* local : tempAsArr(&lambda->locals))
			if (symEq(local->name, name))
				return some<const Expr>(Expr{range, Expr::LocalRef{local}});
		for (const Param* param : ptrsRange(lambda->lambdaParams))
			if (symEq(param->name, name))
				return some<const Expr>(Expr{range, Expr::ParamRef{param}});
		// Check if we've already added something with this name to closureFields to avoid adding it twice.
		for (const ClosureField* field : tempAsArr(&lambda->closureFields))
			if (symEq(field->name, name))
				return some<const Expr>(Expr{range, Expr::ClosureFieldRef{field}});
		return none<const Expr>();
	}

	struct IdentifierAndLambdas {
		const Expr expr;
		// Lambdas outside of this identifier. Se must note those as closures.
		const Arr<LambdaInfo*> outerLambdas;
	};

	const Opt<const IdentifierAndLambdas> getIdentifierNonCall(
		const ExprCtx* ctx,
		const SourceRange range,
		const Sym name
	) {
		// Innermost lambda first
		for (const size_t i : RangeDown{mutArrSize(&ctx->lambdas)}) {
			LambdaInfo* lambda = mutArrAt(&ctx->lambdas, i);
			const Opt<const Expr> id = getIdentifierInLambda(range, name, lambda);
			if (has(id))
				return some<const IdentifierAndLambdas>(
					IdentifierAndLambdas{force(id), slice(tempAsArr(&ctx->lambdas), i + 1)});
		}

		const Arr<LambdaInfo*> allLambdas = tempAsArr(&ctx->lambdas);

		for (const Local* local : tempAsArr(&ctx->messageOrFunctionLocals))
			if (symEq(local->name, name))
				return some<const IdentifierAndLambdas>(
					IdentifierAndLambdas{Expr{range, Expr::LocalRef{local}}, allLambdas});

		for (const Param* param : ptrsRange(ctx->outermostFun->params()))
			if (symEq(param->name, name))
				return some<const IdentifierAndLambdas>(
					IdentifierAndLambdas{Expr{range, Expr::ParamRef{param}}, allLambdas});

		return none<const IdentifierAndLambdas>();
	}

	const Bool nameIsParameterOrLocalInScope(const ExprCtx* ctx, const Sym name) {
		return has(getIdentifierNonCall(ctx, SourceRange::empty(), name));
	}

	const CheckedExpr checkRef(
		ExprCtx* ctx,
		const Expr expr,
		const Sym name,
		const Arr<LambdaInfo*> passedLambdas,
		Expected* expected
	) {
		const Type type = expr.getType(ctx->arena(), ctx->commonTypes);
		if (isEmpty(passedLambdas))
			return check(ctx, expected, type, expr);
		else {
			// First of passedLambdas is the outermost one where we found the param/local.
			// This one can access it directly.
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
				Expr{expr.range(), Expr::ClosureFieldRef{field}},
				name,
				tail(passedLambdas),
				expected);
		}
	}

	const CheckedExpr checkIdentifier(
		ExprCtx* ctx,
		const SourceRange range,
		const IdentifierAst ast,
		Expected* expected
	) {
		const Sym name = ast.name;
		const Opt<const IdentifierAndLambdas> opIdentifier = getIdentifierNonCall(ctx, range, name);
		if (has(opIdentifier))
			return checkRef(
				ctx,
				force(opIdentifier).expr,
				name,
				force(opIdentifier).outerLambdas,
				expected);
		else
			return checkIdentifierCall(ctx, range, name, expected);
	}

	const CheckedExpr checkLiteralInner(
		ExprCtx* ctx,
		const SourceRange range,
		const LiteralInnerAst ast,
		Expected* expected
	) {
		return check(
			ctx,
			expected,
			Type{ctx->commonTypes->str},
			Expr{range, Expr::StringLiteral{copyStr(ctx->arena(), ast.literal)}});
	}

	const CheckedExpr checkLiteral(ExprCtx* ctx, const SourceRange range, const LiteralAst ast, Expected* expected) {
		const LiteralInnerAst inner = LiteralInnerAst{ast.kind, ast.literal};
		if (isExpectingString(expected, ctx->commonTypes->str) ||
			(!hasExpected(expected) && ast.kind == LiteralAst::Kind::string))
			return checkLiteralInner(ctx, range, inner, expected);
		else {
			const CallAst call = CallAst{
				shortSymAlphaLiteral("literal"),
				emptyArr<const TypeAst>(),
				arrLiteral<const ExprAst>(ctx->arena(), { ExprAst{range, ExprAstKind{inner}} })};
			return checkCall(ctx, range, call, expected);
		}
	}

	const Expr checkWithLocal(ExprCtx* ctx, const Local* local, const ExprAst ast, Expected* expected) {
		// Look for a parameter with the name
		if (nameIsParameterOrLocalInScope(ctx, local->name)) {
			ctx->addDiag(local->range, Diag{Diag::LocalShadowsPrevious{local->name}});
			return bogus(expected, ast.range).expr;
		} else {
			MutArr<const Local*>* locals = mutArrIsEmpty(&ctx->lambdas)
				? &ctx->messageOrFunctionLocals
				: &mustPeek(&ctx->lambdas)->locals;
			push(ctx->arena(), locals, local);
			const Expr res = checkExpr(ctx, ast, expected);
			const Local* popped = mustPop(locals);
			assert(ptrEquals(popped, local));
			return res;
		}
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

	const CheckedExpr checkLambda(ExprCtx* ctx, const SourceRange range, const LambdaAst ast, Expected* expected) {
		Arena* arena = ctx->arena();
		const Opt<const ExpectedLambdaType> opEt = getExpectedLambdaType(ctx, range, expected);
		if (!has(opEt))
			return bogus(expected, range);

		const ExpectedLambdaType et = force(opEt);

		if (!sizeEq(ast.params, et.paramTypes)) {
			printf("Number of params should be %zu, got %zu\n", size(et.paramTypes), size(ast.params));
			todo<void>("checkLambdaWorker -- # params is wrong");
		}

		const Arr<const Param> params = checkFunOrSendFunParamsForLambda(arena, ast.params, et.paramTypes);
		LambdaInfo info = LambdaInfo{params};
		Expected returnTypeInferrer = copyWithNewExpectedType(expected, et.nonInstantiatedPossiblyFutReturnType);

		const Expr* body = withLambda(ctx, &info, [&]() {
			// Note: checking the body of the lambda may fill in candidate type args
			// if the expected return type contains candidate's type params
			return ctx->alloc(checkExpr(ctx, *ast.body, &returnTypeInferrer));
		});

		const Arr<const ClosureField*> closureFields = freeze(&info.closureFields);
		const FunKind kind = et.kind;

		switch (kind) {
			case FunKind::ptr:
				for (const ClosureField* cf : closureFields)
					ctx->addDiag(range, Diag{Diag::LambdaForFunPtrHasClosure{cf}});
				break;
			case FunKind::plain:
				for (const ClosureField* cf : closureFields)
					if (cf->type.worstCasePurity() == Purity::mut)
						ctx->addDiag(range, Diag{Diag::LambdaClosesOverMut{cf}});
			case FunKind::mut:
			case FunKind::ref:
				break;
			default:
				assert(0);
		}

		const Type actualPossiblyFutReturnType = inferred(&returnTypeInferrer);
		const Opt<const Type> actualNonFutReturnType = [&]() {
			if (kind == FunKind::ref)
				return actualPossiblyFutReturnType.match(
					[](const Type::Bogus) {
						return some<const Type>(Type{Type::Bogus{}});
					},
					[](const TypeParam*) {
						return none<const Type>();
					},
					[&](const StructInst* ap) {
						return ptrEquals(ap->decl, ctx->commonTypes->fut)
							? some<const Type>(only(ap->typeArgs))
							: none<const Type>();
					});
			else
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
				closureFields,
				instFunStruct,
				kind,
				actualPossiblyFutReturnType};
			return CheckedExpr{Expr{range, lambda}};
		}
	}

	const CheckedExpr checkLet(ExprCtx* ctx, const SourceRange range, const LetAst ast, Expected* expected) {
		const ExprAndType init = checkAndInfer(ctx, *ast.initializer);
		const Local* local = nu<Local>()(
			ctx->arena(),
			ast.name.range,
			ast.name.name,
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
						const Opt<const Local*> local = has(caseAst.local)
							? some<const Local*>(
								nu<Local>()(
									ctx->arena(),
									force(caseAst.local).range,
									force(caseAst.local).name,
									Type{member}))
							: none<const Local*>();
						const Expr then = isBogus(expected)
							? bogus(expected, range).expr
							: checkWithOptLocal(ctx, local, *caseAst.then, expected);
						return Expr::Match::Case{local, ctx->alloc(then)};
					});
				return CheckedExpr{Expr{
						range,
						Expr::Match{ctx->alloc(matchedAndType.expr), matchedUnion, cases, inferred(expected)}}};
			}
		}
	}

	const CheckedExpr checkSeq(ExprCtx* ctx, const SourceRange range, const SeqAst ast, Expected* expected) {
		const Expr* first = ctx->alloc(checkAndExpect(ctx, ast.first, ctx->commonTypes->_void));
		const Expr* then = ctx->alloc(checkExpr(ctx, ast.then, expected));
		return CheckedExpr{Expr{range, Expr::Seq{first, then}}};
	}

	const CheckedExpr checkRecordFieldSet(
		ExprCtx* ctx,
		const SourceRange range,
		const RecordFieldSetAst ast,
		Expected* expected
	) {
		const ExprAndType target = checkAndInfer(ctx, *ast.target);
		const Opt<const StructAndField> opStructAndField = tryGetRecordField(target.type, ast.fieldName);
		if (has(opStructAndField)) {
			const StructAndField structAndField = force(opStructAndField);
			const StructInst* structInst = structAndField.structInst;
			const RecordField* field = structAndField.field;
			if (!field->isMutable) {
				ctx->addDiag(range, Diag{Diag::WriteToNonMutableField{field}});
				return bogusWithType(expected, range, Type{ctx->commonTypes->_void});
			} else {
				const Expr value = checkAndExpect(ctx, ast.value, field->type);
				const Expr::RecordFieldSet rfs = Expr::RecordFieldSet{
					ctx->alloc(target.expr),
					structInst,
					field,
					ctx->alloc(value)};
				return check(ctx, expected, Type{ctx->commonTypes->_void}, Expr{range, rfs});
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
			[&](const CreateRecordMultiLineAst a) {
				return checkCreateRecordMultiLine(ctx, range, a, expected);
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
			[&](const LiteralInnerAst a) {
				return checkLiteralInner(ctx, range, a, expected);
			},
			[&](const MatchAst a) {
				return checkMatch(ctx, range, a, expected);
			},
			[&](const SeqAst a) {
				return checkSeq(ctx, range, a, expected);
			},
			[&](const RecordFieldSetAst a) {
				return checkRecordFieldSet(ctx, range, a, expected);
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
	ExprCtx exprCtx {checkCtx, structsAndAliasesMap, funsMap, commonTypes, fun};
	return exprCtx.alloc(checkAndExpect(&exprCtx, ast, fun->returnType()));
}

const Expr checkExpr(ExprCtx* ctx, const ExprAst ast, Expected* expected) {
	return checkExprWorker(ctx, ast, expected).expr;
}
