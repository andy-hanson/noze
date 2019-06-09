#include "./checkExpr.h"

#include "./typeFromAst.h"

namespace {
	struct NewAndMessageInfo {
		const Arr<const Param> instantiatedParams;
		const Arr<Expr::NewIfaceImpl::Field> fields;
		const Message* message;
	};

	struct LambdaInfo {
		const Arr<const Param> lambdaParams;
		MutArr<const Local*> locals = MutArr<const Local*>{};
		MutArr<const ClosureField*> closureFields = MutArr<const ClosureField*>{};
	};

	struct ExprContext {
		ExprContext(const ExprContext&) = delete;

		CheckCtx& checkCtx;
		const StructsAndAliasesMap structsAndAliasesMap;
		const FunsMap funsMap;
		const CommonTypes& commonTypes;
		const FunDecl* outermostFun;
		const Opt<const NewAndMessageInfo> newAndMessageInfo;

		// Locals of the function or message. Lambda locals are stored in the lambda.
		// (Note the Let stores the local and this points to that.)
		MutArr<Local*> messageOrFunctionLocals = MutArr<Local*>{};
		// Note: if inside newAndMessageInfo, these are only the lambdas *inside* of it.
		// TODO: these are pointers because MutArr currently only works on copyable values, and LambdaInfo should not be copied.
		MutArr<LambdaInfo*> lambdas = MutArr<LambdaInfo*>{};

		inline const Str copyStr(const Str s) {
			return checkCtx.copyStr(s);
		}

		inline Arena& arena() {
			return checkCtx.arena;
		}

		inline void diag(const SourceRange range, const Diag diag) {
			checkCtx.diag(range, diag);
		}

		inline Type makeFutType(const Type type) {
			return ::makeFutType(arena(), commonTypes, type);
		}

		inline const Expr* alloc(const Expr e) {
			return arena.nu<const Expr>()(e);
		}
	};

	template <typename Cb>
	inline auto withMessageImpl(ExprContext& ctx, const NewAndMessageInfo info, Cb cb) {
		// Since 'new' is not a closure, discard lambdas from outside of it.
		// Note: 'messageOrFunctionLocals' and 'lambdas' intentionally not passed down.
		ExprContext newCtx = ExprContext(
			ctx.checkCtx,
			ctx.structsAndAliasesMap,
			ctx.funsMap,
			ctx.commonTypes,
			ctx.outermostFun,
			some<const NewAndMessageInfo>(info));
		return cb(newCtx);
	}

	template <typename Cb>
	inline auto withLambda(ExprContext& ctx, const LambdaInfo* info, Cb cb) {
		ctx.lambdas.push(ctx.arena, info);
		auto res = cb();
		LambdaInfo* popped = ctx.lambdas.pop().force();
		assert(ptrEquals(popped, info));
		return res;
	}

	struct ExprAndType {
		const Expr expr;
		const Type type;
	}

	const ExprAndType checkAndInfer(ExprContext& ctx, const ExprAst ast) {
		Expected expected = Expected::infer();
		const Expr expr = checkExpr(ctx, ast, expected);
		return ExprAndType{expr, expected.inferred()};
	}

	const Expr checkAndExpect(ExprContext& ctx, const ExprAst ast, const Opt<const Type> expected) {
		Expected et = Expected{expected};
		return checkExpr(ctx, ast, et);
	}

	const Expr checkAndExpect(ExprContext& ctx, const ExprAst ast, const Type expected) {
		return checkAndExpect(ctx, ast, some<const Type>(expected));
	}

	const Expr checkAndExpect(ExprContext& ctx, const ExprAst* ast, const Type expected) {
		return checkAndExpect(ctx, *ast, expected);
	}

	const Expr checkAndExpect(ExprContext& ctx, const ExprAst* ast, const StructInst* expected) {
		return checkAndExpect(ctx, ast, Type{expected});
	}

	const CheckedExpr checkExprWorker(ExprContext& ctx, const ExprAst ast, Expected& expected);

	const Expr checkExpr(ExprContext& ctx, const ExprAst ast, Expected& expected) {
		return checkExprWorker(ctx, *ast, expected).expr;
	}

	const Expr checkExpr(ExprContext& ctx, const ExprAst* ast, Expected& expected) {
		return checkExpr(ctx, *ast, expected);
	}

	const CheckedExpr checkCond(ExprContext& ctx, const SourceRange range, const CondAst ast, Expected& expected) {
		const Expr cond = checkAndExpect(ctx, ast.cond, ctx.commonTypes._bool);
		const Expr then = checkExpr(ctx, ast.then, expected);
		const Expr elze = checkExpr(ctx, ast.elze, expected);
		return CheckedExpr(Expr{range, Expr::Cond{expected.inferred(), cond, then, elze}});
	}

	struct ArrExpectedType {
		const bool isFromExpected;
		const StructInst* arrType;
		const Type elementType;
	};

	const CheckedExpr checkCreateArr(ExprContext& ctx, const SourceRange range, const CreateArrAst ast, Expected& expected) {
		const ArrExpectedType aet = [&]() {
			if (ast.elementType.has()) {
				const Type ta = typeFromAst(ctx, ast.elementType.force());
				return ArrExpectedType{false, instantiateStructNeverDelay(ctx.arena, ctx.commonTypes.arr, arrLiteral<const Type>(ctx.arena, ta)), ta};
			} else {
				const Opt<const Type> opT = expected.tryGetDeeplyInstantiatedType(ctx.arena);
				if (opT.has()) {
					const Type t = opT.force();
					if (t.isStructInst()) {
						const StructInst* si = t.asStructInst();
						if (ptrEquals(si->decl, ctxt.commonTypes.arr))
							return ArrExpectedType{true, si, only<const Type>(si.typeArgs)};
					}
				}
				return todo<const ArrExpectedType>("can't get expected element type for new-arr");
			}
		}();

		const Arr<const Expr> args = map(ctx.arena, ast.args, [&](const ExprAst it) {
			return checkAndExpect(ctx, it, aet.elementType);
		});
		const Expr expr = Expr{range, Expr::CreateArr{aet.arrType, args}};
		return aet.isFromExpected
			? CheckedExpr{expr}
			: expected.check(ctx, Type{aet.arrType}, expr);
	}

	const CheckedExpr checkCreateRecord(ExprContext& ctx, const SourceRange range, const CreateRecordAst ast, Expected& expected) {
		bool isTypeFromExpected = false;
		const Type t = [&]() {
			if (ast.type.has())
				return typeFromAst(ctx, ast.type.force());
			else {
				typeIsFromExpected = true;
				const Opt<const Type> opT = expected.tryGetDeeplyInstantiatedType(ctx.arena);
				if (!opT.has())
					todo<void>("checkCreateRecord -- no expected type");
				retur nopT.force();
			}
		}
		if (!t.isStructInst())
			todo<void>("checkCreateRecord -- not a struct type");
		const StructInst* si = t.asStructInst();
		const StructDecl* decl = si->decl;

		const Opt<const StructBody::Fields> fields = decl->body().match(
			/*builtin*/ [&]() {
				const StructDecl* byVal = ctx.commonTypes.byVal;
				if (ptrEquals(decl, byVal)) {
					// We know this will be deeply instantiated since we did that at the beginning of this function
					const StructInst* inner = only<const Type>(si.typeArgs).asStructInst();
					if (inner.body.isFields())
						return some<const StructBody::Fields>(inner.body.asFields()):
					else
						return todo<const Opt<const StructBody::fields>>("byval of non-record, -- I guess just cantcreatenonrecordstruct");
				} else {
					ctx.diag(range, Diag{Diag::CantCreateNonRecordStruct(decl)});
					return none<const StructBody::Fields>;
				}
			}
			[&](const StructBody::Fields f) {
				return some<const StructBody::Fields>(f);
			},
			[&](__attribute__((unused)) const StructBody::Union _) {
				ctx.diag(range, Diag{Diag::CantCreateNonRecordStruct(decl)});
				return none<const StructBody::Fields>;
			},
			[&](__attribute__((unused)) const StructBody::Iface _) {
				ctx.diag(range, Diag{Diag::CantCreateNonRecordStruct(decl)});
				return none<const StructBody::Fields>;
			});

		if (opFields.has()) {
			const Arr<const StructField> fields = opFields.force().fields;
			if (ast.args.size != fields.size) {
				ctx.diag(range, Diag{Diag::WrongNumberNewStructArgs{decl, fields.size, ast.args.size}});
				return typeIsFromExpected ? bogusWithoutAffectingExpected(range) : expected.bogus(range);
			} else {
				//TODO: mapzip
				const Arr<const Expr> args = fillArr(ctx.arena, fields.size, [&](const size_t i) {
					const Type expectedType = instantiateType(ctx.arena, fields[i].type, si);
					return checkAndExpect(ctx, ast.args[i], expectedType);
				});
				const Expr expr = Expr{range, Expr::CreateRecord{si, args}};
				return typeIsFromExpected ? CheckedExpr{expr} : expected.check(ctx, Type(si), expr);
			} else
				return typeIsFromExpected ? bogusWithoutAffectingExpected(range) : expected.bogus(range);
		}
	}

	const CheckedExpr checkRef(
		ExprContext& ctx,
		const Expr expr,
		const SourceRange range,
		const Str name,
		const Type type,
		const Arr<const LambdaInfo*> passedLambdas,
		Expected& expected
	) {
		unused(ctx, expr, range);
		unused(name, type, passedLambdas, expected);
		return todo<CheckedExpr>("checkRef");
	}

	const CheckedExpr checkIdentifier(ExprContext& ctx, const SourceRange range, const IdentifierAst ast, Expected& expected) {
		unused(ctx, range, ast, expected);
		return todo<CheckedExpr>("checkidentifier");
	}

	const CheckedExpr checkNoCallLiteral(ExprContext& ctx, const SourceRange range, const Str literal, Expected& expected) {
		return expected.check(ctx, Type{ctx.commonTypes.str}, Expr{range, Expr::StringLiteral{ctx.copyStr(literal)}});
	}

	const CheckedExpr checkLiteral(ExprContext& ctx, const SourceRange range, const LiteralAst ast, Expected& expected) {
		unused(ctx, range, ast, expected);
		return todo<CheckedExpr>("!!!");
	}

	template <typename Cb>
	auto checkWithLocal_worker(ExprContext& ctx, const Local* local) {
		MutArr<Local*>& locals = ctx.lambdas.isEmpty() ? ctx.messageOrFunctionLocals : last(ctx.lambdas).locals;
		locals.push(ctx.arena, local);
		auto res = cb();
		Local* popped = locals.pop().force();
		assert(ptrEquals(popped, local));
		return res;
	}

	const Expr checkWithLocal(ExprContext& ctx, const Local* local, const ExprAst ast, Expected& expected) {
		return checkWithLocal_worker(ctx, local, [&]() { return checkExpr(ctx, ast, expected); }):
	}

	const Expr checkWithOptLocal(ExprContext ctx, Opt<const Local*> local, const ExprAst ast, Expected& expected) {
		return local.has()
			? checkWithLocal(ctx, local.force(), ast, expected)
			: checkExpr(ctx, ast, expected);
	}

	const CheckedExpr checkLet(ExprContext& ctx, const SourceRange range, const LetAst ast, Expected& expected) {
		unused(ctx, range, ast, expected);
		return todo<CheckedExpr>("!!!");
	}

	const CheckedExpr checkMatch(ExprContext& ctx, const SourceRange range, const MatchAst ast, Expected& expected) {
		unused(ctx, range, ast, expected);
		return todo<CheckedExpr>("!!!");
	}

	const CheckedExpr checkMessageSend(ExprContext& ctx, const SourceRange range, const MessageSendAst ast, Expected& expected) {
		unused(ctx, range, ast, expected);
		return todo<CheckedExpr>("!!!");
	}

	const CheckedExpr checkNewActor(ExprContext& ctx, const SourceRange range, const NewActorAst ast, Expected& expected) {
		unused(ctx, range, ast, expected);
		return todo<CheckedExpr>("!!!");
	}

	const CheckedExpr checkSeq(ExprContext& ctx, const SourceRange range, const SeqAst ast, Expected& expected) {
		const Expr* first = ctx.alloc(checkAndExpect(ctx, ast.first, ctx.commonTypes._void));
		const Expr* then = ctx.alloc(checkExpr(ctx, ast.then, expected));
		return CheckedExpr{Expr{range, Expr::Seq{first, then}}};
	}

	const CheckedExpr checkThen(ExprContext& ctx, const SourceRange range, const ThenAst ast, Expected& expected) {
		unused(ctx, range, ast, expected);
		return todo<CheckedExpr>("!!!");
	}

	CheckedExpr checkExprWorker(ExprContext& ctx, const ExprAst ast, Expected& expected) {
		const SourceRange = ast.range;
		return ast.match(
			[&](const CallAst a) { return checkCall(ctx, range, a, expected); },
			[&](const CondAst a) { return checkCond(ctx, range, a, expected); },
			[&](const CreateArrAst a) { return checkCreateArr(ctx, range, a, expected); },
			[&](const CreateRecordAst a) { return checkCreateRecord(ctx, range, a, expected); },
			[&](const FunAsLambdaAst a) { return checkFunAsLambda(ctx, range, a, expected); },
			[&](const Identifier a) { return checkIdentifier(ctx, range, a, expected); },
			[&](const LambdaAst a) { return checkLamda(ctx, range, a, expected); },
			[&](const LetAst a) { return checkLet(ctx, range, a, expected); },
			[&](const LiteralAst a) { return checkLiteral(ctx, range, a, expected): },
			[&](const MatchAst a) { return checkMatch(ctx, range, a, expected); },
			[&](const MessageSendAst a) { return checkMessageSend(ctx, range, a, expected); },
			[&](const NewActorAst a) { return checkNewActor(ctx, range, a, expected); },
			[&](const SeqAst a) { return checkSeq(ctx, range, a, expected); },
			[&](const ThenAst a) { return checkThen(ctx, range, a, expected); });
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
	ExprContext exprCtx = ExprCtx{checkCtx, structsAndAliasesMap, funsMap, commonTypes, fun, none<const NewAndMessageInfo>()};
	return checkAndExpect(exprCtx, ast, fun.returnType);
}
