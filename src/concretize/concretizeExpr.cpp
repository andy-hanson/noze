#include "./concretizeExpr.h"

#include "../instantiate.h"
#include "../util/arrUtil.h"
#include "./concretizeUtil.h"
#include "./mangleName.h"

namespace {
	struct ConcretizeExprCtx {
		ConcretizeCtx& concretizeCtx;
		const ConcreteFunSource concreteFunSource;
		ConcreteFun* currentConcreteFun; // This is the ConcreteFun* for a lambda, not its containing fun

		//TODO: this should go in the ConcreteFunSource
		const Arr<const ConcreteField> fields; // If this is inside a new iface
		MutDict<const Local*, const ConcreteLocal*, comparePointer<const Local>> locals {};

		ConcretizeExprCtx(const ConcretizeExprCtx&) = delete;
		ConcretizeExprCtx(ConcretizeExprCtx&&) = default;

		Arena& arena() {
			return concretizeCtx.arena;
		}

		AllConstants& allConstants() {
			return concretizeCtx.allConstants;
		}

		const ConcreteType getConcreteType(const Type t) {
			return ::getConcreteType(concretizeCtx, t, typeScope());
		}

		const ConcreteType getConcreteType_forStructInst(const StructInst* i) {
			return ::getConcreteType_forStructInst(concretizeCtx, i, typeScope());
		}

		const Arr<const ConcreteType> typesToConcreteTypes(const Arr<const Type> typeArgs) {
			return ::typesToConcreteTypes(concretizeCtx, typeArgs, typeScope());
		}

		//TODO: just have one of these...
		const TypeArgsScope typeScope() {
			return concreteFunSource.typeArgsScope();
		}
		const TypeArgsScope typeArgsScope() const {
			return concreteFunSource.typeArgsScope();
		}

		const FunDecl* containingFunDecl() const {
			return concreteFunSource.containingFunDecl();
		}
	};

	const ConstantOrExpr concretizeExpr(ConcretizeExprCtx& ctx, const Expr expr);

	const ConcreteFunInst getConcreteFunInstFromCalled(ConcretizeExprCtx& ctx, const Called called) {
		return called.match(
			[&](const FunInst* funInst) {
				const Arr<const ConcreteFunInst> specImpls = map<const ConcreteFunInst>{}(ctx.arena(), funInst->specImpls, [&](const Called calledSpecImpl) {
					return getConcreteFunInstFromCalled(ctx, calledSpecImpl);
				});
				return ConcreteFunInst{
					FunDeclAndTypeArgs{funInst->decl, ctx.typesToConcreteTypes(funInst->typeArgs)},
					specImpls,
				};
			},
			[&](const SpecSig specSig) {
				return at(ctx.concreteFunSource.containingFunInfo.specImpls, specSig.indexOverAllSpecUses);
			});
		unused(ctx); unused(called);
		return todo<const ConcreteFunInst>("!!!!!");
		//const Arr<const FunDecl*> specImpls = map<const FunDecl*>{}(ctx.arena(), e.specImpls(), [&](const CalledDecl specImpl) {
		// return funDeclFromCalledDecl(ctx, specImpl);
		//});
	}

	struct FunAndArgs {
		const ConcreteFun* fun;
		const Arr<const ConstantOrExpr> notSpecializedArgs;
	};

	const ConstantOrExpr concretizeCall(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::Call e) {
		const Arr<const ConstantOrExpr> args = map<const ConstantOrExpr>{}(ctx.arena(), e.args, [&](const Expr arg) {
			return concretizeExpr(ctx, arg);
		});
		const FunAndArgs funAndArgs = [&]() {
			const ConcreteFunInst concreteCalled = getConcreteFunInstFromCalled(ctx, e.called);
			const Opt<const KnownLambdaBody*> opKnownLambdaBody = isEmpty(args)
				? none<const KnownLambdaBody*>()
				: getKnownLambdaBodyFromConstantOrExpr(first(args));

			// TODO: also handle isCallFunPtr
			if (isCallFun(ctx.concretizeCtx, concreteCalled.decl()) && opKnownLambdaBody.has()) {
				const KnownLambdaBody* klb = opKnownLambdaBody.force();
				assert(isEmpty(concreteCalled.specImpls));
				const Arr<const ConstantOrExpr> itsArgs = tail(args);
				const SpecializeOnArgs itsSpecializeOnArgs = getSpecializeOnArgsForFun(ctx.concretizeCtx, range, concreteCalled.decl(), itsArgs);
				const ConcreteFun* actualCalled = instantiateKnownLambdaBodyForDirectCall(ctx.concretizeCtx, klb, itsSpecializeOnArgs.specializeOnArgs);
				const Arr<const ConstantOrExpr> itsNotSpecializedArgs = itsSpecializeOnArgs.notSpecializedArgs;
				// If arg0 is a constant, completely omit it. Else pass it as the closure arg.
				const Arr<const ConstantOrExpr> allArgs = klb->hasClosure()
					? prepend<const ConstantOrExpr>(ctx.arena(), first(args), itsNotSpecializedArgs)
					: itsNotSpecializedArgs;
				return FunAndArgs{actualCalled, allArgs};
			} else {
				const SpecializeOnArgs specializeOnArgs = getSpecializeOnArgsForFun(ctx.concretizeCtx, range, concreteCalled.decl(), args);
				const ConcreteFun* fun = getConcreteFunForCallAndFillBody(ctx.concretizeCtx, concreteCalled, specializeOnArgs.specializeOnArgs);
				return FunAndArgs{fun, specializeOnArgs.notSpecializedArgs};
			}
		}();

		const ConcreteFunBody body = funAndArgs.fun->body();
		// TODO: actually, if args have no *side effects* (and do not throw exceptions) is the only condition we need to make the whole thing a constant
		if (body.isConstant() && allConstant(args))
			return ConstantOrExpr{body.asConstant()};
		else {
			const ConcreteExpr::CallConcreteFun callFun = ConcreteExpr::CallConcreteFun{funAndArgs.fun, funAndArgs.notSpecializedArgs};
			return nuExpr(ctx.arena(), funAndArgs.fun->returnType(), range, knownLambdaBodyFromConcreteFunBody(body), callFun);
		}
	}

	const ConstantOrExpr concretizeClosureFieldRef(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::ClosureFieldRef e) {
		const KnownLambdaBody* klb = ctx.concreteFunSource.knownLambdaBody.force();
		const size_t index = e.index();
		const Arr<const ClosureSingleSpecialize> specialize = klb->closureSpecialize;
		const ClosureSingleSpecialize specialized = at(specialize, index);
		if (specialized.clv.isConstant())
			return ConstantOrExpr{specialized.clv.asConstant()};
		else {
			const ConcreteField* field = specialized.field.force();
			// WRONG! klb->closureParam is by-value every time. Want the one on the currentconcretefun.
			const ConcreteParam* closureParam = &ctx.currentConcreteFun->closureParam.force();
			const ConstantOrExpr closureParamRef = nuExpr(ctx.arena(), closureParam->type, range, ConcreteExpr::ParamRef{closureParam});
			const Opt<const KnownLambdaBody*> itsKlb = getKnownLambdaBodyFromConstantOrLambdaOrVariable(specialized.clv);
			return nuExpr(ctx.arena(), field->type, range, itsKlb, ConcreteExpr::StructFieldAccess{closureParam->type.isPointer, closureParamRef, field});
		}
	}

	const Arr<const ConstantOrExpr> getArgsNonConst(ConcretizeExprCtx& ctx, const Arr<const Expr> argExprs) {
		return map<const ConstantOrExpr>{}(ctx.arena(), argExprs, [&](const Expr arg) {
			return concretizeExpr(ctx, arg);
		});
	}

	const Arr<const ConstantOrExpr> getArgsNonConstWithDynamicLambdas(ConcretizeExprCtx& ctx, const SourceRange range, const Arr<const Expr> argExprs) {
		return makeLambdasDynamic_arr(ctx.concretizeCtx, range, getArgsNonConst(ctx, argExprs));
	}

	const Result<const Arr<const Constant*>, const Arr<const ConstantOrExpr>> getArgs(ConcretizeExprCtx& ctx, const Arr<const Expr> argExprs) {
		const Arr<const ConstantOrExpr> args = getArgsNonConst(ctx, argExprs);
		if (every(args, [&](const ConstantOrExpr c) { return c.isConstant(); }))
			return success<const Arr<const Constant*>, const Arr<const ConstantOrExpr>>(
				map<const Constant*>{}(ctx.arena(), args, [](const ConstantOrExpr c) { return c.asConstant(); }));
		else
			return failure<const Arr<const Constant*>, const Arr<const ConstantOrExpr>>(args);
	};

	const ConstantOrExpr concretizeCreateRecord(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::CreateRecord e) {
		Arena& arena = ctx.arena();
		const ConcreteType type = ctx.getConcreteType_forStructInst(e.structInst);
		auto nonConstant = [&](const Arr<const ConstantOrExpr> nonConstantArgs) -> const ConstantOrExpr {
			const ConstantOrExpr value = nuExpr(arena, type.byVal(), range, ConcreteExpr::CreateRecord{makeLambdasDynamic_arr(ctx.concretizeCtx, range, nonConstantArgs)});
			return type.isPointer
				? nuExpr(
					arena,
					type,
					range,
					getKnownLambdaBodyFromConstantOrExpr(value),
					ConcreteExpr::Alloc{getAllocFun(ctx.concretizeCtx), value.asConcreteExpr()})
				: value;
		};

		if (type.strukt->isSelfMutable())
			return nonConstant(getArgsNonConst(ctx, e.args));
		else {
			const Result<const Arr<const Constant*>, const Arr<const ConstantOrExpr>> args = getArgs(ctx, e.args);
			return args.match(
				[&](const Arr<const Constant*> constantArgs) {
					return ConstantOrExpr{ctx.allConstants().record(arena, type, constantArgs)};
				},
				[&](const Arr<const ConstantOrExpr> nonConstantArgs) {
					return nonConstant(nonConstantArgs);
				});
		}
	}

	const ConstantOrExpr concretizeFunAsLambda(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::FunAsLambda e) {
		Arena& arena = ctx.arena();
		if (e.isRemoteFun)
			todo<void>("funaslambda remote");

		const ConcreteFun* cf = getOrAddNonGenericConcreteFunAndFillBody(ctx.concretizeCtx, e.fun);
		const Str mangledName = cat(arena, cf->mangledName(), strLiteral("__asLambda"));

		const ConcreteType dynamicType = ctx.getConcreteType_forStructInst(e.type);

		const KnownLambdaBody* klb = arena.nu<KnownLambdaBody>()(
			dynamicType, cf->sig, mangledName, none<const ConcreteParam>(), emptyArr<const ClosureSingleSpecialize>());
		const Arr<const Expr> args = mapPointers<const Expr>{}(arena, e.fun->params(), [&](const Param* p) {
			return Expr{range, Expr::ParamRef{p}};
		});
		const Expr* body = arena.nu<const Expr>()(
			range,
			Expr::Call{
				// TODO: this should technically use the model arena and not the concrete arena?
				Called{instantiateNonGenericFun(ctx.arena(), e.fun)},
				args});
		const LambdaInfo info = LambdaInfo{ctx.concreteFunSource.containingFunInfo, body};
		ctx.concretizeCtx.knownLambdaBodyToInfo.add(arena, klb, info);
		return ConstantOrExpr{ctx.allConstants().lambda(arena, klb)};
	}

	struct ClosureSpecialize {
		const Opt<const ConcreteParam> closureParam;
		const Arr<const ClosureSingleSpecialize> closureSpecialize;
		const Arr<const ConstantOrExpr> nonConstantArgs;
	};
	const ClosureSpecialize getClosureSpecialize(
		ConcretizeExprCtx& ctx,
		const SourceRange range,
		const Arr<const ConstantOrExpr> closureArgsWithConstants,
		const Arr<const ClosureField*> closure,
		const Str mangledName
	) {
		Arena& arena = ctx.arena();
		const SpecializeOnArgs specializeOnArgs = getSpecializeOnArgsForLambdaClosure(ctx.concretizeCtx, range, closureArgsWithConstants);

		// Only the non-constant closures should go into the closure type.
		const Arr<const ConcreteField> closureFields = concretizeClosureFieldsAndSpecialize(
			ctx.concretizeCtx,
			closure,
			specializeOnArgs.specializeOnArgs,
			ctx.typeArgsScope());

		const Str typeMangledName = cat(arena, mangledName, strLiteral("___closure"));
		const Opt<const ConcreteType> closureType = concreteTypeFromFields_neverPointer(arena, closureFields, typeMangledName);
		const Opt<const ConcreteParam> closureParam = closureType.has()
			? some<const ConcreteParam>(ConcreteParam{strLiteral("_closure"), closureType.force()})
			: none<const ConcreteParam>();

		size_t fieldI = 0;
		const Arr<const ClosureSingleSpecialize> closureSpecialize = map<const ClosureSingleSpecialize>{}(arena, specializeOnArgs.specializeOnArgs, [&](const ConstantOrLambdaOrVariable clv) {
			const Opt<const ConcreteField*> field = clv.isConstant()
				? none<const ConcreteField*>()
				: [&]() {
					const ConcreteField* res = getPtr(closureFields, fieldI);
					fieldI++;
					return some<const ConcreteField*>(res);
				}();
			return ClosureSingleSpecialize{clv, field};
		});
		assert(fieldI == closureFields.size);

		return ClosureSpecialize{closureParam, closureSpecialize, specializeOnArgs.notSpecializedArgs};

	}

	const ConstantOrExpr concretizeLambda(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::Lambda e) {
		Arena& arena = ctx.arena();
		if (e.isRemoteFun)
			todo<void>("remote fun");

		// Since we're just creating the lambda and not calling it, we can't specialize on params yet. (But we will specialize above.)
		const Arr<const ConcreteParam> nonSpecializedParams = concretizeParamsNoSpecialize(ctx.concretizeCtx, e.params, ctx.typeArgsScope());

		const Str mangledName = [&]() {
			Writer writer { arena };
			writeStr(writer, ctx.currentConcreteFun->mangledName());
			writeStatic(writer, "__lambda");
			writeNat(writer, ctx.currentConcreteFun->nextLambdaIndex++);
			return writer.finish();
		}();

		const Arr<const ConstantOrExpr> closureArgsWithConstants = map<const ConstantOrExpr>{}(arena,  e.closure, [&](const ClosureField* f) {
			return concretizeExpr(ctx, *f->expr);
		});

		const ClosureSpecialize closureSpecialize = getClosureSpecialize(ctx, range, closureArgsWithConstants, e.closure, mangledName);

		const ConcreteSig nonSpecializedSig = ConcreteSig{mangledName, ctx.getConcreteType(e.returnType), nonSpecializedParams};

		const ConcreteType dynamicType = ctx.getConcreteType_forStructInst(e.type);

		const KnownLambdaBody* klb = arena.nu<const KnownLambdaBody>()(
			dynamicType, nonSpecializedSig, mangledName, closureSpecialize.closureParam, closureSpecialize.closureSpecialize);
		const LambdaInfo info = LambdaInfo{ctx.concreteFunSource.containingFunInfo, e.body};

		ctx.concretizeCtx.knownLambdaBodyToInfo.add(arena, klb, info);

		return closureSpecialize.closureParam.has()
			? nuExpr(arena, klb->dynamicType, range, some<const KnownLambdaBody*>(klb), ConcreteExpr::Lambda{closureSpecialize.nonConstantArgs})
			: ConstantOrExpr{ctx.allConstants().lambda(arena, klb)};
	}

	const ConcreteLocal* concretizeLocal(ConcretizeExprCtx& ctx, const Local* local, const ConstantOrLambdaOrVariable clv) {
		const ConcreteType localType = ctx.getConcreteType(local->type);
		return ctx.arena().nu<const ConcreteLocal>()(mangleName(ctx.arena(), local->name), localType, clv);
	}

	const ConstantOrExpr concretizeWithLocal(ConcretizeExprCtx& ctx, const Local* modelLocal, const ConcreteLocal* concreteLocal, const Expr expr) {
		ctx.locals.add(ctx.arena(), modelLocal, concreteLocal);
		const ConstantOrExpr res = concretizeExpr(ctx, expr);
		const ConcreteLocal* cl2 = ctx.locals.mustDelete(modelLocal);
		assert(ptrEquals(cl2, concreteLocal));
		return res;
	}

	const ConstantOrExpr concretizeLet(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::Let e) {
		const ConstantOrExpr value = concretizeExpr(ctx, *e.value);
		const ConcreteLocal* local = concretizeLocal(ctx, e.local, constantOrLambdaOrVariableFromConstantOrExpr(value));
		const ConstantOrExpr then = concretizeWithLocal(ctx, e.local, local, *e.then);
		// if 'value' is a constant, no need to create a variable for that -- we'll inline the constant everywhere
		return value.match(
			[&](const Constant*) {
				return then;
			},
			[&](const ConcreteExpr* valueExpr) {
				return nuExpr(ctx.arena(), then.typeWithoutKnownLambdaBody(), range, getKnownLambdaBodyFromConstantOrExpr(then), ConcreteExpr::Let { local, valueExpr, then });
			});
	}

	const ConstantOrExpr concretizeMatch(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::Match e) {
		Arena& arena = ctx.arena();
		const ConstantOrExpr matched = concretizeExpr(ctx, *e.matched);
		const ConcreteStruct* matchedUnion = ctx.getConcreteType_forStructInst(e.matchedUnion).mustBeNonPointer();
		const ConcreteType type = ctx.getConcreteType(e.type);
		return matched.match(
			[&](const Constant* c) {
				const ConstantKind::Union u = c->kind.asUnion();
				assert(u.unionType == matchedUnion);
				const Expr::Match::Case kase = at(e.cases, u.memberIndex);
				if (kase.local.has()) {
					const ConcreteLocal* local = concretizeLocal(ctx, kase.local.force(), ConstantOrLambdaOrVariable{u.member});
					return concretizeWithLocal(ctx, kase.local.force(), local, *kase.then);
				} else
					return concretizeExpr(ctx, *kase.then);
			},
			[&](const ConcreteExpr* matchedExpr) {
				const Arr<const ConcreteExpr::Match::Case> cases = map<const ConcreteExpr::Match::Case>{}(arena, e.cases, [&](const Expr::Match::Case kase) {
					if (kase.local.has()) {
						const ConcreteLocal* local = concretizeLocal(ctx, kase.local.force(), ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}});
						// Since there are many cases, no one KnownLambdaBody can win
						const ConstantOrExpr then = makeLambdasDynamic(ctx.concretizeCtx, range, concretizeWithLocal(ctx, kase.local.force(), local, *kase.then));
						return ConcreteExpr::Match::Case{some<const ConcreteLocal*>(local), then};
					} else
						return ConcreteExpr::Match::Case{none<const ConcreteLocal*>(), concretizeExpr(ctx, *kase.then)};
				});
				return ConstantOrExpr{nuExpr(arena, type, range, ConcreteExpr::Match{matchedExpr, matchedUnion, cases})};
			});
	}

	const ConstantOrExpr concretizeNewIfaceImpl(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::NewIfaceImpl e) {
		Arena& arena = ctx.arena();
		const ConcreteStruct* iface = ctx.getConcreteType_forStructInst(e.iface).mustBeNonPointer();
		const Arr<const ConcreteField> fields = map<const ConcreteField>{}(arena, e.fields, [&](const Expr::NewIfaceImpl::Field f) {
			return ConcreteField{f.isMutable, mangleName(arena, f.name), ctx.getConcreteType(f.type)};
		});
		const Arr<const ConstantOrExpr> fieldInitializers = map<const ConstantOrExpr>{}(arena, e.fields, [&](const Expr::NewIfaceImpl::Field f) {
			return concretizeExpr(ctx, *f.expr);
		});
		const Str mangledNameBase = [&]() {
			Writer writer { arena };
			writeStr(writer, ctx.currentConcreteFun->mangledName());
			writeStatic(writer, "__ifaceImpl");
			writeNat(writer, ctx.currentConcreteFun->nextNewIfaceImplIndex++);
			return writer.finish();
		}();
		const Arr<const ConcreteExpr::NewIfaceImpl::MessageImpl> messageImpls = mapZip<const ConcreteExpr::NewIfaceImpl::MessageImpl>{}(
			arena,
			iface->body().asIface().messages,
			e.messageImpls,
			[&](const ConcreteSig sig, const Expr body) {
				ConcretizeExprCtx newCtx = todo<ConcretizeExprCtx>("concretizeexprctx for iface message impl");
				const ConstantOrExpr concreteBody = concretizeExpr(newCtx, body);
				const Str mangledName = [&]() {
					Writer writer { arena };
					writeStr(writer, mangledNameBase);
					writeStatic(writer, "__");
					writeStr(writer, sig.mangledName);
					return writer.finish();
				}();
				return ConcreteExpr::NewIfaceImpl::MessageImpl{mangledName, concreteBody};
			});
		const Opt<const ConcreteType> fieldsType = concreteTypeFromFields(arena, fields, mangledNameBase);
		const ConcreteExpr::NewIfaceImpl impl = ConcreteExpr::NewIfaceImpl{iface, fieldsType, fieldInitializers, messageImpls};
		return nuExpr(arena, ConcreteType::fromStruct(iface), range, impl);
	}

	const ConcreteParam* findCorrespondingConcreteParam(const ConcretizeExprCtx& ctx, const size_t paramIndex) {
		size_t paramI = 0;
		for (const size_t i : Range{paramIndex})
			if (!at(ctx.concreteFunSource.paramsSpecialize, i).isConstant())
				paramI++;
		return getPtr(ctx.currentConcreteFun->paramsExcludingCtxAndClosure(), paramI);
	}

	const ConstantOrExpr concretizeParamRef(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::ParamRef e) {
		const size_t paramIndex = e.param->index;
		// NOTE: we'll never see a ParamRef to a param from outside of a lambda -- that would be a ClosureFieldRef instead.
		const ConstantOrLambdaOrVariable paramSpecialize = at(ctx.concreteFunSource.paramsSpecialize, paramIndex);
		if (paramSpecialize.isConstant())
			return ConstantOrExpr{paramSpecialize.asConstant()};
		else {
			const ConcreteParam* concreteParam = findCorrespondingConcreteParam(ctx, paramIndex);
			const Opt<const KnownLambdaBody*> knownLambdaBody = getKnownLambdaBodyFromConstantOrLambdaOrVariable(paramSpecialize);
			return nuExpr(ctx.arena(), concreteParam->type, range, knownLambdaBody, ConcreteExpr::ParamRef{concreteParam});
		}
	}

	const ConcreteField* getMatchingField(const ConcreteType type, const size_t fieldIndex) {
		return getPtr(type.strukt->body().asFields().fields, fieldIndex);
	}

	const ConcreteField* getMatchingField(ConcretizeExprCtx& ctx, const StructInst* targetType, const StructField* field) {
		const ConcreteType type = ctx.getConcreteType_forStructInst(targetType);
		return getMatchingField(type, field->index);
	}

	const ConstantOrExpr concretizeStructFieldAccess(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::StructFieldAccess e) {
		Arena& arena = ctx.arena();
		const ConstantOrExpr target = concretizeExpr(ctx, *e.target);
		const ConcreteType targetType = ctx.getConcreteType_forStructInst(e.targetType);
		const size_t fieldIndex = e.field->index;
		const Str fieldName = e.field->name;
		AllConstants& allConstants = ctx.allConstants();

		const ConcreteField* field = getMatchingField(targetType, fieldIndex);
		const ConcreteType type = field->type;

		auto expr = [&]() -> ConstantOrExpr	{
			return nuExpr(arena, type, range, ConcreteExpr::StructFieldAccess{targetType.isPointer, target, field});
		};

		return target.match(
			[&](const Constant* c) {
				const ConstantKind kind = c->kind;
				if (kind.isRecord()) {
					const ConstantKind::Record r = kind.asRecord();
					assert(compareConcreteType(targetType, r.type) == Comparison::equal);
					return ConstantOrExpr{at(r.args, fieldIndex)};
				} else if (kind.isArray()) {
					const ConstantKind::Array a = kind.asArray();
					// Fields are "size", "data"
					switch (fieldIndex) {
						case 0:
							assert(strEqLiteral(fieldName, "size"));
							return ConstantOrExpr{allConstants.nat64(arena, type, a.size())};
						case 1:
							assert(strEqLiteral(fieldName, "data"));
							return ConstantOrExpr{allConstants.ptr(arena, type, c, 0)};
						default:
							assert(0);
					}
				} else if (kind.isLambda()) {
					// fun0, fun1, fun2... all have the same field names
					const KnownLambdaBody* klb = kind.asLambda().knownLambdaBody;
					switch (fieldIndex) {
						case 0: {
							assert(strEqLiteral(fieldName, "fun-ptr"));
							// instantiate the lambda now
							const ConcreteFun* cf = instantiateKnownLambdaBodyForDynamic(ctx.concretizeCtx, klb);
							return ConstantOrExpr{allConstants.funPtr(arena, type, cf)};
						}
						case 1:
							assert(strEqLiteral(fieldName, "closure"));
							if (klb->hasClosure())
								// Constant parts of closure are omitted, so this must be non-constant.
								return expr();
							else
								return ConstantOrExpr(allConstants._null(arena, type));
						default:
							assert(0);
					}
				} else
					// StructFieldAccess on a non-struct?
					return unreachable<const ConstantOrExpr>();
			},
			[&](const ConcreteExpr*) {
				return expr();
			});
	}

	// This is simpler than above because Constants are never settable
	const ConstantOrExpr concretizeStructFieldSet(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::StructFieldSet e) {
		const ConcreteExpr* target = concretizeExpr(ctx, *e.target).asConcreteExpr();
		const ConcreteType targetType = ctx.getConcreteType_forStructInst(e.targetType);
		assert(targetType.isPointer); // If we're mutating it, it should be by reference.
		const ConcreteField* field = getMatchingField(ctx, e.targetType, e.field);
		const ConstantOrExpr value = concretizeExpr(ctx, *e.value);
		const ConcreteType voidType = ctx.concretizeCtx.voidType();
		return nuExpr(ctx.arena(), voidType, range, ConcreteExpr::StructFieldSet{targetType.isPointer, target, field, value});
	}

	const ConstantOrExpr concretizeExpr(ConcretizeExprCtx& ctx, const Expr expr) {
		Arena& arena = ctx.arena();
		const SourceRange range = expr.range();

		return expr.match(
			[](const Expr::Bogus) {
				return unreachable<const ConstantOrExpr>();
			},
			[&](const Expr::Call e) {
				return concretizeCall(ctx, range, e);
			},
			[&](const Expr::ClosureFieldRef e) {
				return concretizeClosureFieldRef(ctx, range, e);
			},
			[&](const Expr::Cond e) {
				const ConstantOrExpr cond = concretizeExpr(ctx, *e.cond);
				return cond.match(
					[&](const Constant* c) {
						return concretizeExpr(ctx, *(c->kind.asBool() ? e.then : e.elze));
					},
					[&](const ConcreteExpr* condExpr) {
						const ConcreteType type = ctx.getConcreteType(e.type);
						return nuExpr(arena, type, range, ConcreteExpr::Cond{
							condExpr,
							makeLambdasDynamic(ctx.concretizeCtx, range, concretizeExpr(ctx, *e.then)),
							makeLambdasDynamic(ctx.concretizeCtx, range, concretizeExpr(ctx, *e.elze))});
					});
			},
			[&](const Expr::CreateArr e) {
				if (isEmpty(e.args))
					todo<void>("should this ever happen?");
				const Result<const Arr<const Constant*>, const Arr<const ConstantOrExpr>> args = getArgs(ctx, e.args);
				const ConcreteStruct* arrayType = ctx.getConcreteType_forStructInst(e.arrType).mustBeNonPointer();
				const ConcreteType elementType = ctx.getConcreteType(e.elementType());
				return args.match(
					[&](const Arr<const Constant*> constantArgs) {
						return ConstantOrExpr{ctx.allConstants().arr(arena, arrayType, elementType, constantArgs)};
					},
					[&](const Arr<const ConstantOrExpr> nonConstantArgs) {
						const ConcreteExpr::CreateArr ca = ConcreteExpr::CreateArr{
							arrayType,
							elementType,
							getAllocFun(ctx.concretizeCtx),
							makeLambdasDynamic_arr(ctx.concretizeCtx, range, nonConstantArgs)};
						return nuExpr(arena, ConcreteType::fromStruct(arrayType), range, ca);
					});
			},
			[&](const Expr::CreateRecord e) {
				return concretizeCreateRecord(ctx, range, e);
			},
			[&](const Expr::FunAsLambda e) {
				return concretizeFunAsLambda(ctx, range, e);
			},
			[&](const Expr::IfaceImplFieldRef) {
				// never a constant
				return todo<const ConstantOrExpr>("newfieldref");
			},
			[&](const Expr::ImplicitConvertToUnion e) {
				const ConcreteType unionType = ctx.getConcreteType_forStructInst(e.unionType);
				const ConcreteType memberType = ctx.getConcreteType_forStructInst(e.memberType);
				return nuExpr(arena, unionType, range, ConcreteExpr::ImplicitConvertToUnion{
					memberType,
					makeLambdasDynamic(ctx.concretizeCtx, range, concretizeExpr(ctx, *e.inner))});
			},
			[&](const Expr::Lambda e) {
				return concretizeLambda(ctx, range, e);
			},
			[&](const Expr::Let e) {
				return concretizeLet(ctx, range, e);
			},
			[&](const Expr::LocalRef e) {
				const ConcreteLocal* let = ctx.locals.mustGet(e.local);
				const ConstantOrLambdaOrVariable value = let->constantOrLambdaOrVariable;
				if (value.isConstant())
					return ConstantOrExpr{value.asConstant()};
				else
					return nuExpr(arena, let->type, range, getKnownLambdaBodyFromConstantOrLambdaOrVariable(value), ConcreteExpr::LocalRef{let});
			},
			[&](const Expr::Match e) {
				return concretizeMatch(ctx, range, e);
			},
			[&](const Expr::MessageSend e) {
				// never a constant
				const ConstantOrExpr target = concretizeExpr(ctx, *e.target);
				const ConcreteStruct* iface = ctx.getConcreteType_forStructInst(e.iface).mustBeNonPointer();
				const ConcreteSig* message = getPtr(iface->body().asIface().messages, e.messageIndex);
				const Arr<const ConstantOrExpr> args = getArgsNonConstWithDynamicLambdas(ctx, range, e.args);
				const ConcreteType type = ctx.getConcreteType(e.getType());
				return nuExpr(arena, type, range, ConcreteExpr::MessageSend{target, message, args});
			},
			[&](const Expr::NewIfaceImpl e) {
				return concretizeNewIfaceImpl(ctx, range, e);
			},
			[&](const Expr::ParamRef e) {
				return concretizeParamRef(ctx, range, e);
			},
			[&](const Expr::Seq e) {
				const ConstantOrExpr first = concretizeExpr(ctx, *e.first);
				const ConstantOrExpr then = concretizeExpr(ctx, *e.then);
				return first.match(
					[&](const Constant*) {
						// If first is a constant, skip it
						return then;
					},
					[&](const ConcreteExpr* e) {
						return nuExpr(arena, then.typeWithoutKnownLambdaBody(), range, getKnownLambdaBodyFromConstantOrExpr(then), ConcreteExpr::Seq{e, then});
					});
			},
			[&](const Expr::StringLiteral e) {
				const ConcreteType charType = ctx.concretizeCtx.charType();
				const Arr<const Constant*> chars = map<const Constant*>{}(arena, e.literal, [&](const char c) {
					return ctx.allConstants()._char(arena, charType, c);
				});
				return ConstantOrExpr{ctx.allConstants().arr(
					arena,
					ctx.getConcreteType_forStructInst(ctx.concretizeCtx.commonTypes.str).mustBeNonPointer(),
					ctx.getConcreteType_forStructInst(ctx.concretizeCtx.commonTypes._char),
					chars)};
			},
			[&](const Expr::StructFieldAccess e) {
				return concretizeStructFieldAccess(ctx, range, e);
			},
			[&](const Expr::StructFieldSet e) {
				return concretizeStructFieldSet(ctx, range, e);
			});
	}
}

const ConstantOrExpr doConcretizeExpr(
	ConcretizeCtx& ctx,
	const ConcreteFunSource source,
	ConcreteFun* cf,
	const Expr e
) {
	ConcretizeExprCtx exprCtx {ctx, source, cf, emptyArr<const ConcreteField>()};
	return concretizeExpr(exprCtx, e);
}
