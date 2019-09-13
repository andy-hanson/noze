#include "./concretizeExpr.h"

#include "../instantiate.h"
#include "../util/arrUtil.h"
#include "./concretizeUtil.h"
#include "./mangleName.h"
#include "./specialize.h"

namespace {
	struct ConcretizeExprCtx {
		ConcretizeCtx* concretizeCtx;
		const ConcreteFunSource concreteFunSource;
		ConcreteFun* currentConcreteFun; // This is the ConcreteFun* for a lambda, not its containing fun

		// Note: this dict contains only the locals that are currently in scope.
		MutDict<const Local*, const ConcreteLocal*, comparePtr<const Local>> locals {};
		// Contains *all* locals
		MutArr<const ConcreteLocal*> allLocalsInThisFun {};

		ConcretizeExprCtx(const ConcretizeExprCtx*) = delete;
		ConcretizeExprCtx(ConcretizeExprCtx&&) = default;

		AllConstants* allConstants() {
			return concretizeCtx->allConstants;
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

	Arena* getArena(ConcretizeExprCtx* ctx) {
		return ctx->concretizeCtx->arena;
	}

	const ConstantOrExpr concretizeExpr(ConcretizeExprCtx* ctx, const Expr expr);

	const ConcreteFunInst getConcreteFunInstFromCalled(ConcretizeExprCtx* ctx, const Called called) {
		return called.match(
			[&](const FunInst* funInst) {
				const Arr<const ConcreteFunInst> specImpls = map<const ConcreteFunInst>{}(
					getArena(ctx),
					funInst->specImpls,
					[&](const Called calledSpecImpl) {
						return getConcreteFunInstFromCalled(ctx, calledSpecImpl);
					});
				return ConcreteFunInst{
					funInst->decl,
					ctx->typesToConcreteTypes(funInst->typeArgs),
					specImpls};
			},
			[&](const SpecSig specSig) {
				return at(ctx->concreteFunSource.specImpls(), specSig.indexOverAllSpecUses);
			});
	}

	struct FunAndArgs {
		const ConcreteFun* fun;
		// Arguments that haven't been skipped as constants.
		// (NOte: sometimes we choose not to specialize on a constant, so that would still show up here)
		const Arr<const ConstantOrExpr> nonOmittedArgs;
	};

	const ConstantOrExpr concretizeCall(ConcretizeExprCtx* ctx, const SourceRange range, const Expr::Call e) {
		const ConcreteFunInst concreteCalled = getConcreteFunInstFromCalled(ctx, e.called);

		const auto concretizeArg = [&](const size_t index) -> const ConstantOrExpr {
			return concretizeExpr(ctx, at(e.args, index));
		};

		const Opt<const ConstantOrExpr> arg0 = isEmpty(e.args)
			? none<const ConstantOrExpr>()
			: some<const ConstantOrExpr>(concretizeArg(0));

		if (contains<const FunDecl*, ptrEquals<const FunDecl>>(ctx->concretizeCtx->ifFuns, concreteCalled.decl)
			&& force(arg0).isConstant()) {
			const Bool condition = force(arg0).asConstant()->kind.asBool();
			if (size(e.args) == 2)
				return condition
					? concretizeArg(1)
					: ConstantOrExpr{constantVoid(getArena(ctx), ctx->allConstants(), ctx->concretizeCtx->voidType())};
			else if (size(e.args) == 3)
				return condition ? concretizeArg(1) : concretizeArg(2);
			else
				return unreachable<const ConstantOrExpr>();
		} else {
			const Arr<const ConstantOrExpr> args = fillArr<const ConstantOrExpr>{}(
				getArena(ctx),
				size(e.args),
				[&](const size_t i) {
					return i == 0 ? force(arg0) : concretizeArg(i);
				});

			const FunAndArgs funAndArgs = [&]() {
				const Opt<const KnownLambdaBody*> opKnownLambdaBody = isEmpty(args)
					? none<const KnownLambdaBody*>()
					: getKnownLambdaBodyFromConstantOrExpr(first(args));

				// TODO: also handle isCallFunPtr
				if (isCallFun(ctx->concretizeCtx, concreteCalled.decl) && has(opKnownLambdaBody)) {
					const KnownLambdaBody* klb = force(opKnownLambdaBody);
					assert(isEmpty(concreteCalled.specImpls));
					const Arr<const ConstantOrExpr> itsArgs = tail(args);
					const SpecializeOnArgs itsSpecializeOnArgs = getSpecializeOnArgsForFun(
						ctx->concretizeCtx,
						range,
						concreteCalled.decl,
						itsArgs);
					const ConcreteFun* actualCalled = instantiateKnownLambdaBodyForDirectCall(
						ctx->concretizeCtx,
						klb,
						itsSpecializeOnArgs.specializeOnArgs);
					const Arr<const ConstantOrExpr> itsNonOmittedArgs = itsSpecializeOnArgs.nonOmittedArgs;
					// If arg0 is a constant, completely omit it. Else pass it as the closure arg.
					const Arr<const ConstantOrExpr> allArgs = klb->hasClosure()
						? prepend<const ConstantOrExpr>(getArena(ctx), first(args), itsNonOmittedArgs)
						: itsNonOmittedArgs;
					return FunAndArgs{actualCalled, allArgs};
				} else {
					const SpecializeOnArgs specializeOnArgs = getSpecializeOnArgsForFun(
						ctx->concretizeCtx,
						range,
						concreteCalled.decl,
						args);
					const ConcreteFun* fun = getConcreteFunForCallAndFillBody(
						ctx->concretizeCtx,
						concreteCalled,
						specializeOnArgs.specializeOnArgs);
					return FunAndArgs{fun, specializeOnArgs.nonOmittedArgs};
				}
			}();

			const ConcreteFunBody body = funAndArgs.fun->body();
			// TODO: actually, if args have no *side effects* (and do not throw exceptions)
			// is the only condition we need to make the whole thing a constant
			if (body.isConstant() && allConstant(args))
				return ConstantOrExpr{body.asConstant()};
			else {
				const ConcreteExpr::Call callFun = ConcreteExpr::Call{funAndArgs.fun, funAndArgs.nonOmittedArgs};
				return nuExpr(
					getArena(ctx),
					funAndArgs.fun->returnType(),
					range,
					knownLambdaBodyFromConcreteFunBody(body), callFun);
			}
		}
	}

	const ConstantOrExpr concretizeClosureFieldRef(
		ConcretizeExprCtx* ctx,
		const SourceRange range,
		const Expr::ClosureFieldRef e
	) {
		const KnownLambdaBody* klb = force(ctx->concreteFunSource.knownLambdaBody);
		const size_t index = e.index();
		const Arr<const ClosureSingleSpecialize> specialize = klb->closureSpecialize;
		const ClosureSingleSpecialize specialized = at(specialize, index);
		if (specialized.clv.isConstant())
			return ConstantOrExpr{specialized.clv.asConstant()};
		else {
			const ConcreteField* field = force(specialized.field);
			const ConcreteParam* closureParam = forcePtr(&ctx->currentConcreteFun->closureParam);
			const ConstantOrExpr closureParamRef = nuExpr(
				getArena(ctx),
				closureParam->type,
				range,
				ConcreteExpr::ParamRef{closureParam});
			const Opt<const KnownLambdaBody*> itsKlb =
				getKnownLambdaBodyFromConstantOrLambdaOrVariable(specialized.clv);
			return nuExpr(
				getArena(ctx),
				field->type,
				range,
				itsKlb,
				ConcreteExpr::RecordFieldAccess{closureParam->type.isPointer, closureParamRef, field});
		}
	}

	const Arr<const ConstantOrExpr> getArgsNonConst(ConcretizeExprCtx* ctx, const Arr<const Expr> argExprs) {
		return map<const ConstantOrExpr>{}(getArena(ctx), argExprs, [&](const Expr arg) {
			return concretizeExpr(ctx, arg);
		});
	}

	const Result<const Arr<const Constant*>, const Arr<const ConstantOrExpr>> getArgs(
		ConcretizeExprCtx* ctx,
		const Arr<const Expr> argExprs
	) {
		const Arr<const ConstantOrExpr> args = getArgsNonConst(ctx, argExprs);
		if (every(args, [&](const ConstantOrExpr c) { return c.isConstant(); }))
			return success<const Arr<const Constant*>, const Arr<const ConstantOrExpr>>(
				map<const Constant*>{}(getArena(ctx), args, [](const ConstantOrExpr c) { return c.asConstant(); }));
		else
			return failure<const Arr<const Constant*>, const Arr<const ConstantOrExpr>>(args);
	};

	const ConstantOrExpr concretizeCreateRecord(
		ConcretizeExprCtx* ctx,
		const SourceRange range,
		const Expr::CreateRecord e
	) {
		Arena* arena = getArena(ctx);
		const ConcreteType type = ctx->getConcreteType_forStructInst(e.structInst);
		auto nonConstant = [&](const Arr<const ConstantOrExpr> nonConstantArgs) -> const ConstantOrExpr {
			const ConstantOrExpr value = nuExpr(
				arena,
				byVal(type),
				range,
				ConcreteExpr::CreateRecord{makeLambdasDynamic_arr(ctx->concretizeCtx, range, nonConstantArgs)});
			return type.isPointer
				? nuExpr(
					arena,
					type,
					range,
					getKnownLambdaBodyFromConstantOrExpr(value),
					ConcreteExpr::Alloc{getAllocFun(ctx->concretizeCtx), value.asConcreteExpr()})
				: value;
		};

		if (type.strukt->isSelfMutable())
			return nonConstant(getArgsNonConst(ctx, e.args));
		else {
			const Result<const Arr<const Constant*>, const Arr<const ConstantOrExpr>> args = getArgs(ctx, e.args);
			return args.match(
				[&](const Arr<const Constant*> constantArgs) {
					return ConstantOrExpr{constantRecord(arena, ctx->allConstants(), type, constantArgs)};
				},
				[&](const Arr<const ConstantOrExpr> nonConstantArgs) {
					return nonConstant(nonConstantArgs);
				});
		}
	}

	const ConstantOrExpr concretizeFunAsLambda(
		ConcretizeExprCtx* ctx,
		const SourceRange range,
		const Expr::FunAsLambda e
	) {
		Arena* arena = getArena(ctx);
		if (e.kind == FunKind::send)
			todo<void>("funaslambda sendfun");

		const ConcreteFun* cf = getOrAddNonTemplateConcreteFunAndFillBody(ctx->concretizeCtx, e.fun);
		const Str mangledName = cat(arena, cf->mangledName(), strLiteral("__asLambda"));

		const ConcreteType dynamicType = ctx->getConcreteType_forStructInst(e.type);

		const KnownLambdaBody* klb = nu<KnownLambdaBody>{}(
			arena,
			dynamicType,
			cf->sig,
			mangledName,
			none<const ConcreteParam>(),
			emptyArr<const ClosureSingleSpecialize>());
		const Arr<const Expr> args = mapPtrs<const Expr>{}(arena, e.fun->params(), [&](const Param* p) {
			return Expr{range, Expr::ParamRef{p}};
		});
		const Expr* body = nu<const Expr>{}(
			arena,
			range,
			Expr::Call{
				// TODO: this should technically use the model arena and not the concrete arena?
				Called{instantiateNonTemplateFun(getArena(ctx), e.fun)},
				args});
		const LambdaInfo info = LambdaInfo{ctx->concreteFunSource.containingFunInst, body};
		addToDict<const KnownLambdaBody*, const LambdaInfo, comparePtr<const KnownLambdaBody>>(
			arena,
			&ctx->concretizeCtx->knownLambdaBodyToInfo,
			klb,
			info);
		return ConstantOrExpr{constantLambda(arena, ctx->allConstants(), klb)};
	}

	struct ClosureSpecialize {
		const Opt<const ConcreteParam> closureParam;
		const Arr<const ClosureSingleSpecialize> closureSpecialize;
		const Arr<const ConstantOrExpr> nonConstantArgs;
	};
	const ClosureSpecialize getClosureSpecialize(
		ConcretizeExprCtx* ctx,
		const SourceRange range,
		const Arr<const ConstantOrExpr> closureArgsWithConstants,
		const Arr<const ClosureField*> closure,
		const Str mangledName
	) {
		Arena* arena = getArena(ctx);
		const SpecializeOnArgs specializeOnArgs = getSpecializeOnArgsForLambdaClosure(
			ctx->concretizeCtx,
			range,
			closureArgsWithConstants);

		// Only the non-constant closures should go into the closure type.
		const Arr<const ConcreteField> closureFields = concretizeClosureFieldsAndSpecialize(
			ctx->concretizeCtx,
			closure,
			specializeOnArgs.specializeOnArgs,
			ctx->typeArgsScope());

		const Str typeMangledName = cat(arena, mangledName, strLiteral("___closure"));
		const Opt<const ConcreteType> closureType =
			concreteTypeFromFields_neverPointer(arena, closureFields, typeMangledName);
		const Opt<const ConcreteParam> closureParam = has(closureType)
			? some<const ConcreteParam>(ConcreteParam{strLiteral("_closure"), force(closureType)})
			: none<const ConcreteParam>();

		size_t fieldI = 0;
		const Arr<const ClosureSingleSpecialize> closureSpecialize = map<const ClosureSingleSpecialize>{}(
			arena,
			specializeOnArgs.specializeOnArgs,
			[&](const ConstantOrLambdaOrVariable clv) {
				const Opt<const ConcreteField*> field = clv.isConstant()
					? none<const ConcreteField*>()
					: [&]() {
						const ConcreteField* res = ptrAt(closureFields, fieldI);
						fieldI++;
						return some<const ConcreteField*>(res);
					}();
				return ClosureSingleSpecialize{clv, field};
			});
		assert(fieldI == size(closureFields));

		return ClosureSpecialize{closureParam, closureSpecialize, specializeOnArgs.nonOmittedArgs};
	}

	const ConstantOrExpr getGetVatAndActor(ConcretizeExprCtx* ctx, const ConcreteType type, const SourceRange range) {
		return nuExpr(
			getArena(ctx),
			type,
			range,
			none<const KnownLambdaBody*>(),
			ConcreteExpr::Call{getGetVatAndActorFun(ctx->concretizeCtx), emptyArr<const ConstantOrExpr>()});
	}

	const ConstantOrExpr concretizeLambda(ConcretizeExprCtx* ctx, const SourceRange range, const Expr::Lambda e) {
		Arena* arena = getArena(ctx);

		// Since we're just creating the lambda and not calling it, we can't specialize on params yet.
		// (But we will specialize above.)
		const Arr<const ConcreteParam> nonSpecializedParams = concretizeParamsNoSpecialize(
			ctx->concretizeCtx, e.params, ctx->typeArgsScope());

		const Str mangledName = [&]() {
			Writer writer { arena };
			writeStr(&writer, ctx->currentConcreteFun->mangledName());
			writeStatic(&writer, "__lambda");
			writeNat(&writer, ctx->currentConcreteFun->nextLambdaIndex++);
			return finishWriter(&writer);
		}();

		const Arr<const ConstantOrExpr> closureArgsWithConstants = map<const ConstantOrExpr>{}(
			arena,
			e.closure,
			[&](const ClosureField* f) {
				return concretizeExpr(ctx, *f->expr);
			});

		const ClosureSpecialize closureSpecialize = getClosureSpecialize(
			ctx, range, closureArgsWithConstants, e.closure, mangledName);

		const ConcreteSig nonSpecializedSig = ConcreteSig{
			mangledName,
			ctx->getConcreteType(e.returnType),
			nonSpecializedParams};

		const ConcreteType possiblySendType = ctx->getConcreteType_forStructInst(e.type);
		// For a send-fun, this is the inner fun-mut type.
		const ConcreteType dynamicType = e.kind == FunKind::send
			? [&]() {
				const Arr<const ConcreteField> fields = possiblySendType.strukt->body().asRecord().fields;
				assert(size(fields) == 2);
				const ConcreteField funField = at(fields, 1);
				assert(strEqLiteral(funField.mangledName, "fun"));
				return funField.type;
			}()
			: possiblySendType;

		// Even for a send-fun, we always create the one KnwonLambdaBody for the non-specialized case.
		const KnownLambdaBody* klb = nu<const KnownLambdaBody>()(
			arena,
			dynamicType,
			nonSpecializedSig,
			mangledName,
			closureSpecialize.closureParam,
			closureSpecialize.closureSpecialize);
		const LambdaInfo info = LambdaInfo{ctx->concreteFunSource.containingFunInst, e.body};

		addToDict<const KnownLambdaBody*, const LambdaInfo, comparePtr<const KnownLambdaBody>>(
			arena,
			&ctx->concretizeCtx->knownLambdaBodyToInfo,
			klb,
			info);

		// If nothing was closed over, it's a constant.
		const ConstantOrExpr res = has(closureSpecialize.closureParam)
			? nuExpr(
				arena,
				dynamicType,
				range,
				some<const KnownLambdaBody*>(klb), ConcreteExpr::Lambda{closureSpecialize.nonConstantArgs})
			: ConstantOrExpr{constantLambda(arena, ctx->allConstants(), klb)};

		if (e.kind == FunKind::send) {
			const ConcreteField vatAndActorField = at(possiblySendType.strukt->body().asRecord().fields, 0);
			assert(strEqLiteral(vatAndActorField.mangledName, "vat_and_actor"));
			const ConstantOrExpr vatAndActor = getGetVatAndActor(ctx, vatAndActorField.type, range);
			return nuExpr(
				arena,
				possiblySendType,
				range,
				none<const KnownLambdaBody*>(),
				ConcreteExpr::CreateRecord{arrLiteral<const ConstantOrExpr>(
					arena,
					{ vatAndActor, makeLambdasDynamic(ctx->concretizeCtx, range, res) })});
		} else
			return res;
		}

	const Str chooseUniqueName(Arena* arena, const Str mangledName, const Arr<const ConcreteLocal*> allLocals) {
		return exists(allLocals, [&](const ConcreteLocal* l) { return strEq(l->mangledName, mangledName); })
			? chooseUniqueName(arena, cat(arena, mangledName, strLiteral("1")), allLocals)
			: mangledName;
	}

	const ConcreteLocal* makeLocalWorker(
		ConcretizeExprCtx* ctx,
		const Sym name,
		const ConcreteType type,
		const ConstantOrLambdaOrVariable clv
	) {
		const Str mangledName = chooseUniqueName(
			getArena(ctx),
			mangleName(getArena(ctx), name),
			tempAsArr(&ctx->allLocalsInThisFun));
		const ConcreteLocal* res = nu<const ConcreteLocal>{}(getArena(ctx), mangledName, type, clv);
		// If a constant, this won't be accessed through a local.
		if (!clv.isConstant())
			push(getArena(ctx), &ctx->allLocalsInThisFun, res);
		return res;
	}

	const ConcreteLocal* concretizeLocal(
		ConcretizeExprCtx* ctx,
		const Local* local,
		const ConstantOrLambdaOrVariable clv) {
		return makeLocalWorker(ctx, local->name, ctx->getConcreteType(local->type), clv);
	}

	const ConcreteLocal* getMatchedLocal(ConcretizeExprCtx* ctx, const ConcreteStruct* matchedUnion) {
		return makeLocalWorker(
			ctx,
			shortSymAlphaLiteral("matched"),
			concreteType_value(matchedUnion),
			ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}});
	}

	const ConstantOrExpr concretizeWithLocal(
		ConcretizeExprCtx* ctx,
		const Local* modelLocal,
		const ConcreteLocal* concreteLocal,
		const Expr expr
	) {
		addToDict<const Local*, const ConcreteLocal*, comparePtr<const Local>>(
			getArena(ctx),
			&ctx->locals,
			modelLocal,
			concreteLocal);
		const ConstantOrExpr res = concretizeExpr(ctx, expr);
		const ConcreteLocal* cl2 = mustDelete(&ctx->locals, modelLocal);
		assert(ptrEquals(cl2, concreteLocal));
		return res;
	}

	const ConstantOrExpr concretizeLet(ConcretizeExprCtx* ctx, const SourceRange range, const Expr::Let e) {
		const ConstantOrExpr value = concretizeExpr(ctx, *e.value);
		const ConcreteLocal* local = concretizeLocal(ctx, e.local, constantOrLambdaOrVariableFromConstantOrExpr(value));
		const ConstantOrExpr then = concretizeWithLocal(ctx, e.local, local, *e.then);
		// if 'value' is a constant, no need to create a variable for that -- we'll inline the constant everywhere
		return value.match(
			[&](const Constant*) {
				return then;
			},
			[&](const ConcreteExpr* valueExpr) {
				return nuExpr(
					getArena(ctx),
					then.typeWithoutKnownLambdaBody(),
					range,
					getKnownLambdaBodyFromConstantOrExpr(then),
					ConcreteExpr::Let{local, valueExpr, then});
			});
	}

	const ConstantOrExpr concretizeMatch(ConcretizeExprCtx* ctx, const SourceRange range, const Expr::Match e) {
		Arena* arena = getArena(ctx);
		const ConstantOrExpr matched = concretizeExpr(ctx, *e.matched);
		const ConcreteStruct* matchedUnion = ctx->getConcreteType_forStructInst(e.matchedUnion).mustBeNonPointer();
		const ConcreteType type = ctx->getConcreteType(e.type);
		return matched.match(
			[&](const Constant* c) {
				const ConstantKind::Union u = c->kind.asUnion();
				assert(u.unionType == matchedUnion);
				const Expr::Match::Case kase = at(e.cases, u.memberIndex);
				if (has(kase.local)) {
					const ConcreteLocal* local =
						concretizeLocal(ctx, force(kase.local), ConstantOrLambdaOrVariable{u.member});
					return concretizeWithLocal(ctx, force(kase.local), local, *kase.then);
				} else
					return concretizeExpr(ctx, *kase.then);
			},
			[&](const ConcreteExpr* matchedExpr) {
				const Arr<const ConcreteExpr::Match::Case> cases = map<const ConcreteExpr::Match::Case>{}(
					arena,
					e.cases,
					[&](const Expr::Match::Case kase) {
						if (has(kase.local)) {
							const ConcreteLocal* local = concretizeLocal(
								ctx,
								force(kase.local),
								ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}});
							// Since there are many cases, no one KnownLambdaBody can win
							const ConstantOrExpr then = makeLambdasDynamic(
								ctx->concretizeCtx,
								range,
								concretizeWithLocal(ctx, force(kase.local), local, *kase.then));
							return ConcreteExpr::Match::Case{some<const ConcreteLocal*>(local), then};
						} else
							return ConcreteExpr::Match::Case{
								none<const ConcreteLocal*>(),
								concretizeExpr(ctx, *kase.then)};
					});
				return ConstantOrExpr{nuExpr(
					arena,
					type,
					range,
					ConcreteExpr::Match{getMatchedLocal(ctx, matchedUnion), matchedExpr, cases})};
			});
	}

	const ConcreteParam* findCorrespondingConcreteParam(const ConcretizeExprCtx* ctx, const size_t paramIndex) {
		size_t paramI = 0;
		for (const size_t i : Range{paramIndex})
			if (!at(ctx->concreteFunSource.paramsSpecialize, i).isConstant())
				paramI++;
		return ptrAt(ctx->currentConcreteFun->paramsExcludingCtxAndClosure(), paramI);
	}

	const ConstantOrExpr concretizeParamRef(ConcretizeExprCtx* ctx, const SourceRange range, const Expr::ParamRef e) {
		const size_t paramIndex = e.param->index;
		// NOTE: we'll never see a ParamRef to a param from outside of a lambda --
		// that would be a ClosureFieldRef instead.
		const ConstantOrLambdaOrVariable paramSpecialize = at(ctx->concreteFunSource.paramsSpecialize, paramIndex);
		if (paramSpecialize.isConstant())
			return ConstantOrExpr{paramSpecialize.asConstant()};
		else {
			const ConcreteParam* concreteParam = findCorrespondingConcreteParam(ctx, paramIndex);
			const Opt<const KnownLambdaBody*> knownLambdaBody =
				getKnownLambdaBodyFromConstantOrLambdaOrVariable(paramSpecialize);
			return nuExpr(
				getArena(ctx),
				concreteParam->type,
				range,
				knownLambdaBody,
				ConcreteExpr::ParamRef{concreteParam});
		}
	}

	const ConcreteField* getMatchingField(const ConcreteType type, const size_t fieldIndex) {
		return ptrAt(type.strukt->body().asRecord().fields, fieldIndex);
	}

	const ConcreteField* getMatchingField(
		ConcretizeExprCtx* ctx,
		const StructInst* targetType,
		const RecordField* field
	) {
		const ConcreteType type = ctx->getConcreteType_forStructInst(targetType);
		return getMatchingField(type, field->index);
	}

	const ConstantOrExpr concretizeRecordFieldAccess(
		ConcretizeExprCtx* ctx,
		const SourceRange range,
		const Expr::RecordFieldAccess e
	) {
		Arena* arena = getArena(ctx);
		const ConstantOrExpr target = concretizeExpr(ctx, *e.target);
		const ConcreteType targetType = ctx->getConcreteType_forStructInst(e.targetType);
		const size_t fieldIndex = e.field->index;
		const Sym fieldName = e.field->name;
		AllConstants* allConstants = ctx->allConstants();

		const ConcreteField* field = getMatchingField(targetType, fieldIndex);
		const ConcreteType type = field->type;

		auto expr = [&]() -> ConstantOrExpr	{
			return nuExpr(arena, type, range, ConcreteExpr::RecordFieldAccess{targetType.isPointer, target, field});
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
							assert(symEq(fieldName, shortSymAlphaLiteral("size")));
							return ConstantOrExpr{constantNat64(arena, allConstants, type, Nat64{a.size()})};
						case 1:
							assert(symEq(fieldName, shortSymAlphaLiteral("data")));
							return ConstantOrExpr{constantPtr(arena, allConstants, type, c, Nat64{0})};
						default:
							assert(0);
					}
				} else if (kind.isLambda()) {
					// fun0, fun1, fun2... all have the same field names
					const KnownLambdaBody* klb = kind.asLambda().knownLambdaBody;
					switch (fieldIndex) {
						case 0: {
							assert(symEq(fieldName, shortSymAlphaLiteral("fun-ptr")));
							// instantiate the lambda now
							const ConcreteFun* cf = instantiateKnownLambdaBodyForDynamic(ctx->concretizeCtx, klb);
							return ConstantOrExpr{constantFunPtr(arena, allConstants, type, cf)};
						}
						case 1:
							assert(symEq(fieldName, shortSymAlphaLiteral("closure")));
							if (klb->hasClosure())
								// Constant parts of closure are omitted, so this must be non-constant.
								return expr();
							else
								return ConstantOrExpr(constantNull(arena, allConstants, type));
						default:
							assert(0);
					}
				} else
					// RecordFieldAccess on a non-struct?
					return unreachable<const ConstantOrExpr>();
			},
			[&](const ConcreteExpr*) {
				return expr();
			});
	}

	// This is simpler than above because Constants are never settable
	const ConstantOrExpr concretizeRecordFieldSet(
		ConcretizeExprCtx* ctx,
		const SourceRange range,
		const Expr::RecordFieldSet e
	) {
		const ConcreteExpr* target = concretizeExpr(ctx, *e.target).asConcreteExpr();
		const ConcreteType targetType = ctx->getConcreteType_forStructInst(e.targetType);
		assert(targetType.isPointer); // If we're mutating it, it should be by reference.
		const ConcreteField* field = getMatchingField(ctx, e.targetType, e.field);
		const ConstantOrExpr value = concretizeExpr(ctx, *e.value);
		const ConcreteType voidType = ctx->concretizeCtx->voidType();
		return nuExpr(
			getArena(ctx),
			voidType,
			range,
			ConcreteExpr::RecordFieldSet{targetType.isPointer, target, field, value});
	}

	const ConstantOrExpr concretizeExpr(ConcretizeExprCtx* ctx, const Expr expr) {
		Arena* arena = getArena(ctx);
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
						const ConcreteType type = ctx->getConcreteType(e.type);
						return nuExpr(arena, type, range, ConcreteExpr::Cond{
							condExpr,
							makeLambdasDynamic(ctx->concretizeCtx, range, concretizeExpr(ctx, *e.then)),
							makeLambdasDynamic(ctx->concretizeCtx, range, concretizeExpr(ctx, *e.elze))});
					});
			},
			[&](const Expr::CreateArr e) {
				const Result<const Arr<const Constant*>, const Arr<const ConstantOrExpr>> args = getArgs(ctx, e.args);
				const ConcreteStruct* arrayType = ctx->getConcreteType_forStructInst(e.arrType).mustBeNonPointer();
				const ConcreteType elementType = ctx->getConcreteType(e.elementType());
				return args.match(
					[&](const Arr<const Constant*> constantArgs) {
						return ConstantOrExpr{
							constantArr(arena, ctx->allConstants(), arrayType, elementType, constantArgs)};
					},
					[&](const Arr<const ConstantOrExpr> nonConstantArgs) {
						const ConcreteExpr::CreateArr ca = ConcreteExpr::CreateArr{
							arrayType,
							elementType,
							getAllocFun(ctx->concretizeCtx),
							makeLambdasDynamic_arr(ctx->concretizeCtx, range, nonConstantArgs)};
						return nuExpr(arena, concreteType_fromStruct(arrayType), range, ca);
					});
			},
			[&](const Expr::CreateRecord e) {
				return concretizeCreateRecord(ctx, range, e);
			},
			[&](const Expr::FunAsLambda e) {
				return concretizeFunAsLambda(ctx, range, e);
			},
			[&](const Expr::ImplicitConvertToUnion e) {
				const ConstantOrExpr inner = concretizeExpr(ctx, *e.inner);
				const ConcreteType unionType = ctx->getConcreteType_forStructInst(e.unionType);
				if (inner.isConstant())
					return ConstantOrExpr{constantUnion(
						getArena(ctx),
						ctx->allConstants(),
						unionType,
						e.memberIndex,
						inner.asConstant())};
				else
					return nuExpr(arena, unionType, range, ConcreteExpr::ImplicitConvertToUnion{
						e.memberIndex,
						makeLambdasDynamic(ctx->concretizeCtx, range, inner)});
			},
			[&](const Expr::Lambda e) {
				return concretizeLambda(ctx, range, e);
			},
			[&](const Expr::Let e) {
				return concretizeLet(ctx, range, e);
			},
			[&](const Expr::LocalRef e) {
				const ConcreteLocal* let = mustGetAt_mut(&ctx->locals, e.local);
				const ConstantOrLambdaOrVariable value = let->constantOrLambdaOrVariable;
				if (value.isConstant())
					return ConstantOrExpr{value.asConstant()};
				else
					return nuExpr(
						arena,
						let->type,
						range,
						getKnownLambdaBodyFromConstantOrLambdaOrVariable(value),
						ConcreteExpr::LocalRef{let});
			},
			[&](const Expr::Match e) {
				return concretizeMatch(ctx, range, e);
			},
			[&](const Expr::ParamRef e) {
				return concretizeParamRef(ctx, range, e);
			},
			[&](const Expr::RecordFieldAccess e) {
				return concretizeRecordFieldAccess(ctx, range, e);
			},
			[&](const Expr::RecordFieldSet e) {
				return concretizeRecordFieldSet(ctx, range, e);
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
						return nuExpr(
							arena,
							then.typeWithoutKnownLambdaBody(),
							range,
							getKnownLambdaBodyFromConstantOrExpr(then),
							ConcreteExpr::Seq{e, then});
					});
			},
			[&](const Expr::StringLiteral e) {
				const ConcreteType charType = ctx->concretizeCtx->charType();
				const Arr<const Constant*> chars = map<const Constant*>{}(arena, e.literal, [&](const char c) {
					return constantChar(arena, ctx->allConstants(), charType, c);
				});
				const CommonTypes* commonTypes = ctx->concretizeCtx->commonTypes;
				return ConstantOrExpr{constantArr(
					arena,
					ctx->allConstants(),
					ctx->getConcreteType_forStructInst(commonTypes->str).mustBeNonPointer(),
					ctx->getConcreteType_forStructInst(commonTypes->_char),
					chars)};
			});
	}
}

const ConcreteFunBody concretizeExpr(
	ConcretizeCtx* ctx,
	const ConcreteFunSource source,
	ConcreteFun* cf,
	const Expr e
) {
	ConcretizeExprCtx exprCtx {ctx, source, cf};
	const ConstantOrExpr res = concretizeExpr(&exprCtx, e);
	return res.match(
		[](const Constant* c) {
			return ConcreteFunBody{c};
		},
		[&](const ConcreteExpr* e) {
			return ConcreteFunBody{ConcreteFunExprBody{freeze(&exprCtx.allLocalsInThisFun), e}};
		});
}
