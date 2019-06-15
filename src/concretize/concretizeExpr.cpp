#include "./concretizeExpr.h"

#include "../util/arrUtil.h"
#include "./concretizeUtil.h"
#include "./mangleName.h"

namespace {
	struct ConcretizeExprCtx {
		ConcretizeCtx& concretizeCtx;
		const ConcreteFunSource concreteFunSource;
		ConcreteFun* currentConcreteFun; // This is the ConcreteFun* for a lambda, not its containing fun
		const Arr<const ConcreteField> fields; // If this is inside a new iface
		const Arr<const ConcreteField> closure; // If this is inside a lambda
		MutDict<const Local*, const ConcreteLocal*, comparePointer<const Local>> locals {};

		ConcretizeExprCtx(const ConcretizeExprCtx&) = delete;
		ConcretizeExprCtx(ConcretizeExprCtx&&) = default;

		const FunDecl* getSpecImpl(const size_t index) const {
			unused(index);
			// Add this info to the ConcreteFunSource
			return todo<const FunDecl*>("getSpecImpl");
		}

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

	const ConcreteFun* getAllocFun(ConcretizeCtx& ctx) {
		return getOrAddNonGenericConcreteFunAndFillBody(ctx, ctx.allocFun);
	}

	template <typename T>
	const ConstantOrExpr nuExpr(Arena& arena, const SourceRange range, const Opt<const KnownLambdaBody*> klb, T t) {
		return ConstantOrExpr{arena.nu<const ConcreteExpr>()(range, klb, t)};
	}

	template <typename T>
	const ConstantOrExpr nuExpr(Arena& arena, const SourceRange range, T t) {
		return nuExpr(arena, range, none<const KnownLambdaBody*>(), t);
	}

	const ConstantOrExpr concretizeExpr(ConcretizeExprCtx& ctx, const Expr expr);

	struct FunAndArgs {
		const ConcreteFun* fun;
		const Arr<const ConstantOrExpr> notSpecializedArgs;
	};

	const ConstantOrExpr concretizeCall(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::Call e) {
		const Arr<const ConstantOrExpr> args = map<const ConstantOrExpr>{}(ctx.arena(), e.args, [&](const Expr arg) {
			return concretizeExpr(ctx, arg);
		});
		//TODO: handle calling specs
		const FunDecl* f = e.calledDecl().asFunDecl();

		const FunAndArgs funAndArgs = [&]() {
			const Opt<const KnownLambdaBody*> opKnownLambdaBody = isEmpty(args)
				? none<const KnownLambdaBody*>()
				: getKnownLambdaBodyFromConstantOrExpr(args[0]);

			// TODO: also handle isCallFunPtr
			if (isCallFun(ctx.concretizeCtx, f) && opKnownLambdaBody.has()) {
				const KnownLambdaBody* klb = opKnownLambdaBody.force();
				const Arr<const ConstantOrExpr> itsArgs = tail(args);
				const SpecializeOnArgs itsSpecializeOnArgs = getSpecializeOnArgsForFun(ctx.concretizeCtx, f, itsArgs);
				const ConcreteFun* actualCalled = instantiateKnownLambdaBodyForDirectCall(ctx.concretizeCtx, klb, itsSpecializeOnArgs.specializeOnArgs);
				const Arr<const ConstantOrExpr> itsNotSpecializedArgs = itsSpecializeOnArgs.notSpecializedArgs;
				// If arg0 is a constant, completely omit it. Else pass it as the closure arg.
				const Arr<const ConstantOrExpr> allArgs = klb->hasClosure()
					? prepend<const ConstantOrExpr>(ctx.arena(), args[0], itsNotSpecializedArgs)
					: itsNotSpecializedArgs;
				return FunAndArgs{actualCalled, allArgs};
			} else {
				const SpecializeOnArgs specializeOnArgs = getSpecializeOnArgsForFun(ctx.concretizeCtx, f, args);
				const Arr<const FunDecl*> specImpls = map<const FunDecl*>{}(ctx.arena(), e.specImpls(), [&](const CalledDecl specImpl) {
					return specImpl.match(
						[](const FunDecl* f) {
							return f;
						},
						[&](const CalledDecl::SpecUseSig s) {
							return  ctx.getSpecImpl(s.sigIndexOverAllSpecUses);
						});
				});
				const ConcreteFun* fun = getConcreteFunForCallAndFillBody(
					ctx.concretizeCtx, f, ctx.typesToConcreteTypes(e.typeArgs()), specImpls, specializeOnArgs.specializeOnArgs);
				return FunAndArgs{fun, specializeOnArgs.notSpecializedArgs};
			}
		}();

		const ConcreteFunBody body = funAndArgs.fun->body();
		// TODO: actually, if args have no *side effects* (and do not throw exceptions) is the only condition we need to make the whole thing a constant
		if (body.isConstant() && allConstant(args))
			return ConstantOrExpr{body.asConstant()};
		else {
			const ConcreteExpr::CallConcreteFun fun = ConcreteExpr::CallConcreteFun{funAndArgs.fun, funAndArgs.notSpecializedArgs};
			return nuExpr(ctx.arena(), range, knownLambdaBodyFromConcreteFunBody(body), fun);
		}
	}

	const ConstantOrExpr concretizeFunAsLambda(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::FunAsLambda e) {
		Arena& arena = ctx.arena();
		if (e.isRemoteFun)
			todo<void>("funaslambda remote");

		const ConcreteFun* cf = getOrAddNonGenericConcreteFunAndFillBody(ctx.concretizeCtx, e.fun);
		const Str mangledName = cat(arena, cf->mangledName(), strLiteral("__asLambda"));
		const KnownLambdaBody* klb = arena.nu<KnownLambdaBody>()(
			cf->sig, mangledName, none<const ConcreteType>(), emptyArr<const ConstantOrLambdaOrVariable>());
		const Arr<const Expr> args = mapPointers<const Expr>{}(arena, e.fun->params(), [&](const Param* p) {
			return Expr{range, Expr::ParamRef{p}};
		});
		const Expr* body = arena.nu<const Expr>()(
			range,
			Expr::Call{
				e.fun->returnType(),
				Expr::Call::Called{
					CalledDecl{e.fun},
					emptyArr<const Type>(),
					emptyArr<const CalledDecl>()},
				args});
		const LambdaInfo info = LambdaInfo{ctx.concreteFunSource.containingFunInfo, body};
		ctx.concretizeCtx.knownLambdaBodyToInfo.add(arena, klb, info);
		return ConstantOrExpr{ctx.allConstants().lambda(arena, klb)};
	}

	const Opt<const ConcreteType> concreteTypeFromFields(Arena& arena, const Arr<const ConcreteField> fields, const Str mangledName) {
		return isEmpty(fields)
			? none<const ConcreteType>()
			: some<const ConcreteType>(
				ConcreteType::fromStruct(arena.nu<const ConcreteStruct>()(
					mangledName,
					none<const SpecialStructKind>(),
					ConcreteStructBody{ConcreteStructBody::Fields{fields}})));
	}

	const ConstantOrExpr concretizeLambda(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::Lambda e) {
		Arena& arena = ctx.arena();
		if (e.isRemoteFun)
			todo<void>("remote fun");

		const Arr<const ConstantOrExpr> closureArgsWithConstants = map<const ConstantOrExpr>{}(arena,  e.closure, [&](const ClosureField* f) {
			return concretizeExpr(ctx, *f->expr);
		});

		// Since we're just creating the lambda and not calling it, we can't specialize on params yet. (But we did specialize closure above.)
		const Arr<const ConcreteParam> nonSpecializedParams = concretizeParamsNoSpecialize(ctx.concretizeCtx, e.params, ctx.typeArgsScope());

		const Str mangledName = [&]() {
			Writer writer { arena };
			writer.writeStr(ctx.currentConcreteFun->mangledName());
			writer.writeStatic("__lambda");
			writer.writeUint(ctx.currentConcreteFun->nextLambdaIndex++);
			return writer.finish();
		}();

		const SpecializeOnArgs closureSpecialize = getSpecializeOnArgsForLambdaClosure(arena, closureArgsWithConstants);
		// Only the non-constant closures should go into the closure type.
		const Arr<const ConcreteField> closureFields = concretizeClosureFieldsAndSpecialize(
			ctx.concretizeCtx,
			e.closure,
			closureSpecialize.specializeOnArgs,
			ctx.typeArgsScope());
		const Opt<const ConcreteType> closureType = concreteTypeFromFields(arena, closureFields, mangledName);
		const ConcreteSig nonSpecializedSig = ConcreteSig{mangledName, ctx.getConcreteType(e.returnType), nonSpecializedParams};
		const KnownLambdaBody* klb = arena.nu<const KnownLambdaBody>()(
			nonSpecializedSig, mangledName, closureType, closureSpecialize.specializeOnArgs);
		const LambdaInfo info = LambdaInfo{ctx.concreteFunSource.containingFunInfo, e.body};
		ctx.concretizeCtx.knownLambdaBodyToInfo.add(arena, klb, info);
		return closureType.has()
			? nuExpr(arena, range, some<const KnownLambdaBody*>(klb), ConcreteExpr::Lambda{closureSpecialize.notSpecializedArgs})
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
				return nuExpr(ctx.arena(), range, getKnownLambdaBodyFromConstantOrExpr(then), ConcreteExpr::Let { local, valueExpr, then });
			});
	}

	const ConstantOrExpr concretizeMatch(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::Match e) {
		Arena& arena = ctx.arena();
		const ConstantOrExpr matched = concretizeExpr(ctx, *e.matched);
		const ConcreteStruct* matchedUnion = ctx.getConcreteType_forStructInst(e.matchedUnion).mustBeNonPointer();
		return matched.match(
			[&](const Constant* c) {
				const ConstantKind::Union u = c->kind.asUnion();
				assert(u.unionType == matchedUnion);
				const Expr::Match::Case kase = e.cases[u.memberIndex];
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
						const ConstantOrExpr then = concretizeWithLocal(ctx, kase.local.force(), local, *kase.then);
						return ConcreteExpr::Match::Case{some<const ConcreteLocal*>(local), then};
					} else
						return ConcreteExpr::Match::Case{none<const ConcreteLocal*>(), concretizeExpr(ctx, *kase.then)};
				});
				return ConstantOrExpr{nuExpr(arena, range, ConcreteExpr::Match{matchedExpr, matchedUnion, cases})};
			});
	}

	const ConstantOrExpr concretizeNewIfaceImpl(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::NewIfaceImpl e) {
		Arena& arena = ctx.arena();
		const ConcreteStruct* iface = ctx.getConcreteType_forStructInst(e.iface).mustBeNonPointer();
		const Arr<const ConcreteField> fields = map<const ConcreteField>{}(arena, e.fields, [&](const Expr::NewIfaceImpl::Field f) {
			return ConcreteField{mangleName(arena, f.name), ctx.getConcreteType(f.type)};
		});
		const Arr<const ConstantOrExpr> fieldInitializers = map<const ConstantOrExpr>{}(arena, e.fields, [&](const Expr::NewIfaceImpl::Field f) {
			return concretizeExpr(ctx, *f.expr);
		});
		const Str mangledNameBase = [&]() {
			Writer writer { arena };
			writer.writeStr(ctx.currentConcreteFun->mangledName());
			writer.writeStatic("__ifaceImpl");
			writer.writeUint(ctx.currentConcreteFun->nextNewIfaceImplIndex++);
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
					writer.writeStr(mangledNameBase);
					writer.writeStatic("__");
					writer.writeStr(sig.mangledName);
					return writer.finish();
				}();
				return ConcreteExpr::NewIfaceImpl::MessageImpl{mangledName, concreteBody};
			});
		const Opt<const ConcreteType> fieldsType = concreteTypeFromFields(arena, fields, mangledNameBase);
		const ConcreteExpr::NewIfaceImpl impl = ConcreteExpr::NewIfaceImpl{iface, fieldsType, fieldInitializers, messageImpls};
		return nuExpr(arena, range, impl);
	}

	const ConcreteParam* findCorrespondingConcreteParam(const ConcretizeExprCtx& ctx, const size_t paramIndex) {
		size_t paramI = 0;
		for (const size_t i : Range{paramIndex})
			if (!ctx.concreteFunSource.paramsSpecialize[i].isConstant())
				paramI++;
		return ctx.currentConcreteFun->paramsExcludingClosure().getPtr(paramI);
	}

	const ConstantOrExpr concretizeParamRef(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::ParamRef e) {
		const size_t paramIndex = e.param->index;
		// NOTE: we'll never see a ParamRef to a param from outside of a lambda -- that would be a ClosureFieldRef instead.
		const ConstantOrLambdaOrVariable paramSpecialize = ctx.concreteFunSource.paramsSpecialize[paramIndex];
		if (paramSpecialize.isConstant())
			return ConstantOrExpr{paramSpecialize.asConstant()};
		else {
			const ConcreteParam* concreteParam = findCorrespondingConcreteParam(ctx, paramIndex);
			const Opt<const KnownLambdaBody*> knownLambdaBody = getKnownLambdaBodyFromConstantOrLambdaOrVariable(paramSpecialize);
			return nuExpr(ctx.arena(), range, knownLambdaBody, ConcreteExpr::ParamRef{concreteParam});
		}
	}

	const ConstantOrExpr concretizeStructFieldAccess(ConcretizeExprCtx& ctx, const SourceRange range, const Expr::StructFieldAccess e) {
		Arena& arena = ctx.arena();
		const ConstantOrExpr target = concretizeExpr(ctx, *e.target);
		const ConcreteType type = ctx.getConcreteType_forStructInst(e.targetType);
		const size_t fieldIndex = e.fieldIndex();
		const Str fieldName = e.field->name;
		AllConstants& allConstants = ctx.allConstants();

		auto expr = [&]() -> ConstantOrExpr	{
			const ConcreteField* field = type.strukt->body().asFields().fields.getPtr(fieldIndex);
			return nuExpr(arena, range, ConcreteExpr::StructFieldAccess{target, field});
		};

		return target.match(
			[&](const Constant* c) {
				const ConstantKind kind = c->kind;
				if (kind.isRecord()) {
					const ConstantKind::Record r = kind.asRecord();
					assert(compareConcreteType(type, r.type) == Comparison::equal);
					return ConstantOrExpr{r.args[fieldIndex]};
				} else if (kind.isArray()) {
					const ConstantKind::Array a = kind.asArray();
					// Fields are "size", "data"
					switch (fieldIndex) {
						case 0:
							assert(strEqLiteral(fieldName, "size"));
							return ConstantOrExpr{allConstants.nat64(arena, a.size())};
						case 1:
							assert(strEqLiteral(fieldName, "data"));
							return ConstantOrExpr{allConstants.ptr(arena, c, 0)};
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
							return ConstantOrExpr{allConstants.funPtr(arena, cf)};
						}
						case 1:
							assert(strEqLiteral(fieldName, "closure"));
							if (klb->closureType.has())
								// Constant parts of closure are omitted, so this must be non-constant.
								return expr();
							else
								return ConstantOrExpr(allConstants._null);
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

	const ConstantOrExpr concretizeExpr(ConcretizeExprCtx& ctx, const Expr expr) {
		Arena& arena = ctx.arena();
		const SourceRange range = expr.range();

		auto r = [&](const Expr sub) -> const ConstantOrExpr {
			return concretizeExpr(ctx, sub);
		};

		auto getArgsNonConst = [&](const Arr<const Expr> argExprs) -> const Arr<const ConstantOrExpr> {
			return map<const ConstantOrExpr>{}(arena, argExprs, r);
		};

		auto getArgs = [&](const Arr<const Expr> argExprs) -> const Result<const Arr<const Constant*>, const Arr<const ConstantOrExpr>> {
			const Arr<const ConstantOrExpr> args = getArgsNonConst(argExprs);
			if (every(args, [&](const ConstantOrExpr c) { return c.isConstant(); }))
				return success<const Arr<const Constant*>, const Arr<const ConstantOrExpr>>(
					map<const Constant*>{}(arena, args, [](const ConstantOrExpr c) { return c.asConstant(); }));
			else
				return failure<const Arr<const Constant*>, const Arr<const ConstantOrExpr>>(args);
		};

		return expr.match(
			[](const Expr::Bogus) {
				return unreachable<const ConstantOrExpr>();
			},
			[&](const Expr::Call e) {
				return concretizeCall(ctx, range, e);
			},
			[&](const Expr::ClosureFieldRef e) {
				const ConcreteField* fromClosure = ctx.closure.getPtr(e.index());
				const ConstantOrLambdaOrVariable specialized = ctx.concreteFunSource.closureSpecialize.force()[e.index()];
				if (specialized.isConstant())
					return ConstantOrExpr{specialized.asConstant()};
				else {
					// The closure should be the 0th parameter to the current ConcreteFun.
					// Assert that it's called "_closure", then do a field access on it.
					unused(fromClosure);
					return todo<const ConstantOrExpr>("closurefieldref");
				}
			},
			[&](const Expr::Cond e) {
				const ConstantOrExpr cond = r(*e.cond);
				return cond.match(
					[&](const Constant* c) {
						return concretizeExpr(ctx, *(c->kind.asBool() ? e.then : e.elze));
					},
					[&](const ConcreteExpr* condExpr) {
						return nuExpr(arena, range, ConcreteExpr::Cond{condExpr, r(*e.then), r(*e.elze)});
					});
			},
			[&](const Expr::CreateArr e) {
				if (isEmpty(e.args))
					todo<void>("should this ever happen?");
				const Result<const Arr<const Constant*>, const Arr<const ConstantOrExpr>> args = getArgs(e.args);
				const ConcreteStruct* arrayType = ctx.getConcreteType_forStructInst(e.arrType).mustBeNonPointer();
				const ConcreteType elementType = ctx.getConcreteType(e.elementType());
				return args.match(
					[&](const Arr<const Constant*> constantArgs) {
						return ConstantOrExpr{ctx.allConstants().arr(arena, arrayType, elementType, constantArgs)};
					},
					[&](const Arr<const ConstantOrExpr> nonConstantArgs) {
						const ConcreteExpr::CreateArr ca = ConcreteExpr::CreateArr{arrayType, elementType, getAllocFun(ctx.concretizeCtx), nonConstantArgs};
						return nuExpr(arena, range, ca);
					});
			},
			[&](const Expr::CreateRecord e) {
				const ConcreteType s = ctx.getConcreteType_forStructInst(e.structInst);
				const Result<const Arr<const Constant*>, const Arr<const ConstantOrExpr>> args = getArgs(e.args);
				return args.match(
					[&](const Arr<const Constant*> constantArgs) {
						return ConstantOrExpr{ctx.allConstants().record(arena, s, constantArgs)};
					},
					[&](const Arr<const ConstantOrExpr> nonConstantArgs) {
						const Opt<const ConcreteFun*> alloc = s.isPointer
							? some<const ConcreteFun*>(getAllocFun(ctx.concretizeCtx))
							: none<const ConcreteFun*>();
						const ConcreteExpr::CreateRecord r = ConcreteExpr::CreateRecord{s, alloc, nonConstantArgs};
						return nuExpr(arena, range, r);
					});
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
				// TODO:PERF support constant unions
				auto x = ConcreteExpr::ImplicitConvertToUnion{unionType, memberType, r(*e.inner)};
				return nuExpr(arena, range, x);
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
					return nuExpr(arena, range, getKnownLambdaBodyFromConstantOrLambdaOrVariable(value), ConcreteExpr::LocalRef{let});
			},
			[&](const Expr::Match e) {
				return concretizeMatch(ctx, range, e);
			},
			[&](const Expr::MessageSend e) {
				// never a constant
				const ConstantOrExpr target = r(*e.target);
				const ConcreteStruct* iface = ctx.getConcreteType_forStructInst(e.iface).mustBeNonPointer();
				const ConcreteSig* message = iface->body().asIface().messages.getPtr(e.messageIndex);
				const Arr<const ConstantOrExpr> args = getArgsNonConst(e.args);
				return nuExpr(arena, range, ConcreteExpr::MessageSend{target, message, args});
			},
			[&](const Expr::NewIfaceImpl e) {
				return concretizeNewIfaceImpl(ctx, range, e);
			},
			[&](const Expr::ParamRef e) {
				return concretizeParamRef(ctx, range, e);
			},
			[&](const Expr::Seq e) {
				const ConstantOrExpr first = r(*e.first);
				const ConstantOrExpr then = r(*e.then);
				return first.match(
					[&](const Constant*) {
						// If first is a constant, skip it
						return then;
					},
					[&](const ConcreteExpr* e) {
						return nuExpr(arena, range, getKnownLambdaBodyFromConstantOrExpr(then), ConcreteExpr::Seq{e, then});
					});
			},
			[&](const Expr::StringLiteral e) {
				const Arr<const Constant*> chars = map<const Constant*>{}(arena, e.literal, [&](const char c) {
					return ctx.allConstants()._char(arena, c);
				});
				return ConstantOrExpr{ctx.allConstants().arr(
					arena,
					ctx.getConcreteType_forStructInst(ctx.concretizeCtx.commonTypes.str).mustBeNonPointer(),
					ctx.getConcreteType_forStructInst(ctx.concretizeCtx.commonTypes._char),
					chars)};
			},
			[&](const Expr::StructFieldAccess e) {
				return concretizeStructFieldAccess(ctx, range, e);
			});
	}
}

const ConstantOrExpr doConcretizeExpr(
	ConcretizeCtx& ctx,
	const ConcreteFunSource source,
	ConcreteFun* cf,
	const Expr e
) {
	ConcretizeExprCtx exprCtx {ctx, source, cf, emptyArr<const ConcreteField>(), emptyArr<const ConcreteField>()};
	return concretizeExpr(exprCtx, e);
}
