#include "./checkCall.h"

#include "../util/arrUtil.h"
#include "./checkExpr.h"

namespace {
	const Opt<SingleInferringType*> tryGetTypeArg(InferringTypeArgs inferringTypeArgs, const TypeParam* typeParam) {
		return tryGetTypeArg(inferringTypeArgs.params, inferringTypeArgs.args, typeParam);
	}

	SingleInferringType* getTypeArg(InferringTypeArgs inferringTypeArgs, const TypeParam* typeParam) {
		return tryGetTypeArg(inferringTypeArgs, typeParam).force();
	}

	const Arr<const Type> typeArgsFromAsts(ExprContext& ctx, const Arr<const TypeAst> typeAsts) {
		return map<const Type>{}(ctx.arena(), typeAsts, [&](const TypeAst it) { return typeFromAst(ctx, it); });
	}

	// If we call `foo \x ...` where `foo` is a function that doesn't exist,
	// don't continue checking the lambda it in the hopes that it might have a property.
	const Bool exprMightHaveProperties(const ExprAst ast) {
		return ast.kind.match(
			[](const CallAst) { return True; },
			[](const CondAst e) {
				return _and(exprMightHaveProperties(*e.then), exprMightHaveProperties(*e.elze));
			},
			[](const CreateArrAst) { return True; },
			[](const CreateRecordAst) { return True; },
			[](const FunAsLambdaAst) { return False; },
			[](const IdentifierAst) { return True; },
			[](const LambdaAst) { return False; },
			[](const LetAst e) { return exprMightHaveProperties(*e.then); },
			[](const LiteralAst) { return True; },
			// TODO: check all branches
			[](const MatchAst) { return True; },
			// Always returns fut
			[](const MessageSendAst) { return False; },
			[](const NewActorAst) { return False; },
			[](const SeqAst e) { return exprMightHaveProperties(*e.then); },
			[](const StructFieldSetAst) { return False; },
			// Always returns fut
			[](const ThenAst) { return False; });
	}

	struct Candidate {
		const CalledDecl called;
		// Note: this is always empty if calling a SpecSig
		const Arr<SingleInferringType> typeArgs;

		InferringTypeArgs inferringTypeArgs() const {
			return InferringTypeArgs{called.typeParams(), typeArgs};
		}
	};

	MutArr<Candidate> getInitialCandidates(ExprContext& ctx, const Str funName, const Arr<const Type> explicitTypeArgs, const size_t arity) {
		MutArr<Candidate> res {};
		eachFunInScope(ctx, funName, [&](const CalledDecl called) {
			const size_t nTypeParams = called.typeParams().size;
			if (called.arity() == arity &&
				(isEmpty(explicitTypeArgs) || nTypeParams == explicitTypeArgs.size)) {
				const Arr<SingleInferringType> inferringTypeArgs = fillArr<SingleInferringType>{}(ctx.arena(), nTypeParams, [&](const size_t i) {
					// InferringType for a type arg doesn't need a candidate; that's for a (value) arg's expected type
					return SingleInferringType{isEmpty(explicitTypeArgs) ? none<const Type>() : some<const Type>(at(explicitTypeArgs, i))};
				});
				push(ctx.arena(), res, Candidate{called, inferringTypeArgs});
			}
		});
		return res;
	}

	//TODO: share more code with matchActualAndCandidateTypeForReturn?
	//Though that handles implicit cast to union and this should not.
	// TODO: share code with matchTypesNoDiagnostic?
	const Bool matchActualAndCandidateTypeInner(Arena& arena, const Type fromExternal, const Type fromCandidate, Candidate& candidate) {
		return fromExternal.match(
			[](const Type::Bogus) {
				return todo<const Bool>("bogus");
			},
			[&](const TypeParam*) {
				// Case A: expected return type is a type parameter. So the function we're calling must be generic too.
				// Case B: If providing a type parameter as an argument, must be calling a function that is itself generic.
				// for example:
				//    id ?T(v ?T)
				//        v
				//    id2 ?U(v ?U)
				//        id v
				// In the call to `id`, we expect '?U', and the call will actually return '?U' (by inferring `id<?U>`).
				return fromCandidate.isTypeParam();
			},
			[&](const StructInst* structInstFromExternal) {
				return fromCandidate.match(
					[](const Type::Bogus) {
						return False;
					},
					[&](const TypeParam* p) {
						return getTypeArg(candidate.inferringTypeArgs(), p)->setTypeNoDiagnostic(arena, fromExternal);
					},
					[&](const StructInst* structInstFromCandidate) {
						return _and(
							ptrEquals(structInstFromExternal->decl, structInstFromCandidate->decl),
							eachCorresponds(
								structInstFromExternal->typeArgs,
								structInstFromCandidate->typeArgs,
								[&](const Type a, const Type b) { return matchActualAndCandidateTypeInner(arena, a, b, candidate); }));
					});
			});
	}

	const Bool typeArgsMatch(Arena& arena, const Arr<const Type> expected, const Arr<const Type> actual, Candidate& candidate) {
		return eachCorresponds(expected, actual, [&](const Type expectedTypeArg, const Type declaredTypeArg) {
			return matchActualAndCandidateTypeInner(arena, expectedTypeArg, declaredTypeArg, candidate);
		});
	}

	const Bool matchActualAndCandidateTypeForReturn(Arena& arena, const Type expectedReturnType, const Type candidateReturnType, Candidate& candidate) {
		auto handleTypeParam = [&](const TypeParam* p) {
			// Function returns a type parameter, and we get expect it to return <bogus> or a particular struct inst.
			const Opt<SingleInferringType*> t = tryGetTypeArg(candidate.inferringTypeArgs(), p);
			if (t.has())
				return t.force()->setTypeNoDiagnostic(arena, expectedReturnType);
			else
				return todo<const Bool>("matchActualAndCandidateTypeForReturn type param");
		};

		return expectedReturnType.match(
			[&](const Type::Bogus) {
				return candidateReturnType.match(
					[](const Type::Bogus) { return True; },
					handleTypeParam,
					[](const StructInst*) { return True; });
			},
			[&](const TypeParam*) {
				// We expect to return a type param.
				// It's possible that declaredReturnType is one of the candidate's type params.
				if (candidateReturnType.isTypeParam()) {
					const Opt<SingleInferringType*> typeArg = tryGetTypeArg(candidate.inferringTypeArgs(), candidateReturnType.asTypeParam());
					return typeArg.has()
						? typeArg.force()->setTypeNoDiagnostic(arena, expectedReturnType)
						: expectedReturnType.typeEquals(candidateReturnType);
				} else
					return False;
			},
			[&](const StructInst* expectedStructInst) {
				return candidateReturnType.match(
					[](const Type::Bogus) {
						return todo<const Bool>("matchActualAndCandidateTypeForReturn bogus");
					},
					handleTypeParam,
					[&](const StructInst* actualStructInst) {
						if (ptrEquals(expectedStructInst->decl, actualStructInst->decl))
							return typeArgsMatch(arena, expectedStructInst->typeArgs, actualStructInst->typeArgs, candidate);
						else {
							const StructBody body = expectedStructInst->decl->body();
							if (body.isUnion()) {
								// May be suitable for implicitConvertToUnion
								const Opt<const StructInst*> member = find(body.asUnion().members, [&](const StructInst* i) {
									return ptrEquals(i->decl, actualStructInst->decl);
								});
								return _and(
									member.has(),
									typeArgsMatch(
										arena,
										instantiateStructInst(arena, member.force(), expectedStructInst)->typeArgs,
										actualStructInst->typeArgs,
										candidate));
							} else
								return False;
						}
					});
			});
	}

	const Type getCandidateExpectedParameterTypeRecur(Arena& arena, const Candidate& candidate, const Type candidateParamType) {
		return candidateParamType.match(
			[](const Type::Bogus) {
				return Type{Type::Bogus{}};
			},
			[&](const TypeParam* p) {
				const Opt<SingleInferringType*> sit = tryGetTypeArg(InferringTypeArgs{candidate.called.typeParams(), candidate.typeArgs}, p);
				const Opt<const Type> inferred = sit.has() ? sit.force()->tryGetInferred() : none<const Type>();
				return inferred.has() ? inferred.force() : Type{p};
			},
			[&](const StructInst* i) {
				//TODO:PERF, the map might change nothing, so don't reallocate in that situation
				const Arr<const Type> typeArgs = map<const Type>{}(arena, i->typeArgs, [&](const Type t) {
					return getCandidateExpectedParameterTypeRecur(arena, candidate, t);
				});
				return Type{instantiateStructNeverDelay(arena, i->decl, typeArgs)};
			});
	}

	const Type getCandidateExpectedParameterType(Arena& arena, const Candidate& candidate, const size_t argIdx) {
		return getCandidateExpectedParameterTypeRecur(arena, candidate, at(candidate.called.params(), argIdx).type);
	}

	struct CommonOverloadExpected {
		Expected expected;
		const Bool isExpectedFromCandidate;
	};

	CommonOverloadExpected getCommonOverloadParamExpected(Arena& arena, const Arr<Candidate> candidates, const size_t argIdx) {
		switch (candidates.size) {
			case 0:
				return CommonOverloadExpected{Expected::infer(), False};
			case 1: {
				const Candidate& candidate = only(candidates);
				const Type t = getCandidateExpectedParameterType(arena, candidate, argIdx);
				return CommonOverloadExpected{Expected{some<const Type>(t), candidate.inferringTypeArgs()}, True};
			}
			default:
				// For multiple candidates, only have an expected type if they have exactly the same param type
				Cell<const Opt<const Type>> expected { none<const Type>() };
				for (const Candidate& candidate : candidates) {
					// If we get a generic candidate and haven't inferred this param type yet, no expected type.
					const Type paramType = getCandidateExpectedParameterType(arena, candidate, argIdx);
					if (expected.get().has()) {
						if (!typeEquals(paramType, expected.get().force()))
							// Only get an expected type if all candidates expect it.
							return CommonOverloadExpected{Expected::infer(), False};
					} else
						expected.set(some<const Type>(paramType));
				}
				// Can't be inferring type arguments for candidates if there's more than one.
				// (Handle that *after* getting the arg type.)
				return CommonOverloadExpected{Expected{expected.get()}, False};
		}
	}

	const Opt<const Expr::StructFieldAccess> tryGetStructFieldAccess(ExprContext& ctx, const Str funName, const Expr arg) {
		const Opt<const StructAndField> field = tryGetStructField(arg.getType(ctx.arena(), ctx.commonTypes), funName);
		return field.has()
			? some<const Expr::StructFieldAccess>(Expr::StructFieldAccess{ctx.alloc(arg), field.force().structInst, field.force().field})
			: none<const Expr::StructFieldAccess>();
	}

	void checkCallFlags(CheckCtx& ctx, const SourceRange range, const FunFlags calledFlags, const FunFlags callerFlags) {
		if (!calledFlags.noCtx && callerFlags.noCtx)
			ctx.diag(range, Diag{Diag::CantCallNonNoCtx{}});
		if (calledFlags.summon && !callerFlags.summon)
			ctx.diag(range, Diag{Diag::CantCallSummon{}});
		if (calledFlags.unsafe && !callerFlags.trusted && !callerFlags.unsafe)
			ctx.diag(range, Diag{Diag::CantCallUnsafe{}});
	}

	template <typename TypeArgsEqual>
	const Bool structInstsEqual(const StructInst* a, const StructInst* b, TypeArgsEqual typeArgsEqual) {
		return _and(
			ptrEquals(a->decl, b-> decl),
			eachCorresponds(a->typeArgs, b->typeArgs, typeArgsEqual));
	}

	void checkCalledDeclFlags(ExprContext& ctx, const CalledDecl res, const SourceRange range) {
		res.match(
			[&](const FunDecl* f) {
				checkCallFlags(ctx.checkCtx, range, f->flags, ctx.outermostFun->flags);
			},
			[](const SpecSig) {
				// For a spec, we check the flags when providing the spec impl
			});
	}

	void filterByReturnType(Arena& arena, MutArr<Candidate>& candidates, const Type expectedReturnType) {
		// Filter by return type. Also does type argument inference on the candidate.
		filterUnordered(candidates, [&](Candidate& candidate) {
			return matchActualAndCandidateTypeForReturn(arena, expectedReturnType, candidate.called.returnType(), candidate);
		});
	}

	void filterByParamType(Arena& arena, MutArr<Candidate>& candidates, const Type actualArgType, const size_t argIdx) {
		// Remove candidates that can't accept this as a param. Also does type argument inference on the candidate.
		filterUnordered(candidates, [&](Candidate& candidate) {
			const Type expectedArgType = getCandidateExpectedParameterType(arena, candidate, argIdx);
			return matchTypesNoDiagnostic(arena, expectedArgType, actualArgType, candidate.inferringTypeArgs());
		});
	}

	const Opt<const Called> getCalledFromCandidate(ExprContext& ctx, const SourceRange range, const Candidate candidate, const bool allowSpecs);

	const Opt<const Called> findSpecSigImplementation(ExprContext& ctx, const SourceRange range, const Sig specSig) {
		MutArr<Candidate> candidates = getInitialCandidates(ctx, specSig.name, emptyArr<const Type>(), specSig.arity());
		filterByReturnType(ctx.arena(), candidates, specSig.returnType);
		for (const size_t argIdx : Range{specSig.arity()})
			filterByParamType(ctx.arena(), candidates, at(specSig.params, argIdx).type, argIdx);

		// If any candidates left take specs -- leave as a TODO
		const Arr<const Candidate> candidatesArr = asConst(freeze(candidates));
		switch (candidatesArr.size) {
			case 0:
				ctx.diag(range, Diag{Diag::SpecImplNotFound{specSig.name}});
				return none<const Called>();
			case 1:
				return getCalledFromCandidate(ctx, range, only(candidatesArr), /*allowSpecs*/ false);
			default:
				todo<void>("diagnostic: multiple functions satisfy the spec");
				return none<const Called>();
		}
	}

	// On failure, returns none.
	const Opt<const Arr<const Called>> checkSpecImpls(ExprContext& ctx, const SourceRange range, const FunDecl* called, const Arr<const Type> typeArgs, const bool allowSpecs) {
		// We store the impls in a flat array. Calculate the size ahead of time.
		const size_t size = [&]() {
			size_t s = 0;
			for (const SpecInst* specInst : called->specs)
				s += specInst->sigs.size;
			return s;
		}();

		if (size != 0 && !allowSpecs) {
			ctx.diag(range, Diag{Diag::SpecImplHasSpecs{}});
			return none<const Arr<const Called>>();
		} else {
			MutArr<const Called> res = newUninitializedMutArr<const Called>(ctx.arena(), size);
			size_t outI = 0;
			for (const SpecInst* specInst : called->specs) {
				// Note: specInst was instantiated potentialyl based on f's params.
				// Meed to instantiate it again.
				const SpecInst* specInstInstantiated = instantiateSpecInst(ctx.arena(), specInst, TypeParamsAndArgs{called->typeParams, typeArgs});
				for (const Sig sig : specInstInstantiated->sigs) {
					const Opt<const Called> impl = findSpecSigImplementation(ctx, range, sig);
					if (!impl.has())
						return none<const Arr<const Called>>();
					setAt<const Called>(res, outI, impl.force());
					outI++;
				}
			}
			assert(outI == size);
			return some<const Arr<const Called>>(freeze(res));
		}
	}

	const Opt<const Arr<const Type>> finishCandidateTypeArgs(ExprContext& ctx, const SourceRange range, const Candidate candidate) {
		const Opt<const Arr<const Type>> res = mapOrNone<const Type>{}(
			ctx.arena(),
			candidate.typeArgs,
			[](const SingleInferringType& i) {
				return i.tryGetInferred();
			});
		if (!res.has())
			ctx.diag(range, Diag{Diag::CantInferTypeArguments{}});
		return res;
	}

	const Opt<const Called> getCalledFromCandidate(ExprContext& ctx, const SourceRange range, const Candidate candidate, const bool allowSpecs) {
		checkCalledDeclFlags(ctx, candidate.called, range);
		const Opt<const Arr<const Type>> candidateTypeArgs = finishCandidateTypeArgs(ctx, range, candidate);
		if (candidateTypeArgs.has()) {
			const Arr<const Type> typeArgs = candidateTypeArgs.force();
			return candidate.called.match(
				[&](const FunDecl* f) {
					const Opt<const Arr<const Called>> specImpls = checkSpecImpls(ctx, range, f, typeArgs, allowSpecs);
					if (specImpls.has())
						return some<const Called>(Called{instantiateFun(ctx.arena(), f, typeArgs, specImpls.force())});
					else
						return none<const Called>();
				},
				[&](const SpecSig s) {
					return some<const Called>(Called{s});
				});
		} else
			return none<const Called>();
	}

	const CheckedExpr checkCallAfterChoosingOverload(
		ExprContext& ctx,
		const Candidate candidate,
		const SourceRange range,
		const Arr<const Expr> args,
		Expected& expected
	) {
		const Opt<const Called> opCalled = getCalledFromCandidate(ctx, range, candidate, /*allowSpecs*/ true);
		if (opCalled.has()) {
			const Called called = opCalled.force();
			//TODO: PERF second return type check may be unnecessary if we already filtered by return type at the beginning
			return expected.check(ctx, called.returnType(), Expr{range, Expr::Call{called, args}});
		}
		else
			return expected.bogus(range);
	}
}

const CheckedExpr checkCall(ExprContext& ctx, const SourceRange range, const CallAst ast, Expected& expected) {
	const size_t arity = ast.args.size;

	const Bool mightBePropertyAccess = _and(arity == 1, exprMightHaveProperties(only(ast.args)));

	const Arr<const Type> explicitTypeArgs = typeArgsFromAsts(ctx, ast.typeArgs);
	MutArr<Candidate> candidates = getInitialCandidates(ctx, ast.funName, explicitTypeArgs, arity);
	// TODO: may not need to be deeply instantiated to do useful filtering here
	const Opt<const Type> expectedReturnType = expected.tryGetDeeplyInstantiatedType(ctx.arena());
	if (expectedReturnType.has())
		filterByReturnType(ctx.arena(), candidates, expectedReturnType.force());

	Cell<const Bool> someArgIsBogus { False };
	const Opt<const Arr<const Expr>> args = fillArrOrFail<const Expr>{}(ctx.arena(), arity, [&](const size_t argIdx) {
		if (isEmpty(candidates) && !mightBePropertyAccess)
			// Already certainly failed.
			return none<const Expr>();

		CommonOverloadExpected common = getCommonOverloadParamExpected(ctx.arena(), tempAsArr(candidates), argIdx);
		Expr arg = checkExpr(ctx, at(ast.args, argIdx), common.expected);

		// If it failed to check, don't continue, just stop there.
		if (arg.typeIsBogus(ctx.arena())) {
			someArgIsBogus.set(True);
			return none<const Expr>();
		}

		// If the Inferring already came from the candidate, no need to do more work.
		if (!common.isExpectedFromCandidate) {
			const Type actualArgType = common.expected.inferred();
			filterByParamType(ctx.arena(), candidates, actualArgType, argIdx);
		}
		return some<const Expr>(arg);
	});

	const Arr<const Candidate> candidatesArr = asConst(freeze(candidates));

	if (someArgIsBogus.get())
		return expected.bogus(range);

	if (mightBePropertyAccess && arity == 1 && args.has()) {
		// Might be a struct field access
		const Opt<const Expr::StructFieldAccess> sfa = tryGetStructFieldAccess(ctx, ast.funName, only(args.force()));
		if (sfa.has()) {
			if (!isEmpty(candidatesArr))
				todo<void>("ambiguous call vs property access");
			return expected.check(ctx, sfa.force().accessedFieldType(), Expr{range, sfa.force()});
		}
	}

	if (!args.has() || candidatesArr.size != 1) {
		if (isEmpty(candidatesArr))
			ctx.diag(range, Diag{Diag::NoSuchFunction{ctx.checkCtx.copyStr(ast.funName)}});
		else {
			const Arr<const CalledDecl> calledDecls = map<const CalledDecl>{}(
				ctx.arena(),
				candidatesArr,
				[](const Candidate c) { return c.called; });
			ctx.diag(range, Diag{Diag::MultipleFunctionCandidates{calledDecls}});
		}
		return expected.bogus(range);
	} else
		return checkCallAfterChoosingOverload(ctx, only(candidatesArr), range, args.force(), expected);
}
