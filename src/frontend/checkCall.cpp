#include "./checkCall.h"

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
	bool exprMightHaveProperties(const ExprAst ast) {
		return ast.kind.match(
			[](const CallAst) { return true; },
			[](const CondAst e) {
				return exprMightHaveProperties(*e.then) && exprMightHaveProperties(*e.elze);
			},
			[](const CreateArrAst) { return true; },
			[](const CreateRecordAst) { return true; },
			[](const FunAsLambdaAst) { return false; },
			[](const IdentifierAst) { return true; },
			[](const LambdaAst) { return false; },
			[](const LetAst e) { return exprMightHaveProperties(*e.then); },
			[](const LiteralAst) { return true; },
			// TODO: check all branches
			[](const MatchAst) { return true; },
			// Always returns fut
			[](const MessageSendAst) { return false; },
			[](const NewActorAst) { return false; },
			[](const SeqAst e) { return exprMightHaveProperties(*e.then); },
			// Always returns fut
			[](const ThenAst) { return false; });
	}

	const Type instantiateSpecUse(const Type declaredCandidateType, const CalledDecl called) {
		return called.match(
			[&](const FunDecl*) { return declaredCandidateType; },
			[&](const CalledDecl::SpecUseSig sus) {
				if (declaredCandidateType.isTypeParam()) {
					const Opt<const Type*> t = tryGetTypeArg(sus.specUse->spec->typeParams, sus.specUse->typeArgs, declaredCandidateType.asTypeParam());
					return t.has() ? *t.force() : declaredCandidateType;
				} else
					return declaredCandidateType;
			});
	}

	struct Candidate {
		const CalledDecl called;
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
				(explicitTypeArgs.isEmpty() || nTypeParams == explicitTypeArgs.size)) {
				const Arr<SingleInferringType> inferringTypeArgs = fillArr<SingleInferringType>{}(ctx.arena(), nTypeParams, [&](const size_t i) {
					// InferringType for a type arg doesn't need a candidate; that's for a (value) arg's expected type
					return SingleInferringType{explicitTypeArgs.isEmpty() ? none<const Type>() : some<const Type>(explicitTypeArgs[i])};
				});
				res.push(ctx.arena(), Candidate{called, inferringTypeArgs});
			}
		});
		return res;
	}

	//TODO: share more code with matchActualAndCandidateTypeForReturn?
	//Though that handles implicit cast to union and this should not.
	// TODO: share code with matchTypesNoDiagnostic?
	bool matchActualAndCandidateTypeInner(Arena& arena, const Type fromExternal, const Type declaredCandidateType, Candidate& candidate) {
		const Type fromCandidate = instantiateSpecUse(declaredCandidateType, candidate.called);
		return fromExternal.match(
			/*bogus*/ []() { return todo<bool>("bogus"); },
			[&](const TypeParam*) {
				// Case A: expected return type is a type parameter. So the function we're calling must be generic too.
				// Case B: If providing a type parameter as an argument, must be calling a function that is itself generic.
				// for example:
				//    id ?T(v ?T)
				//        v
				//    id2 ?U(v ?U)
				//        id v
				// In the call to `id`, we expect '?U', and the call will actually return '?U' (by inferring `id<?U>`).
				todo<void>("do inference here?"); // see matchActualAndCandidateTypeForReturn
				return fromCandidate.isTypeParam();
			},
			[&](const StructInst* structInstFromExternal) {
				return fromCandidate.match(
					/*bogus*/ []() { return false; },
					[&](const TypeParam* p) {
						return getTypeArg(candidate.inferringTypeArgs(), p)->setTypeNoDiagnostic(arena, fromExternal);
					},
					[&](const StructInst* structInstFromCandidate) {
						return ptrEquals(structInstFromExternal->decl, structInstFromCandidate->decl)
							&& eachCorresponds(
								structInstFromExternal->typeArgs,
								structInstFromCandidate->typeArgs,
								[&](const Type a, const Type b) { return matchActualAndCandidateTypeInner(arena, a, b, candidate); });
					});
			});
	}

	bool typeArgsMatch(Arena& arena, const Arr<const Type> expected, const Arr<const Type> actual, Candidate& candidate) {
		return eachCorresponds(expected, actual, [&](const Type expectedTypeArg, const Type declaredTypeArg) {
			return matchActualAndCandidateTypeInner(arena, expectedTypeArg, declaredTypeArg, candidate);
		});
	}

	bool matchActualAndCandidateTypeForReturn(Arena& arena, const Type expectedReturnType, const Type declaredReturnType, Candidate& candidate) {
		auto handleTypeParam = [&](const TypeParam* p) {
			// Function returns a type parameter, and we get expect it to return <bogus> or a particular struct inst.
			const Opt<SingleInferringType*> t = tryGetTypeArg(candidate.inferringTypeArgs(), p);
			if (t.has())
				return t.force()->setTypeNoDiagnostic(arena, expectedReturnType);
			else
				return todo<bool>("matchActualAndCandidateTypeForReturn type param");
		};

		const Type instantiatedReturnType = instantiateSpecUse(declaredReturnType, candidate.called);
		return expectedReturnType.match(
			/*bogus*/ [&]() {
				return instantiatedReturnType.match(
					/*bogus*/ []() { return true; },
					handleTypeParam,
					[](const StructInst*) { return true; });
			},
			[&](const TypeParam*) {
				// We expect to return a type param.
				// It's possible that declaredReturnType is one of the candidate's type params.
				if (declaredReturnType.isTypeParam()) {
					const Opt<SingleInferringType*> typeArg = tryGetTypeArg(candidate.inferringTypeArgs(), declaredReturnType.asTypeParam());
					return typeArg.has()
						? typeArg.force()->setTypeNoDiagnostic(arena, expectedReturnType)
						: expectedReturnType.typeEquals(instantiatedReturnType);
				} else
					return false;
			},
			[&](const StructInst* expectedStructInst) {
				return instantiatedReturnType.match(
					/*bogus*/ []() { return todo<bool>("matchActualAndCandidateTypeForReturn bogus"); },
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
								return member.has() && typeArgsMatch(
									arena,
									instantiateStructInst(arena, member.force(), expectedStructInst)->typeArgs,
									actualStructInst->typeArgs,
									candidate);
							} else
								return false;
						}
					});
			});
	}

	//TODO: just reuse getCandidateExpectedParameterType?
	const Type getCallReturnType(Arena& arena, const Type declaredReturnType, const CalledDecl called, const Arr<const Type> candidateTypeArgs) {
		const Type instantiatedDeclaredReturnType = instantiateSpecUse(declaredReturnType, called);
		return instantiatedDeclaredReturnType.match(
			/*bogus*/ []() {
				return todo<const Type>("bogus");
			},
			[&](const TypeParam* p) {
				const Opt<const Type*> t = tryGetTypeArg(called.typeParams(), candidateTypeArgs, p);
				return t.has() ? *t.force() : Type{p};
			},
			[&](const StructInst* i) {
				const Arr<const Type> typeArgs = map<const Type>{}(arena, i->typeArgs, [&](const Type t) {
					return getCallReturnType(arena, t, called, candidateTypeArgs);
				});
				//TODO:PERF: the map might change nothing, so don't reallocate in that situation
				return Type{instantiateStructNeverDelay(arena, i->decl, typeArgs)};
			});
	}

	const Type getCandidateExpectedParameterTypeRecur(Arena& arena, const Candidate& candidate, const Type declaredType) {
		const Type specInstantiated = instantiateSpecUse(declaredType, candidate.called);
		return specInstantiated.match(
			/*bogus*/ []() { return Type::bogus(); },
			[&](const TypeParam* p) {
				const Opt<SingleInferringType*> sit = tryGetTypeArg(InferringTypeArgs{candidate.called.typeParams(), candidate.typeArgs}, p);
				const Opt<const Type> inferred = sit.has() ? sit.force()->tryGetInferred() : none<const Type>();
				return inferred.has() ? inferred.force() : specInstantiated;
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
		return getCandidateExpectedParameterTypeRecur(arena, candidate, candidate.called.params()[argIdx].type);
	}

	struct CommonOverloadExpected {
		Expected expected;
		const bool isExpectedFromCandidate;
	};

	CommonOverloadExpected getCommonOverloadParamExpected(Arena& arena, const Arr<Candidate> candidates, const size_t argIdx) {
		switch (candidates.size) {
			case 0:
				return CommonOverloadExpected{Expected::infer(), false};
			case 1: {
				const Candidate& candidate = only(candidates);
				const Type t = getCandidateExpectedParameterType(arena, candidate, argIdx);
				return CommonOverloadExpected{Expected{some<const Type>(t), candidate.inferringTypeArgs()}, true};
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
							return CommonOverloadExpected{Expected::infer(), false};
					} else
						expected.set(some<const Type>(paramType));
				}
				// Can't be inferring type arguments for candidates if there's more than one.
				// (Handle that *after* getting the arg type.)
				return CommonOverloadExpected{Expected{expected.get()}, false};
		}
	}

	const Opt<const Expr::StructFieldAccess> tryGetStructFieldAccess(ExprContext& ctx, const Str funName, const Expr arg) {
		const Type argType = arg.getType(ctx.arena(), ctx.commonTypes);
		if (argType.isStructInst()) {
			const StructInst* argStructInst = argType.asStructInst();
			return argStructInst->body().match(
				/*builtin*/ []() {
					return none<const Expr::StructFieldAccess>();
				},
				[&](const StructBody::Fields f) {
					const Opt<const StructField*> field = findPtr(f.fields, [&](const StructField* f) {
						return strEq(f->name, funName);
					});
					return field.has()
						? some<const Expr::StructFieldAccess>(Expr::StructFieldAccess{ctx.alloc(arg), argStructInst, field.force()})
						: none<const Expr::StructFieldAccess>();
				},
				[](const StructBody::Union) {
					return none<const Expr::StructFieldAccess>();
				},
				[](const StructBody::Iface) {
					return none<const Expr::StructFieldAccess>();
				});
		} else
			return none<const Expr::StructFieldAccess>();
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
	bool structInstsEqual(const StructInst* a, const StructInst* b, TypeArgsEqual typeArgsEqual) {
		return ptrEquals(a->decl, b-> decl) && eachCorresponds(a->typeArgs, b->typeArgs, typeArgsEqual);
	}

	bool sigTypesEqualNoSub(const Type specType, const Type actual) {
		return specType.match(
			/*bogus*/ []() {
				return todo<bool>("sigTypesEqualNoSub bogus");
			},
			[](const TypeParam*) {
				return unreachable<bool>();
			},
			[&](const StructInst* i) {
				// Type args should be fully substituted now, so just use ptrEquals
				return actual.isStructInst() && ptrEquals(i, actual.asStructInst());
			});
	}

	//TODO:MOVE
	struct TypeParamsAndArgs {
		const Arr<const TypeParam> typeParams;
		const Arr<const Type> typeArgs;

		const Type substitute(const TypeParam* p) const {
			assert(ptrEquals(typeParams.getPtr(p->index), p));
			return typeArgs[p->index];
		}
	};

	// substitutedFromSpec will be the type arg to the SpecUse.
	// This may further need to be substituted by a type arg from the candidate.
	bool sigTypesEqualOneSub(const Type substitutedFromSpec, const Type actual, const TypeParamsAndArgs candidateTypeArgs) {
		return substitutedFromSpec.match(
			/*bogus*/ [&]() {
				return todo<bool>("sigtypesequalonesub bogus");
			},
			[&](const TypeParam* p) {
				return sigTypesEqualNoSub(candidateTypeArgs.substitute(p), actual);
			},
			[&](const StructInst* i) {
				return actual.isStructInst() && structInstsEqual(i, actual.asStructInst(), [&](const Type specType, const Type actualType) {
					return sigTypesEqualOneSub(specType, actualType, candidateTypeArgs);
				});
			});
	}

	struct TypeArgsScope {
		const TypeParamsAndArgs specUseTypeArgs;
		const TypeParamsAndArgs candidateTypeArgs;
	};

	bool sigTypesEqual(const Type typeFromSpec, const Type actual, const TypeArgsScope typeArgs) {
		return typeFromSpec.match(
			/*bogus*/ []() {
				return todo<bool>("sigtypesequal bogus");
			},
			[&](const TypeParam* p) {
				return sigTypesEqualOneSub(typeArgs.specUseTypeArgs.substitute(p), actual, typeArgs.candidateTypeArgs);
			},
			[&](const StructInst* i) {
				return actual.isStructInst() && structInstsEqual(i, actual.asStructInst(), [&](const Type specType, const Type actualType) {
					return sigTypesEqual(specType, actualType, typeArgs);
				});
			});
	}

	bool sigMatches(const Sig specSig, const Sig actual, const TypeArgsScope typeArgs) {
		return sigTypesEqual(specSig.returnType, actual.returnType, typeArgs) &&
			eachCorresponds(specSig.params, actual.params, [&](const Param a, const Param b) {
				return sigTypesEqual(a.type, b.type, typeArgs);
			});
	}

	const CalledDecl findSpecSigImplementation(ExprContext& ctx, const SourceRange range, const Sig specSig, const TypeArgsScope typeArgs) {
		Cell<const Opt<const CalledDecl>> match { none<const CalledDecl>() };
		eachFunInScope(ctx, specSig.name, [&](const CalledDecl called) {
			// TODO: support matching a aspec with a generic function
			if (called.typeParams().isEmpty() && sigMatches(specSig, *called.sig(), typeArgs)) {
				if (match.get().has())
					todo<void>("findSpecSigImplementation -- two sigs match");
				match.set(some<const CalledDecl>(called));
			}
		});

		if (match.get().has()) {
			const CalledDecl res = match.get().force();
			if (res.isFunDecl())
				checkCallFlags(ctx.checkCtx, range, res.asFunDecl()->flags, ctx.outermostFun->flags);
			return res;
		} else
			return todo<const CalledDecl>("findSpecSigImplementation -- no match");

	}

	const Arr<const CalledDecl> checkSpecImpls(ExprContext& ctx, const SourceRange range, const CalledDecl called, const Arr<const Type> typeArgs) {
		return called.match(
			[&](const FunDecl* f) {
				// We store a flat array. Calculate the size ahead of time.
				const size_t size = [&]() {
					size_t s = 0;
					for (const SpecUse specUse : f->specs)
						s += specUse.spec->sigs.size;
					return s;
				}();

				MutSlice<const CalledDecl> res = newUninitializedMutSlice<const CalledDecl>(ctx.arena(), size);
				size_t outI = 0;
				for (const SpecUse specUse : f->specs) {
					const TypeArgsScope typeArgsScope = TypeArgsScope{
						TypeParamsAndArgs{specUse.spec->typeParams, specUse.typeArgs},
						TypeParamsAndArgs{called.typeParams(), typeArgs}};
					for (const Sig sig : specUse.spec->sigs) {
						res.set(outI, findSpecSigImplementation(ctx, range, sig, typeArgsScope));
						outI++;
					}
				}
				assert(outI == size);
				return res.freeze();

			},
			[](const CalledDecl::SpecUseSig) {
				// Specs can't have specs
				return emptyArr<const CalledDecl>();
			});
	}

	const CheckedExpr checkCallAfterChoosingOverload(
		ExprContext& ctx,
		const Candidate candidate,
		const SourceRange range,
		const Arr<const Expr> args,
		Expected& expected
	) {
		if (candidate.called.isFunDecl())
			// For a spec, we check the flags when providing the spec impl
			checkCallFlags(ctx.checkCtx, range, candidate.called.asFunDecl()->flags, ctx.outermostFun->flags);
		const Opt<const Arr<const Type>> candidateTypeArgs = mapOrNone<const Type>{}(
			ctx.arena(),
			candidate.typeArgs,
			[](const SingleInferringType& i) {
				return i.tryGetInferred();
			});
		if (candidateTypeArgs.has()) {
			const Type callReturnType = getCallReturnType(ctx.arena(), candidate.called.returnType(), candidate.called, candidateTypeArgs.force());
			//TODO: PERF second return type check may be unnecessary if we already filtered by return type at the beginning
			const Arr<const CalledDecl> specImpls = checkSpecImpls(ctx, range, candidate.called, candidateTypeArgs.force());
			Expr::Call::Called called = Expr::Call::Called{candidate.called, candidateTypeArgs.force(), specImpls};
			const Expr expr = Expr{range, Expr::Call{callReturnType, called, args}};
			return expected.check(ctx, callReturnType, expr);
		} else {
			ctx.diag(range, Diag{Diag::CantInferTypeArguments{}});
			return expected.bogus(range);
		}
	}
}

const CheckedExpr checkCall(ExprContext& ctx, const SourceRange range, const CallAst ast, Expected& expected) {
	const size_t arity = ast.args.size;

	const bool mightBePropertyAccess = arity == 1 && exprMightHaveProperties(only(ast.args));

	const Arr<const Type> explicitTypeArgs = typeArgsFromAsts(ctx, ast.typeArgs);
	MutArr<Candidate> candidates = getInitialCandidates(ctx, ast.funName, explicitTypeArgs, arity);
	// TODO: may not need to be deeply instantiated to do useful filtering here
	const Opt<const Type> expectedReturnType = expected.tryGetDeeplyInstantiatedType(ctx.arena());
	if (expectedReturnType.has())
		// Filter by return type. Also does type argument inference on the candidate.
		filterUnordered(candidates, [&](Candidate& candidate) {
			return matchActualAndCandidateTypeForReturn(ctx.arena(), expectedReturnType.force(), candidate.called.returnType(), candidate);
		});

	bool someArgIsBogus = false;
	const Opt<const Arr<const Expr>> args = fillArrOrFail<const Expr>{}(ctx.arena(), arity, [&](const size_t argIdx) {
		if (candidates.isEmpty() && !mightBePropertyAccess)
			// Already certainly failed.
			return none<const Expr>();

		CommonOverloadExpected common = getCommonOverloadParamExpected(ctx.arena(), candidates.tempAsArr(), argIdx);
		Expr arg = checkExpr(ctx, ast.args[argIdx], common.expected);

		// If it failed to check, don't continue, just stop there.
		if (arg.typeIsBogus(ctx.arena())) {
			someArgIsBogus = true;
			return none<const Expr>();
		}

		// If the Inferring already came from the candidate, no need to do more work.
		if (!common.isExpectedFromCandidate) {
			const Type actualArgType = common.expected.inferred();
			// Remove candidates that can't accept this as a param. Also does type argument inference on the candidate.
			filterUnordered(candidates, [&](Candidate& candidate) {
				const Type expectedArgType = getCandidateExpectedParameterType(ctx.arena(), candidate, argIdx);
				return matchTypesNoDiagnostic(ctx.arena(), expectedArgType, actualArgType, candidate.inferringTypeArgs());
			});
		}
		return some<const Expr>(arg);
	});

	const Arr<Candidate> candidatesArr = candidates.freeze();

	if (someArgIsBogus)
		return expected.bogus(range);

	if (mightBePropertyAccess && arity == 1 && args.has()) {
		// Might be a struct field access
		const Opt<const Expr::StructFieldAccess> sfa = tryGetStructFieldAccess(ctx, ast.funName, only(args.force()));
		if (sfa.has()) {
			if (!candidatesArr.isEmpty())
				todo<void>("ambiguous call vs property access");
			return expected.check(ctx, sfa.force().accessedFieldType(), Expr{range, sfa.force()});
		}
	}

	if (!args.has() || candidatesArr.size != 1) {
		if (candidatesArr.isEmpty())
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
