#include "./checkCall.h"

#include "../util/arrUtil.h"
#include "./checkExpr.h"

namespace {
	const Opt<SingleInferringType*> tryGetTypeArg(InferringTypeArgs inferringTypeArgs, const TypeParam* typeParam) {
		return tryGetTypeArg(inferringTypeArgs.params, inferringTypeArgs.args, typeParam);
	}

	const Arr<const Type> typeArgsFromAsts(ExprCtx& ctx, const Arr<const TypeAst> typeAsts) {
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

	MutArr<Candidate> getInitialCandidates(ExprCtx& ctx, const Sym funName, const Arr<const Type> explicitTypeArgs, const size_t actualArity) {
		MutArr<Candidate> res {};
		eachFunInScope(ctx, funName, [&](const CalledDecl called) {
			const size_t nTypeParams = called.typeParams().size;
			if (arity(called) == actualArity &&
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

	const Arr<const CalledDecl> getAllCandidatesAsCalledDecls(ExprCtx& ctx, const Sym funName) {
		ArrBuilder<const CalledDecl> res {};
		eachFunInScope(ctx, funName, [&](const CalledDecl called) {
			add<const CalledDecl>(ctx.arena(), &res, called);
		});
		return finishArr(&res);
	}

	const Type getCandidateExpectedParameterTypeRecur(Arena* arena, const Candidate& candidate, const Type candidateParamType) {
		return candidateParamType.match(
			[](const Type::Bogus) {
				return Type{Type::Bogus{}};
			},
			[&](const TypeParam* p) {
				const Opt<SingleInferringType*> sit = tryGetTypeArg(InferringTypeArgs{candidate.called.typeParams(), candidate.typeArgs}, p);
				const Opt<const Type> inferred = has(sit) ? force(sit)->tryGetInferred() : none<const Type>();
				return has(inferred) ? force(inferred) : Type{p};
			},
			[&](const StructInst* i) {
				//TODO:PERF, the map might change nothing, so don't reallocate in that situation
				const Arr<const Type> typeArgs = map<const Type>{}(arena, i->typeArgs, [&](const Type t) {
					return getCandidateExpectedParameterTypeRecur(arena, candidate, t);
				});
				return Type{instantiateStructNeverDelay(arena, i->decl, typeArgs)};
			});
	}

	const Type getCandidateExpectedParameterType(Arena* arena, const Candidate& candidate, const size_t argIdx) {
		return getCandidateExpectedParameterTypeRecur(arena, candidate, at(candidate.called.params(), argIdx).type);
	}

	struct CommonOverloadExpected {
		Expected expected;
		const Bool isExpectedFromCandidate;
	};

	CommonOverloadExpected getCommonOverloadParamExpected(Arena* arena, const Arr<Candidate> candidates, const size_t argIdx) {
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
					// If we get a template candidate and haven't inferred this param type yet, no expected type.
					const Type paramType = getCandidateExpectedParameterType(arena, candidate, argIdx);
					if (has(cellGet(&expected))) {
						if (!typeEquals(paramType, force(cellGet(&expected))))
							// Only get an expected type if all candidates expect it.
							return CommonOverloadExpected{Expected::infer(), False};
					} else
						cellSet<const Opt<const Type>>(&expected, some<const Type>(paramType));
				}
				// Can't be inferring type arguments for candidates if there's more than one.
				// (Handle that *after* getting the arg type.)
				return CommonOverloadExpected{Expected{cellGet(&expected)}, False};
		}
	}

	const Opt<const Expr::StructFieldAccess> tryGetStructFieldAccess(ExprCtx& ctx, const Sym funName, const Expr arg) {
		const Opt<const StructAndField> field = tryGetStructField(arg.getType(ctx.arena(), ctx.commonTypes), funName);
		return has(field)
			? some<const Expr::StructFieldAccess>(Expr::StructFieldAccess{ctx.alloc(arg), force(field).structInst, force(field).field})
			: none<const Expr::StructFieldAccess>();
	}

	void checkCallFlags(CheckCtx& ctx, const SourceRange range, const FunFlags calledFlags, const FunFlags callerFlags) {
		if (!calledFlags.noCtx && callerFlags.noCtx)
			ctx.addDiag(range, Diag{Diag::CantCallNonNoCtx{}});
		if (calledFlags.summon && !callerFlags.summon)
			ctx.addDiag(range, Diag{Diag::CantCallSummon{}});
		if (calledFlags.unsafe && !callerFlags.trusted && !callerFlags.unsafe)
			ctx.addDiag(range, Diag{Diag::CantCallUnsafe{}});
	}

	template <typename TypeArgsEqual>
	const Bool structInstsEqual(const StructInst* a, const StructInst* b, TypeArgsEqual typeArgsEqual) {
		return _and(
			ptrEquals(a->decl, b-> decl),
			eachCorresponds(a->typeArgs, b->typeArgs, typeArgsEqual));
	}

	void checkCalledDeclFlags(ExprCtx& ctx, const CalledDecl res, const SourceRange range) {
		res.match(
			[&](const FunDecl* f) {
				checkCallFlags(ctx.checkCtx, range, f->flags, ctx.outermostFun->flags);
			},
			[](const SpecSig) {
				// For a spec, we check the flags when providing the spec impl
			});
	}

	void filterByReturnType(Arena* arena, MutArr<Candidate>& candidates, const Type expectedReturnType) {
		// Filter by return type. Also does type argument inference on the candidate.
		filterUnordered(candidates, [&](Candidate& candidate) {
			return matchTypesNoDiagnostic(
				arena,
				candidate.called.returnType(),
				expectedReturnType,
				candidate.inferringTypeArgs(),
				/*allowConvertAToBUnion*/ True);
		});
	}

	void filterByParamType(Arena* arena, MutArr<Candidate>& candidates, const Type actualArgType, const size_t argIdx) {
		// Remove candidates that can't accept this as a param. Also does type argument inference on the candidate.
		filterUnordered(candidates, [&](Candidate& candidate) {
			const Type expectedArgType = getCandidateExpectedParameterType(arena, candidate, argIdx);
			return matchTypesNoDiagnostic(arena, expectedArgType, actualArgType, candidate.inferringTypeArgs());
		});
	}

	const Opt<const Called> getCalledFromCandidate(ExprCtx& ctx, const SourceRange range, const Candidate candidate, const bool allowSpecs);

	const Opt<const Called> findSpecSigImplementation(ExprCtx& ctx, const SourceRange range, const Sig specSig) {
		MutArr<Candidate> candidates = getInitialCandidates(ctx, specSig.name, emptyArr<const Type>(), arity(specSig));
		filterByReturnType(ctx.arena(), candidates, specSig.returnType);
		for (const size_t argIdx : Range{arity(specSig)})
			filterByParamType(ctx.arena(), candidates, at(specSig.params, argIdx).type, argIdx);

		// If any candidates left take specs -- leave as a TODO
		const Arr<const Candidate> candidatesArr = asConstArr<Candidate>(freeze(candidates));
		switch (candidatesArr.size) {
			case 0:
				ctx.addDiag(range, Diag{Diag::SpecImplNotFound{specSig.name}});
				return none<const Called>();
			case 1:
				return getCalledFromCandidate(ctx, range, only(candidatesArr), /*allowSpecs*/ false);
			default:
				todo<void>("diagnostic: multiple functions satisfy the spec");
				return none<const Called>();
		}
	}

	// On failure, returns none.
	const Opt<const Arr<const Called>> checkSpecImpls(ExprCtx& ctx, const SourceRange range, const FunDecl* called, const Arr<const Type> typeArgs, const bool allowSpecs) {
		// We store the impls in a flat array. Calculate the size ahead of time.
		const size_t size = [&]() {
			size_t s = 0;
			for (const SpecInst* specInst : called->specs)
				s += specInst->sigs.size;
			return s;
		}();

		if (size != 0 && !allowSpecs) {
			ctx.addDiag(range, Diag{Diag::SpecImplHasSpecs{}});
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
					if (!has(impl))
						return none<const Arr<const Called>>();
					setAt<const Called>(res, outI, force(impl));
					outI++;
				}
			}
			assert(outI == size);
			return some<const Arr<const Called>>(freeze(res));
		}
	}

	const Opt<const Arr<const Type>> finishCandidateTypeArgs(ExprCtx& ctx, const SourceRange range, const Candidate candidate) {
		const Opt<const Arr<const Type>> res = mapPtrsOrNone<const Type>{}(
			ctx.arena(),
			candidate.typeArgs,
			[](const SingleInferringType* i) {
				return i->tryGetInferred();
			});
		if (!has(res))
			ctx.addDiag(range, Diag{Diag::CantInferTypeArguments{}});
		return res;
	}

	const Opt<const Called> getCalledFromCandidate(ExprCtx& ctx, const SourceRange range, const Candidate candidate, const bool allowSpecs) {
		checkCalledDeclFlags(ctx, candidate.called, range);
		const Opt<const Arr<const Type>> candidateTypeArgs = finishCandidateTypeArgs(ctx, range, candidate);
		if (has(candidateTypeArgs)) {
			const Arr<const Type> typeArgs = force(candidateTypeArgs);
			return candidate.called.match(
				[&](const FunDecl* f) {
					const Opt<const Arr<const Called>> specImpls = checkSpecImpls(ctx, range, f, typeArgs, allowSpecs);
					if (has(specImpls))
						return some<const Called>(Called{instantiateFun(ctx.arena(), f, typeArgs, force(specImpls))});
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
		ExprCtx& ctx,
		const Candidate candidate,
		const SourceRange range,
		const Arr<const Expr> args,
		Expected& expected
	) {
		const Opt<const Called> opCalled = getCalledFromCandidate(ctx, range, candidate, /*allowSpecs*/ true);
		if (has(opCalled)) {
			const Called called = force(opCalled);
			//TODO: PERF second return type check may be unnecessary if we already filtered by return type at the beginning
			return expected.check(ctx, called.returnType(), Expr{range, Expr::Call{called, args}});
		}
		else
			return expected.bogus(range);
	}
}

const CheckedExpr checkCall(ExprCtx& ctx, const SourceRange range, const CallAst ast, Expected& expected) {
	const Sym funName = ast.funName;
	const size_t arity = ast.args.size;

	const Bool mightBePropertyAccess = _and(arity == 1, exprMightHaveProperties(only(ast.args)));

	const Arr<const Type> explicitTypeArgs = typeArgsFromAsts(ctx, ast.typeArgs);
	MutArr<Candidate> candidates = getInitialCandidates(ctx, funName, explicitTypeArgs, arity);
	// TODO: may not need to be deeply instantiated to do useful filtering here
	const Opt<const Type> expectedReturnType = expected.tryGetDeeplyInstantiatedType(ctx.arena());
	if (has(expectedReturnType))
		filterByReturnType(ctx.arena(), candidates, force(expectedReturnType));

	ArrBuilder<const Type> actualArgTypes;

	Cell<const Bool> someArgIsBogus { False };
	const Opt<const Arr<const Expr>> args = fillArrOrFail<const Expr>{}(ctx.arena(), arity, [&](const size_t argIdx) {
		if (isEmpty(candidates) && !mightBePropertyAccess)
			// Already certainly failed.
			return none<const Expr>();

		CommonOverloadExpected common = getCommonOverloadParamExpected(ctx.arena(), tempAsArr(candidates), argIdx);
		Expr arg = checkExpr(ctx, at(ast.args, argIdx), common.expected);

		// If it failed to check, don't continue, just stop there.
		if (arg.typeIsBogus(ctx.arena())) {
			cellSet<const Bool>(&someArgIsBogus, True);
			return none<const Expr>();
		}

		const Type actualArgType = common.expected.inferred();
		add<const Type>(ctx.arena(), &actualArgTypes, actualArgType);
		// If the Inferring already came from the candidate, no need to do more work.
		if (!common.isExpectedFromCandidate)
			filterByParamType(ctx.arena(), candidates, actualArgType, argIdx);
		return some<const Expr>(arg);
	});

	if (cellGet(&someArgIsBogus))
		return expected.bogus(range);

	const Arr<const Candidate> candidatesArr = asConstArr<Candidate>(freeze(candidates));

	if (mightBePropertyAccess && arity == 1 && has(args)) {
		// Might be a struct field access
		const Opt<const Expr::StructFieldAccess> sfa = tryGetStructFieldAccess(ctx, funName, only(force(args)));
		if (has(sfa)) {
			if (!isEmpty(candidatesArr))
				todo<void>("ambiguous call vs property access");
			return expected.check(ctx, force(sfa).accessedFieldType(), Expr{range, force(sfa)});
		}
	}

	if (!has(args) || candidatesArr.size != 1) {
		if (isEmpty(candidatesArr)) {
			const Arr<const CalledDecl> allCandidates = getAllCandidatesAsCalledDecls(ctx, funName);
			ctx.addDiag(range, Diag{Diag::CallNoMatch{funName, expectedReturnType, arity, finishArr(&actualArgTypes), allCandidates}});
		} else {
			const Arr<const CalledDecl> matches = map<const CalledDecl>{}(
				ctx.arena(),
				candidatesArr,
				[](const Candidate c) { return c.called; });
			ctx.addDiag(range, Diag{Diag::CallMultipleMatches{funName, matches}});
		}
		return expected.bogus(range);
	} else
		return checkCallAfterChoosingOverload(ctx, only(candidatesArr), range, force(args), expected);
}
