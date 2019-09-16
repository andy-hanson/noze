#include "./checkCall.h"

#include "../util/arrUtil.h"
#include "./checkExpr.h"

namespace {
	// If we call `foo: \x ...` where `foo` is a function that doesn't exist,
	// don't continue checking the lambda it in the hopes that it might have a property.
	const Bool exprMightHaveProperties(const ExprAst ast) {
		return ast.kind.match(
			[](const CallAst) { return True; },
			[](const CondAst e) {
				return _and(exprMightHaveProperties(*e.then), exprMightHaveProperties(*e.elze));
			},
			[](const CreateArrAst) { return True; },
			[](const CreateRecordAst) { return True; },
			[](const CreateRecordMultiLineAst) { return True; },
			[](const FunAsLambdaAst) { return False; },
			[](const IdentifierAst) { return True; },
			[](const LambdaAst) { return False; },
			[](const LetAst e) { return exprMightHaveProperties(*e.then); },
			[](const LiteralAst) { return True; },
			// TODO: check all branches
			[](const MatchAst) { return True; },
			// Always returns fut
			[](const SeqAst e) { return exprMightHaveProperties(*e.then); },
			[](const RecordFieldSetAst) { return False; },
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

	MutArr<Candidate> getInitialCandidates(
		ExprCtx* ctx,
		const Sym funName,
		const Arr<const Type> explicitTypeArgs,
		const size_t actualArity
	) {
		MutArr<Candidate> res {};
		eachFunInScope(ctx, funName, [&](const CalledDecl called) {
			const size_t nTypeParams = size(called.typeParams());
			if (arity(called) == actualArity &&
				(isEmpty(explicitTypeArgs) || nTypeParams == size(explicitTypeArgs))) {
				const Arr<SingleInferringType> inferringTypeArgs = fillArr<SingleInferringType>{}(
					ctx->arena(),
					nTypeParams, [&](const size_t i) {
						// InferringType for a type arg doesn't need a candidate;
						// that's for a (value) arg's expected type
						return SingleInferringType{isEmpty(explicitTypeArgs)
							? none<const Type>()
							: some<const Type>(at(explicitTypeArgs, i))};
					});
				push(ctx->arena(), &res, Candidate{called, inferringTypeArgs});
			}
		});
		return res;
	}

	const Arr<const CalledDecl> getAllCandidatesAsCalledDecls(ExprCtx* ctx, const Sym funName) {
		ArrBuilder<const CalledDecl> res {};
		eachFunInScope(ctx, funName, [&](const CalledDecl called) {
			add<const CalledDecl>(ctx->arena(), &res, called);
		});
		return finishArr(&res);
	}

	const Type getCandidateExpectedParameterTypeRecur(
		Arena* arena,
		const Candidate* candidate,
		const Type candidateParamType
	) {
		return candidateParamType.match(
			[](const Type::Bogus) {
				return Type{Type::Bogus{}};
			},
			[&](const TypeParam* p) {
				const Opt<SingleInferringType*> sit = tryGetTypeArgFromInferringTypeArgs(
					InferringTypeArgs{candidate->called.typeParams(), candidate->typeArgs}, p);
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

	const Type getCandidateExpectedParameterType(Arena* arena, const Candidate* candidate, const size_t argIdx) {
		return getCandidateExpectedParameterTypeRecur(arena, candidate, at(candidate->called.params(), argIdx).type);
	}

	struct CommonOverloadExpected {
		Expected expected;
		const Bool isExpectedFromCandidate;
	};

	CommonOverloadExpected getCommonOverloadParamExpected(
		Arena* arena,
		const Arr<Candidate> candidates,
		const size_t argIdx
	) {
		switch (size(candidates)) {
			case 0:
				return CommonOverloadExpected{Expected::infer(), False};
			case 1: {
				const Candidate* candidate = onlyPtr(candidates);
				const Type t = getCandidateExpectedParameterType(arena, candidate, argIdx);
				return CommonOverloadExpected{Expected{some<const Type>(t), candidate->inferringTypeArgs()}, True};
			}
			default:
				// For multiple candidates, only have an expected type if they have exactly the same param type
				Cell<const Opt<const Type>> expected { none<const Type>() };
				for (const Candidate* candidate : ptrsRange(candidates)) {
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

	const Opt<const Expr::RecordFieldAccess> tryGetRecordFieldAccess(ExprCtx* ctx, const Sym funName, const Expr arg) {
		const Opt<const StructAndField> field = tryGetRecordField(arg.getType(ctx->arena(), ctx->commonTypes), funName);
		return has(field)
			? some<const Expr::RecordFieldAccess>(Expr::RecordFieldAccess{
				ctx->alloc(arg),
				force(field).structInst,
				force(field).field})
			: none<const Expr::RecordFieldAccess>();
	}

	const Opt<const Diag::CantCall::Reason> getCantCallReason(const FunFlags calledFlags, const FunFlags callerFlags) {
		if (!calledFlags.noCtx && callerFlags.noCtx)
			return some<const Diag::CantCall::Reason>(Diag::CantCall::Reason::nonNoCtx);
		else if (calledFlags.summon && !callerFlags.summon)
			return some<const Diag::CantCall::Reason>(Diag::CantCall::Reason::summon);
		else if (calledFlags.unsafe && !callerFlags.trusted && !callerFlags.unsafe)
			return some<const Diag::CantCall::Reason>(Diag::CantCall::Reason::unsafe);
		else
			return none<const Diag::CantCall::Reason>();
	}

	void checkCallFlags(
		CheckCtx* ctx,
		const SourceRange range,
		const FunDecl* called,
		const FunDecl* caller
	) {
		const Opt<const Diag::CantCall::Reason> reason = getCantCallReason(called->flags, caller->flags);
		if (has(reason))
			addDiag(ctx, range, Diag{Diag::CantCall{force(reason), called, caller}});
	}

	template <typename TypeArgsEqual>
	const Bool structInstsEqual(const StructInst* a, const StructInst* b, TypeArgsEqual typeArgsEqual) {
		return _and(
			ptrEquals(a->decl, b-> decl),
			eachCorresponds(a->typeArgs, b->typeArgs, typeArgsEqual));
	}

	void checkCalledDeclFlags(ExprCtx* ctx, const CalledDecl res, const SourceRange range) {
		res.match(
			[&](const FunDecl* f) {
				checkCallFlags(ctx->checkCtx, range, f, ctx->outermostFun);
			},
			[](const SpecSig) {
				// For a spec, we check the flags when providing the spec impl
			});
	}

	void filterByReturnType(Arena* arena, MutArr<Candidate>* candidates, const Type expectedReturnType) {
		// Filter by return type. Also does type argument inference on the candidate.
		filterUnordered(candidates, [&](Candidate* candidate) {
			return matchTypesNoDiagnostic(
				arena,
				candidate->called.returnType(),
				expectedReturnType,
				candidate->inferringTypeArgs(),
				/*allowConvertAToBUnion*/ True);
		});
	}

	void filterByParamType(Arena* arena, MutArr<Candidate>* candidates, const Type actualArgType, const size_t argIdx) {
		// Remove candidates that can't accept this as a param. Also does type argument inference on the candidate.
		filterUnordered(candidates, [&](Candidate* candidate) {
			const Type expectedArgType = getCandidateExpectedParameterType(arena, candidate, argIdx);
			return matchTypesNoDiagnostic(arena, expectedArgType, actualArgType, candidate->inferringTypeArgs());
		});
	}

	const Opt<const Called> getCalledFromCandidate(
		ExprCtx* ctx,
		const SourceRange range,
		const Candidate candidate,
		const bool allowSpecs);

	const Opt<const Called> findSpecSigImplementation(ExprCtx* ctx, const SourceRange range, const Sig specSig) {
		MutArr<Candidate> candidates = getInitialCandidates(ctx, specSig.name, emptyArr<const Type>(), arity(specSig));
		filterByReturnType(ctx->arena(), &candidates, specSig.returnType);
		for (const size_t argIdx : Range{arity(specSig)})
			filterByParamType(ctx->arena(), &candidates, at(specSig.params, argIdx).type, argIdx);

		// If any candidates left take specs -- leave as a TODO
		const Arr<const Candidate> candidatesArr = asConstArr<Candidate>(freeze(&candidates));
		switch (size(candidatesArr)) {
			case 0:
				ctx->addDiag(range, Diag{Diag::SpecImplNotFound{specSig.name}});
				return none<const Called>();
			case 1:
				return getCalledFromCandidate(ctx, range, only(candidatesArr), /*allowSpecs*/ false);
			default:
				todo<void>("diagnostic: multiple functions satisfy the spec");
				return none<const Called>();
		}
	}

	// See if e.g. 'data<?t>' is declared on this function.
	const Bool findBuiltinSpecOnType(
		ExprCtx* ctx,
		const SpecBody::Builtin::Kind kind,
		const Type type
	) {
		return exists(ctx->outermostFun->specs, [&](const SpecInst* inst) {
			return inst->body.match(
				[&](const SpecBody::Builtin b) {
					return _and(
						enumEq(b.kind, kind),
						typeEquals(only(inst->typeArgs), type));
				},
				[](const Arr<const Sig>) {
					//TODO: s might inherit from builtin spec?
					return False;
				});
		});
	}

	const Bool checkBuiltinSpec(
		ExprCtx* ctx,
		const FunDecl* called,
		const SourceRange range,
		const SpecBody::Builtin::Kind kind,
		const Type typeArg
	) {
		// TODO: Instead of worstCasePurity(), it type is a type parameter,
		// see if the current function has its own spec requiring that it be data / send
		const Bool typeIsGood = _or(
			[&]() {
				switch (kind) {
					case SpecBody::Builtin::Kind::data:
						return enumEq(typeArg.worstCasePurity(), Purity::data);
					case SpecBody::Builtin::Kind::send:
						return isDataOrSendable(typeArg.worstCasePurity());
					default:
						return unreachable<const Bool>();
				}
			}(),
			findBuiltinSpecOnType(ctx, kind, typeArg));
		if (!typeIsGood)
			ctx->addDiag(range, Diag{Diag::SpecBuiltinNotSatisfied{kind, typeArg, called}});
		return typeIsGood;
	}

	// On failure, returns none.
	const Opt<const Arr<const Called>> checkSpecImpls(
		ExprCtx* ctx,
		const SourceRange range,
		const FunDecl* called,
		const Arr<const Type> typeArgs,
		const bool allowSpecs
	) {
		// We store the impls in a flat array. Calculate the size ahead of time.
		const size_t nImpls = sum(called->specs, [](const SpecInst* specInst) {
			return specInst->body.nSigs();
		});
		if (nImpls != 0 && !allowSpecs) {
			ctx->addDiag(range, Diag{Diag::SpecImplHasSpecs{}});
			return none<const Arr<const Called>>();
		} else {
			MutArr<const Called> res = newUninitializedMutArr<const Called>(ctx->arena(), nImpls);
			size_t outI = 0;
			Cell<const Bool> allSucceeded = Cell<const Bool>{True};
			for (const SpecInst* specInst : called->specs) {
				// Note: specInst was instantiated potentialyl based on f's params.
				// Meed to instantiate it again.
				const SpecInst* specInstInstantiated = instantiateSpecInst(
					ctx->arena(),
					specInst,
					TypeParamsAndArgs{called->typeParams, typeArgs});
				const Bool succeeded = specInstInstantiated->body.match(
					[&](const SpecBody::Builtin b) {
						return checkBuiltinSpec(ctx, called, range, b.kind, only(specInstInstantiated->typeArgs));
					},
					[&](const Arr<const Sig> sigs) {
						for (const Sig sig : sigs) {
							const Opt<const Called> impl = findSpecSigImplementation(ctx, range, sig);
							if (!has(impl))
								return False;
							setAt<const Called>(&res, outI, force(impl));
							outI++;
						}
						return True;
					});
				if (!succeeded)
					cellSet<const Bool>(&allSucceeded, False);
			}
			if (cellGet(&allSucceeded)) {
				assert(outI == nImpls);
				return some<const Arr<const Called>>(freeze(&res));
			} else
				return none<const Arr<const Called>>();
		}
	}

	const Opt<const Arr<const Type>> finishCandidateTypeArgs(
		ExprCtx* ctx,
		const SourceRange range,
		const Candidate candidate
	) {
		const Opt<const Arr<const Type>> res = mapPtrsOrNone<const Type>{}(
			ctx->arena(),
			candidate.typeArgs,
			[](const SingleInferringType* i) {
				return i->tryGetInferred();
			});
		if (!has(res))
			ctx->addDiag(range, Diag{Diag::CantInferTypeArguments{}});
		return res;
	}

	const Opt<const Called> getCalledFromCandidate(
		ExprCtx* ctx,
		const SourceRange range,
		const Candidate candidate,
		const bool allowSpecs
	) {
		checkCalledDeclFlags(ctx, candidate.called, range);
		const Opt<const Arr<const Type>> candidateTypeArgs = finishCandidateTypeArgs(ctx, range, candidate);
		if (has(candidateTypeArgs)) {
			const Arr<const Type> typeArgs = force(candidateTypeArgs);
			return candidate.called.match(
				[&](const FunDecl* f) {
					const Opt<const Arr<const Called>> specImpls = checkSpecImpls(ctx, range, f, typeArgs, allowSpecs);
					if (has(specImpls))
						return some<const Called>(Called{instantiateFun(ctx->arena(), f, typeArgs, force(specImpls))});
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
		ExprCtx* ctx,
		const Candidate candidate,
		const SourceRange range,
		const Arr<const Expr> args,
		Expected* expected
	) {
		const Opt<const Called> opCalled = getCalledFromCandidate(ctx, range, candidate, /*allowSpecs*/ true);
		if (has(opCalled)) {
			const Called called = force(opCalled);
			//TODO: PERF second return type check may be unnecessary
			// if we already filtered by return type at the beginning
			return check(ctx, expected, called.returnType(), Expr{range, Expr::Call{called, args}});
		}
		else
			return bogus(expected, range);
	}
}

const CheckedExpr checkCall(ExprCtx* ctx, const SourceRange range, const CallAst ast, Expected* expected) {
	const Sym funName = ast.funName;
	const size_t arity = size(ast.args);

	const Arr<const Type> explicitTypeArgs = typeArgsFromAsts(ctx, ast.typeArgs);
	MutArr<Candidate> candidates = getInitialCandidates(ctx, funName, explicitTypeArgs, arity);
	// TODO: may not need to be deeply instantiated to do useful filtering here
	const Opt<const Type> expectedReturnType = tryGetDeeplyInstantiatedType(ctx->arena(), expected);
	if (has(expectedReturnType))
		filterByReturnType(ctx->arena(), &candidates, force(expectedReturnType));

	// Try to determine if the expression can have properties, without type-checking it.
	const Bool mightBePropertyAccess = _and3(
		arity == 1,
		exprMightHaveProperties(only(ast.args)),
		mutSymSetHas(&ctx->programState()->recordFieldNames, funName));

	ArrBuilder<const Type> actualArgTypes;

	Cell<const Bool> someArgIsBogus { False };
	const Opt<const Arr<const Expr>> args = fillArrOrFail<const Expr>{}(ctx->arena(), arity, [&](const size_t argIdx) {
		if (mutArrIsEmpty(&candidates) && !mightBePropertyAccess)
			// Already certainly failed.
			return none<const Expr>();

		CommonOverloadExpected common = mightBePropertyAccess
			? CommonOverloadExpected{Expected::infer(), False}
			: getCommonOverloadParamExpected(ctx->arena(), tempAsArr(&candidates), argIdx);
		Expr arg = checkExpr(ctx, at(ast.args, argIdx), &common.expected);

		// If it failed to check, don't continue, just stop there.
		if (arg.typeIsBogus(ctx->arena())) {
			cellSet<const Bool>(&someArgIsBogus, True);
			return none<const Expr>();
		}

		const Type actualArgType = inferred(&common.expected);
		add<const Type>(ctx->arena(), &actualArgTypes, actualArgType);
		// If the Inferring already came from the candidate, no need to do more work.
		if (!common.isExpectedFromCandidate)
			filterByParamType(ctx->arena(), &candidates, actualArgType, argIdx);
		return some<const Expr>(arg);
	});

	if (cellGet(&someArgIsBogus))
		return bogus(expected, range);

	const Arr<const Candidate> candidatesArr = asConstArr<Candidate>(freeze(&candidates));

	if (mightBePropertyAccess && arity == 1 && has(args)) {
		// Might be a struct field access
		const Opt<const Expr::RecordFieldAccess> sfa = tryGetRecordFieldAccess(ctx, funName, only(force(args)));
		if (has(sfa)) {
			if (!isEmpty(candidatesArr))
				todo<void>("ambiguous call vs property access");
			return check(ctx, expected, force(sfa).accessedFieldType(), Expr{range, force(sfa)});
		}
	}

	if (!has(args) || size(candidatesArr) != 1) {
		if (isEmpty(candidatesArr)) {
			const Arr<const CalledDecl> allCandidates = getAllCandidatesAsCalledDecls(ctx, funName);
			ctx->addDiag(
				range,
				Diag{Diag::CallNoMatch{
					funName,
					expectedReturnType,
					arity,
					finishArr(&actualArgTypes), allCandidates}});
		} else {
			const Arr<const CalledDecl> matches = map<const CalledDecl>{}(
				ctx->arena(),
				candidatesArr,
				[](const Candidate c) { return c.called; });
			ctx->addDiag(range, Diag{Diag::CallMultipleMatches{funName, matches}});
		}
		return bogus(expected, range);
	} else
		return checkCallAfterChoosingOverload(ctx, only(candidatesArr), range, force(args), expected);
}
