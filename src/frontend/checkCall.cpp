#include "./checkCall.h"

#include "./checkExpr.h"

namespace {
	const Opt<SingleInferringType*> tryGetTypeArg(InferringTypeArgs inferringTypeArgs, const TypeParam* typeParam) {
		return tryGetTypeArg(inferringTypeArgs.params, inferringTypeArgs.args, typeParam);
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

	bool matchActualAndCandidateTypeForReturn(Arena& arena, const Type expectedReturnType, const Type declaredReturnType, const Candidate& candidate) {
		unused(arena, expectedReturnType, declaredReturnType, candidate);
		return todo<bool>("match");
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

		// If it failed to cehck, don't continue, just stop there.
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
	unused(args, range);
	return todo<const CheckedExpr>("!!!");
}
