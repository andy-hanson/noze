#include "./inferringType.h"

#include "../util/arrUtil.h"

namespace {
	struct SetTypeResult {
		// set a new type
		struct Set {
			const Type type;
		};
		// keep the type as-is
		struct Keep {};
		// type error
		struct Fail {};

	private:
		enum class Kind {
			// set a new type
			set,
			// keep the type as-is
			keep,
			// type error
			fail,
		};
		const Kind kind;
		union {
			const Set set;
			const Keep keep;
			const Fail fail;
		};
	public:
		explicit inline SetTypeResult(const Set _set) : kind{Kind::set}, set{_set} {}
		explicit inline SetTypeResult(const Keep _keep) : kind{Kind::keep}, keep{_keep} {}
		explicit inline SetTypeResult(const Fail _fail) : kind{Kind::fail}, fail{_fail} {}

		template <
			typename CbSet,
			typename CbKeep,
			typename CbFail
		>
		inline auto match(
			CbSet cbSet,
			CbKeep cbKeep,
			CbFail cbFail
		) const {
			switch (kind) {
				case Kind::set:
					return cbSet(set);
				case Kind::keep:
					return cbKeep(keep);
				case Kind::fail:
					return cbFail(fail);
				default:
					assert(0);
			}
		}
	};

	// When matching a type, we may fill in type parameters, so we may want to set a new more specific expected type.
	const SetTypeResult setTypeNoDiagnosticWorkerWorkerWorker(
		Arena* arena,
		const Type a,
		const Type b,
		const InferringTypeArgs aInferringTypeArgs,
		const Bool allowConvertAToBUnion
	);

	const SetTypeResult setForStructInstsWithSameDecl(
		Arena* arena,
		const StructDecl* decl,
		const Arr<const Type> as,
		const Arr<const Type> bs,
		const InferringTypeArgs aInferringTypeArgs
	) {
		// If we need to set at least one type arg, return Set.
		// If all passed, return Keep.
		// Else, return Fail.
		Cell<const Bool> someIsSet { False };
		const Opt<const Arr<const Type>> newTypeArgs = zipOrFail<const Type>{}(
			arena,
			as,
			bs,
			[&](const Type a, const Type b) {
				const SetTypeResult res = setTypeNoDiagnosticWorkerWorkerWorker(arena, a, b, aInferringTypeArgs, False);
				return res.match(
					[&](const SetTypeResult::Set s) {
						cellSet<const Bool>(&someIsSet, True);
						return some<const Type>(s.type);
					},
					[&](const SetTypeResult::Keep) {
						return some<const Type>(a);
					},
					[](const SetTypeResult::Fail) {
						return none<const Type>();
					});
			});
		return has(newTypeArgs)
			? cellGet(&someIsSet)
				? SetTypeResult{SetTypeResult::Set{Type{instantiateStructNeverDelay(arena, decl, force(newTypeArgs))}}}
				: SetTypeResult{SetTypeResult::Keep{}}
			: SetTypeResult{SetTypeResult::Fail{}};
	}

	const SetTypeResult setTypeNoDiagnosticWorker_forStructInst(
		Arena* arena,
		const StructInst* a,
		const StructInst* b,
		const InferringTypeArgs aInferringTypeArgs,
		const Bool allowConvertAToBUnion
	) {
		// Handling a union expected type is done in Expected::check
		// TODO: but it's done here to for case of call return type ...
		if (ptrEquals(a->decl, b->decl))
			return setForStructInstsWithSameDecl(arena, a->decl, a->typeArgs, b->typeArgs, aInferringTypeArgs);
		else {
			const StructBody bBody = b->decl->body();
			if (allowConvertAToBUnion && bBody.isUnion()) {
				const Opt<const StructInst*> bMember = find(bBody.asUnion().members, [&](const StructInst* i) {
					return ptrEquals(i->decl, a->decl);
				});
				if (has(bMember))
					return setForStructInstsWithSameDecl(
						arena,
						a->decl,
						a->typeArgs,
						instantiateStructInst(arena, force(bMember), b)->typeArgs,
						aInferringTypeArgs);
				else
					return SetTypeResult{SetTypeResult::Fail{}};
			} else
				return SetTypeResult{SetTypeResult::Fail{}};
		}
	}

	inline const Opt<SingleInferringType*> tryGetTypeArg(const InferringTypeArgs inferringTypeArgs, const TypeParam* typeParam) {
		return ::tryGetTypeArg<SingleInferringType>(inferringTypeArgs.params, inferringTypeArgs.args, typeParam);
	}

	const Opt<const Type> tryGetDeeplyInstantiatedTypeWorker(Arena* arena, const Type t, const InferringTypeArgs inferringTypeArgs) {
		return t.match(
			[](const Type::Bogus) {
				return some<const Type>(Type{Type::Bogus{}});
			},
			[&](const TypeParam* p) {
				const Opt<SingleInferringType*> ta = tryGetTypeArg(inferringTypeArgs, p);
				// If it's not one of the inferring types, it's instantiated enough to return.
				return has(ta) ? force(ta)->tryGetInferred() : some<const Type>(t);
			},
			[&](const StructInst* i) {
				const Opt<const Arr<const Type>> typeArgs = mapOrNone<const Type>{}(arena, i->typeArgs, [&](const Type t) {
					return tryGetDeeplyInstantiatedTypeWorker(arena, t, inferringTypeArgs);
				});
				return has(typeArgs)
					? some<const Type>(Type{instantiateStructNeverDelay(arena, i->decl, force(typeArgs))})
					: none<const Type>();
			});
	}

	const SetTypeResult setTypeNoDiagnosticWorkerWorker(
		Arena* arena,
		const Opt<const Type> a,
		const Type b,
		const InferringTypeArgs aInferringTypeArgs
	) {
		return has(a)
			? setTypeNoDiagnosticWorkerWorkerWorker(arena, force(a), b, aInferringTypeArgs, False)
			: SetTypeResult{SetTypeResult::Set{b}};
	}

	const SetTypeResult setTypeNoDiagnosticWorker_forSingleInferringType(Arena* arena, SingleInferringType& sit, const Type setType) {
		const SetTypeResult res = setTypeNoDiagnosticWorkerWorker(arena, cellGet(&sit.type), setType, InferringTypeArgs::none());
		res.match(
			[&](const SetTypeResult::Set s) {
				cellSet<const Opt<const Type>>(&sit.type, some<const Type>(s.type));
			},
			[](const SetTypeResult::Keep) {},
			[](const SetTypeResult::Fail) {});
		return res;
	}

	// TODO:NAME
	// We are trying to assign 'a = b'.
	// 'a' may contain type parameters from inferringTypeArgs. We'll infer those here.
	// If 'allowConvertAToBUnion' is set, if 'b' is a union type and 'a' is a member, we'll set it to the union.
	const SetTypeResult setTypeNoDiagnosticWorkerWorkerWorker(
		Arena* arena,
		const Type a,
		const Type b,
		const InferringTypeArgs aInferringTypeArgs,
		const Bool allowConvertAToBUnion
	) {
		return a.match(
			[](const Type::Bogus) {
				// TODO: make sure to infer type params in this case!
				return SetTypeResult{SetTypeResult::Keep{}};
			},
			[&](const TypeParam* pa) {
				const Opt<SingleInferringType*> aInferring = tryGetTypeArg(aInferringTypeArgs, pa);
				return has(aInferring)
					? setTypeNoDiagnosticWorker_forSingleInferringType(arena, *force(aInferring), b)
					: b.match(
						[](const Type::Bogus) {
							return SetTypeResult{SetTypeResult::Keep{}};
						},
						[&](const TypeParam* pb) {
							return ptrEquals(pa, pb)
								? SetTypeResult{SetTypeResult::Keep{}}
								: SetTypeResult{SetTypeResult::Fail{}};
						},
						[](const StructInst*) {
							// Expecting a type param, got a particular type
							return SetTypeResult{SetTypeResult::Fail{}};
						});
			},
			[&](const StructInst* ai) {
				return b.isStructInst()
					? setTypeNoDiagnosticWorker_forStructInst(arena, ai, b.asStructInst(), aInferringTypeArgs, allowConvertAToBUnion)
					: SetTypeResult{SetTypeResult::Fail{}};
			});
	}

}

const Bool SingleInferringType::setTypeNoDiagnostic(Arena* arena, const Type setType) {
	return setTypeNoDiagnosticWorkerWorker(arena, cellGet(&type), setType, InferringTypeArgs::none()).match(
		[&](const SetTypeResult::Set s) {
			cellSet<const Opt<const Type>>(&type, some<const Type>(s.type));
			return True;
		},
		[](const SetTypeResult::Keep) {
			return True;
		},
		[](const SetTypeResult::Fail) {
			return False;
		});
}


const CheckedExpr check(ExprCtx* ctx, Expected* expected, const Type exprType, const Expr expr) {
	// Allow implicitly converting to union
	// TODO: implicitly convert to Fut by wrapping in 'resolved'
	const Opt<const Type> t = cellGet(&expected->type);
	if (has(t) && force(t).isStructInst() && exprType.isStructInst()) {
		const StructInst* expectedStruct = force(t).asStructInst();
		const StructInst* exprStruct = exprType.asStructInst();
		const StructBody body = expectedStruct->decl->body();
		if (body.isUnion()) {
			const Arr<const StructInst*> members = body.asUnion().members;
			// This is like 't' but with the union's type parameters
			const Opt<const size_t> opMemberIndex = findIndex(
				members,
				[&](const StructInst* it) {
					return ptrEquals(it->decl, exprStruct->decl);
				});
			if (has(opMemberIndex)) {
				const size_t memberIndex = force(opMemberIndex);
				const StructInst* instantiatedExpectedUnionMember = instantiateStructInst(
					ctx->arena(),
					at(members, memberIndex),
					expectedStruct);

				const SetTypeResult setTypeResult = setTypeNoDiagnosticWorker_forStructInst(
					ctx->arena(),
					instantiatedExpectedUnionMember,
					exprStruct,
					expected->inferringTypeArgs,
					/*allowConvertAToBUnion*/ False);

				return setTypeResult.match(
					[](const SetTypeResult::Set) {
						return todo<const CheckedExpr>("should never happen?");
					},
					[&](const SetTypeResult::Keep) {
						const Opt<const Type> opU = tryGetDeeplyInstantiatedType(ctx->arena(), expected);
						if (!has(opU))
							return todo<const CheckedExpr>("expected check -- not deeply instantiated");

						const Expr::ImplicitConvertToUnion toU {force(opU).asStructInst(), memberIndex, ctx->alloc(expr)};
						return CheckedExpr{Expr{expr.range(), toU}};
					},
					[&](const SetTypeResult::Fail) {
						ctx->addDiag(expr.range(), Diag{Diag::TypeConflict{force(t), exprType}});
						return CheckedExpr{Expr{expr.range(), Expr::Bogus{}}};
					});
			}
		}
	}

	if (setTypeNoDiagnostic(ctx->arena(), expected, exprType))
		return CheckedExpr{expr};
	else {
		// Failed to set type. This happens if there was already an inferred type.
		ctx->addDiag(expr.range(), Diag{Diag::TypeConflict{force(t), exprType}});
		return bogus(expected, expr.range());
	}
}

const Bool setTypeNoDiagnostic(Arena* arena, Expected* expected, const Type setType) {
	const SetTypeResult typeToSet = setTypeNoDiagnosticWorkerWorker(
		arena,
		cellGet(&expected->type),
		setType,
		expected->inferringTypeArgs);
	return typeToSet.match(
		[&](const SetTypeResult::Set s) {
			cellSet<const Opt<const Type>>(&expected->type, some<const Type>(s.type));
			return True;
		},
		[](const SetTypeResult::Keep) {
			return True;
		},
		[](const SetTypeResult::Fail) {
			return False;
		});
}

const Opt<const Type> shallowInstantiateType(const Expected* expected) {
	const Opt<const Type> t = cellGet(&expected->type);
	if (has(t) && force(t).isTypeParam()) {
		const Opt<SingleInferringType*> typeArg = tryGetTypeArg(expected->inferringTypeArgs, force(t).asTypeParam());
		return has(typeArg) ? force(typeArg)->tryGetInferred() : none<const Type>();
	} else
		return t;
}

const Opt<const Type> tryGetDeeplyInstantiatedTypeFor(Arena* arena, const Expected* expected, const Type t) {
	return tryGetDeeplyInstantiatedTypeWorker(arena, t, expected->inferringTypeArgs);
}

const Bool matchTypesNoDiagnostic(
	Arena* arena,
	const Type expectedType,
	const Type setType,
	const InferringTypeArgs inferringTypeArgs,
	const Bool allowConvertToUnion
) {
	return setTypeNoDiagnosticWorkerWorkerWorker(arena, expectedType, setType, inferringTypeArgs, allowConvertToUnion).match(
		[](const SetTypeResult::Set) {
			return True;
		},
		[](const SetTypeResult::Keep) {
			return True;
		},
		[](const SetTypeResult::Fail) {
			return False;
		});
}

const Opt<const StructAndField> tryGetStructField(const Type targetType, const Sym fieldName) {
	return targetType.match(
		[](const Type::Bogus) {
			//TODO: want to avoid cascading errors here.
			return none<const StructAndField>();
		},
		[](const TypeParam*) {
			return none<const StructAndField>();
		},
		[&](const StructInst* targetStructInst) {
			return targetStructInst->body().match(
				[](const StructBody::Bogus) {
					return none<const StructAndField>();
				},
				[](const StructBody::Builtin) {
					return none<const StructAndField>();
				},
				[&](const StructBody::Record r) {
					const Opt<const StructField*> field = findPtr(r.fields, [&](const StructField* f) {
						return symEq(f->name, fieldName);
					});
					return has(field)
						? some<const StructAndField>(StructAndField{targetStructInst, force(field)})
						: none<const StructAndField>();
				},
				[](const StructBody::Union) {
					return none<const StructAndField>();
				},
				[](const StructBody::Iface) {
					return none<const StructAndField>();
				});
		});
}
