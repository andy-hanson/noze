#include "./inferringType.h"

namespace {
	const Opt<const Type> setTypeNoDiagnosticWorkerWorker(
		Arena& arena,
		const Opt<const Type> expectedType,
		const Type setType,
		InferringTypeArgs& inferringTypeArgs
	);

	const Opt<const Type> setTypeNoDiagnosticWorker_forStructInst(
		Arena& arena,
		const StructInst* expectedType,
		const StructInst* setType,
		InferringTypeArgs& inferringTypeArgs
	) {
		if (ptrEquals(setType->decl, expectedType->decl)) {
			const Opt<const Arr<const Type>> newTypeArgs = zipOrFail<const Type>{}(
				arena,
				expectedType->typeArgs,
				setType->typeArgs,
				[&](const Type expectedTypeArg, const Type setTypeArg) {
					return setTypeNoDiagnosticWorkerWorker(arena, some<const Type>(expectedTypeArg), setTypeArg, inferringTypeArgs);
				});
			return newTypeArgs.has()
				? some<const Type>(Type{instantiateStructNeverDelay(arena, setType->decl, newTypeArgs.force())})
				: none<const Type>();
		} else
			return none<const Type>();
	}

	inline Opt<SingleInferringType*> tryGetTypeArg(InferringTypeArgs& inferringTypeArgs, const TypeParam* typeParam) {
		return ::tryGetTypeArg<SingleInferringType>(inferringTypeArgs.params, inferringTypeArgs.args, typeParam);
	}
	inline Opt<const SingleInferringType*> tryGetTypeArg(const InferringTypeArgs& inferringTypeArgs, const TypeParam* typeParam) {
		return ::tryGetTypeArg<const SingleInferringType>(inferringTypeArgs.params, inferringTypeArgs.args.asConst(), typeParam);
	}

	const Opt<const Type> tryGetDeeplyInstantiatedTypeWorker(Arena& arena, const Type t, const InferringTypeArgs& inferringTypeArgs) {
		return t.match(
			/*bogus*/ [&]() {
				return some<const Type>(t);
			},
			[&](const TypeParam* p) {
				const Opt<const SingleInferringType*> ta = tryGetTypeArg(inferringTypeArgs, p);
				// If it's not one of the inferring types, it's instantiated enough to return.
				return ta.has() ? ta.force()->tryGetInferred() : some<const Type>(t);
			},
			[&](const StructInst* i) {
				const Opt<const Arr<const Type>> typeArgs = mapOrNone<const Type>{}(arena, i->typeArgs, [&](const Type t) {
					return tryGetDeeplyInstantiatedTypeWorker(arena, t, inferringTypeArgs);
				});
				return typeArgs.has()
					? some<const Type>(Type{instantiateStructNeverDelay(arena, i->decl, typeArgs.force())})
					: none<const Type>();
			});
	}

	const Opt<const Type> setTypeNoDiagnosticWorkerWorker(
		Arena& arena,
		const Opt<const Type> expectedType,
		const Type setType,
		InferringTypeArgs& inferringTypeArgs
	) {
		if (expectedType.has())
			return expectedType.force().match(
				/*bogus*/ []() {
					return some<const Type>(Type::bogus());
				},
				[&](const TypeParam* p) {
					if (setType.isTypeParam() && ptrEquals(setType.asTypeParam(), p))
						// Setting a type parameter to itself
						return some<const Type>(setType);
					else {
						const Opt<SingleInferringType*> it = tryGetTypeArg(inferringTypeArgs, p);
						return it.has()
							? it.force()->setTypeNoDiagnosticWorker(arena, setType)
							: none<const Type>();
					}
				},
				[&](const StructInst* i) {
					return setType.isStructInst()
						? setTypeNoDiagnosticWorker_forStructInst(arena, i, setType.asStructInst(), inferringTypeArgs)
						: none<const Type>();
				});
		else
			return some<const Type>(setType);
	}
}

bool SingleInferringType::setTypeNoDiagnostic(Arena& arena, const Type setType) {
	InferringTypeArgs ita = InferringTypeArgs::none();
	const Opt<const Type> typeToSet = setTypeNoDiagnosticWorkerWorker(arena, type.get(), setType, ita);
	if (typeToSet.has()) {
		type.set(typeToSet);
		return true;
	} else
		return false;
}

const Opt<const Type> SingleInferringType::setTypeNoDiagnosticWorker(Arena& arena, const Type setType) {
	InferringTypeArgs ita = InferringTypeArgs::none();
	const Opt<const Type> res = setTypeNoDiagnosticWorkerWorker(arena, type.get(), setType, ita);
	if (res.has())
		type.set(res);
	return res;
}

const CheckedExpr Expected::check(ExprContext& ctx, const Type exprType, const Expr expr) {
	// Allow implicitly converting to union
	// TODO: implicitly convert to Fut by wrapping in 'resolved'
	if (type.get().has() && type.get().force().isStructInst() && exprType.isStructInst()) {
		const StructInst* expectedStruct = type.get().force().asStructInst();
		const StructInst* exprStruct = exprType.asStructInst();
		const StructBody body = expectedStruct->decl->body();
		if (body.isUnion()) {
			// This is like 't' but with the union's type parameters
			const Opt<const StructInst*> matching = find(
				body.asUnion().members,
				[&](const StructInst* it) {
					return ptrEquals(it->decl, exprStruct->decl);
				});
			if (matching.has()) {
				// Fill in type args
				const Opt<const Type> x = setTypeNoDiagnosticWorker_forStructInst(
					ctx.arena(),
					instantiateStructInst(ctx.arena(), matching.force(), expectedStruct),
					exprStruct,
					inferringTypeArgs);
				if (!x.has())
					todo<void>("expected check");
				const Opt<const Type> opU = tryGetDeeplyInstantiatedType(ctx.arena());
				if (!opU.has())
					todo<void>("expected check");
				const Expr::ImplicitConvertToUnion toU {opU.force().asStructInst(), exprStruct, ctx.alloc(expr)};
				return CheckedExpr{Expr{expr.range(), toU}};
			}
		}
	}

	if (setTypeNoDiagnostic(ctx.arena(), exprType))
		return CheckedExpr{expr};
	else {
		// Failed to set type. This happens if there was already an inferred type.
		ctx.diag(expr.range(), Diag{Diag::TypeConflict{type.get().force(), exprType}});
		return bogus(expr.range());
	}
}

bool Expected::setTypeNoDiagnostic(Arena& arena, const Type setType) {
	const Opt<const Type> typeToSet = setTypeNoDiagnosticWorkerWorker(arena, type.get(), setType, inferringTypeArgs);
	if (typeToSet.has()) {
		type.set(typeToSet);
		return true;
	} else
		return false;
}

const Opt<const Type> Expected::setTypeNoDiagnosticWorker(Arena& arena, const Type setType) {
	const Opt<const Type> res = setTypeNoDiagnosticWorkerWorker(arena, type.get(), setType, inferringTypeArgs);
	if (res.has())
		type.set(res);
	return res;
}

const Opt<const Type> Expected::shallowInstantiateType() const {
	const Opt<const Type> t = type.get();
	if (t.has() && t.force().isTypeParam()) {
		const Opt<const SingleInferringType*> typeArg = tryGetTypeArg(inferringTypeArgs, t.force().asTypeParam());
		return typeArg.has() ? typeArg.force()->tryGetInferred() : none<const Type>();
	} else
		return t;
}

const Opt<const Type> Expected::tryGetDeeplyInstantiatedTypeFor(Arena& arena, const Type t) const {
	return tryGetDeeplyInstantiatedTypeWorker(arena, t, inferringTypeArgs);
}

bool matchTypesNoDiagnostic(Arena& arena, const Type expectedType, const Type setType, InferringTypeArgs inferringTypeArgs) {
	return setTypeNoDiagnosticWorkerWorker(arena, some<const Type>(expectedType), setType, inferringTypeArgs).has();
}
