#pragma once

#include "../model.h"
#include "./typeFromAst.h" // makeFutType

struct LambdaInfo {
	const Arr<const Param> lambdaParams;
	MutArr<const Local*> locals = MutArr<const Local*>{};
	MutArr<const ClosureField*> closureFields = MutArr<const ClosureField*>{};
};

struct ExprCtx {
	ExprCtx(const ExprCtx&) = delete;

	CheckCtx* checkCtx;
	const StructsAndAliasesMap structsAndAliasesMap;
	const FunsMap funsMap;
	const CommonTypes* commonTypes;
	const FunDecl* outermostFun;

	// Locals of the function or message. Lambda locals are stored in the lambda.
	// (Note the Let stores the local and this points to that.)
	MutArr<const Local*> messageOrFunctionLocals = MutArr<const Local*>{};
	// TODO: these are pointers because MutArr currently only works on copyable values,
	// and LambdaInfo should not be copied.
	MutArr<LambdaInfo*> lambdas = MutArr<LambdaInfo*>{};


	inline Arena* arena() {
		return checkCtx->arena;
	}

	inline void addDiag(const SourceRange range, const Diag diag) {
		::addDiag(checkCtx, range, diag);
	}

	inline Type makeFutType(const Type type) {
		return ::makeFutType(arena(), commonTypes, type);
	}

	inline const Expr* alloc(const Expr e) {
		return nu<const Expr>{}(arena(), e);
	}

	inline const ProgramState* programState() const {
		return checkCtx->programState;
	}
};

inline const Type typeFromAst(ExprCtx* ctx, const TypeAst typeAst) {
	return typeFromAst(
		ctx->checkCtx,
		typeAst,
		ctx->structsAndAliasesMap,
		TypeParamsScope{ctx->outermostFun->typeParams},
		none<MutArr<StructInst*>*>());
}

struct SingleInferringType {
	Cell<const Opt<const Type>> type;

	inline SingleInferringType(const Opt<const Type> t) : type{t} {}

	inline const Opt<const Type> tryGetInferred() const {
		return cellGet(&type);
	}
};

struct InferringTypeArgs {
	const Arr<const TypeParam> params;
	const Arr<SingleInferringType> args;

	static InferringTypeArgs none() {
		return InferringTypeArgs{emptyArr<const TypeParam>(), emptyArr<SingleInferringType>()};
	}
};

// Gets the type system to ensure that we set the expected type.
struct CheckedExpr {
	Expr expr;
};

inline const CheckedExpr bogusWithoutAffectingExpected(const SourceRange range) {
	return CheckedExpr{Expr{range, Expr::Bogus{}}};
}

// Inferring type args are in 'a', not 'b'
const Bool matchTypesNoDiagnostic(
	Arena* arena,
	const Type a,
	const Type b,
	const InferringTypeArgs aInferringTypeArgs,
	const Bool allowConvertAToBUnion);
inline const Bool matchTypesNoDiagnostic(
	Arena* arena,
	const Type a,
	const Type b,
	const InferringTypeArgs aInferringTypeArgs) {
	return matchTypesNoDiagnostic(arena, a, b, aInferringTypeArgs, /*allowConvertAToBUnion*/ False);
}

struct Expected {
	Cell<const Opt<const Type>> type;
	InferringTypeArgs inferringTypeArgs;

	inline Expected(const Opt<const Type> init)
		: type{init}, inferringTypeArgs{InferringTypeArgs::none()} {}
	inline Expected(const Opt<const Type> init, InferringTypeArgs ita)
		: type{init}, inferringTypeArgs{ita} {}

	static inline Expected infer() {
		return Expected{none<const Type>()};
	}
};

inline const Opt<const Type> tryGetInferred(const Expected* expected) {
	return cellGet(&expected->type);
}

// TODO: if we have a bogus expected type we should probably not be doing any more checking at all?
inline const Bool isBogus(const Expected* expected) {
	const Opt<const Type> t = tryGetInferred(expected);
	return _and(has(t), force(t).isBogus());
}

inline Expected copyWithNewExpectedType(const Expected* expected, const Type type) {
	return Expected{some<const Type>(type), expected->inferringTypeArgs};
}

const Opt<const Type> shallowInstantiateType(const Expected* expected);

const Opt<const Type> tryGetDeeplyInstantiatedTypeFor(Arena* arena, const Expected* expected, const Type t);

inline const Opt<const Type> tryGetDeeplyInstantiatedType(Arena* arena, const Expected* expected) {
	const Opt<const Type> t = tryGetInferred(expected);
	return has(t)
		? tryGetDeeplyInstantiatedTypeFor(arena, expected, force(t))
		: none<const Type>();
}

inline const Bool hasExpected(const Expected* expected) {
	return has(tryGetInferred(expected));
}

inline const CheckedExpr bogusWithType(Expected* expected, const SourceRange range, const Type setType) {
	cellSet<const Opt<const Type>>(&expected->type, some<const Type>(setType));
	return bogusWithoutAffectingExpected(range);
}

inline const CheckedExpr bogus(Expected* expected, const SourceRange range) {
	return bogusWithType(expected, range, Type{Type::Bogus{}});
}

inline const Type inferred(const Expected* expected) {
	return force(tryGetInferred(expected));
}

inline const Bool isExpectingString(const Expected* expected, const StructInst* stringType) {
	const Opt<const Type> t = tryGetInferred(expected);
	return _and(
		has(t),
		typeEquals(force(t), Type{stringType}));
}

const CheckedExpr check(ExprCtx* ctx, Expected* expected, const Type exprType, const Expr expr);

// Note: this may infer type parameters
const Bool setTypeNoDiagnostic(Arena* arena, Expected* expected, const Type setType);

struct StructAndField {
	const StructInst* structInst;
	const RecordField* field;
};
const Opt<const StructAndField> tryGetRecordField(const Type targetType, const Sym fieldName);

inline const Opt<SingleInferringType*> tryGetTypeArgFromInferringTypeArgs(
	const InferringTypeArgs inferringTypeArgs,
	const TypeParam* typeParam
) {
	return tryGetTypeArg<SingleInferringType>(inferringTypeArgs.params, inferringTypeArgs.args, typeParam);
}
