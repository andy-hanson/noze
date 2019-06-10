#pragma once

#include "../model.h"
#include "./typeFromAst.h" // makeFutType

struct NewAndMessageInfo {
	const Arr<const Param> instantiatedParams;
	const Arr<const Expr::NewIfaceImpl::Field> fields;
	const Message* message;
};

struct LambdaInfo {
	const Arr<const Param> lambdaParams;
	MutArr<const Local*> locals = MutArr<const Local*>{};
	MutArr<const ClosureField*> closureFields = MutArr<const ClosureField*>{};
};

struct ExprContext {
	ExprContext(const ExprContext&) = delete;

	CheckCtx& checkCtx;
	const StructsAndAliasesMap structsAndAliasesMap;
	const FunsMap funsMap;
	const CommonTypes& commonTypes;
	const FunDecl* outermostFun;
	const Opt<const NewAndMessageInfo> newAndMessageInfo;

	// Locals of the function or message. Lambda locals are stored in the lambda.
	// (Note the Let stores the local and this points to that.)
	MutArr<const Local*> messageOrFunctionLocals = MutArr<const Local*>{};
	// Note: if inside newAndMessageInfo, these are only the lambdas *inside* of it.
	// TODO: these are pointers because MutArr currently only works on copyable values, and LambdaInfo should not be copied.
	MutArr<LambdaInfo*> lambdas = MutArr<LambdaInfo*>{};

	inline const Str copyStr(const Str s) {
		return checkCtx.copyStr(s);
	}

	inline Arena& arena() {
		return checkCtx.arena;
	}

	inline void diag(const SourceRange range, const Diag diag) {
		checkCtx.diag(range, diag);
	}

	inline Type makeFutType(const Type type) {
		return ::makeFutType(arena(), commonTypes, type);
	}

	inline const Expr* alloc(const Expr e) {
		return arena().nu<const Expr>()(e);
	}
};

inline const Type typeFromAst(ExprContext& ctx, const TypeAst typeAst) {
	return typeFromAst(ctx.checkCtx, typeAst, ctx.structsAndAliasesMap, TypeParamsScope{ctx.outermostFun->typeParams}, none<MutArr<StructInst*>&>());
}

struct SingleInferringType {
private:
	Cell<const Opt<const Type>> type;

public:
	inline SingleInferringType(const Opt<const Type> t) : type{t} {}

	inline const Opt<const Type> tryGetInferred() const {
		return type.get();
	}

	// Note: this may infer type parameters.
	bool setTypeNoDiagnostic(Arena& arena, const Type setType);
	const Opt<const Type> setTypeNoDiagnosticWorker(Arena& arena, const Type setType);
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
	return CheckedExpr{Expr::bogus(range)};
}

bool matchTypesNoDiagnostic(Arena& arena, const Type expectedType, const Type setTYpe, InferringTypeArgs inferringTypeArgs);

struct Expected {
private:
	Cell<const Opt<const Type>> type;
	InferringTypeArgs inferringTypeArgs;

public:
	inline Expected(const Opt<const Type> init)
		: type{init}, inferringTypeArgs{InferringTypeArgs::none()} {}
	inline Expected(const Opt<const Type> init, InferringTypeArgs ita)
		: type{init}, inferringTypeArgs{ita} {}

	// TODO: if we have a bogus expected type we should probably not be doing any more checking at all?
	inline bool isBogus() const {
		return type.get().has() && type.get().force().isBogus();
	}

	static inline Expected infer() {
		return Expected{none<const Type>()};
	}

	inline Expected copyWithNewExpectedType(const Type type) {
		return Expected{some<const Type>(type), inferringTypeArgs};
	}

	const Opt<const Type> shallowInstantiateType() const;
	inline const Opt<const Type> tryGetDeeplyInstantiatedType(Arena& arena) const {
		return type.get().has() ? tryGetDeeplyInstantiatedTypeFor(arena, type.get().force()) : none<const Type>();
	}
	const Opt<const Type> tryGetDeeplyInstantiatedTypeFor(Arena& arena, const Type t) const;

	inline bool hasExpected() const {
		return type.get().has();
	}

	inline const CheckedExpr bogus(const SourceRange range) {
		type.set(some<const Type>(Type::bogus()));
		return bogusWithoutAffectingExpected(range);
	}

	inline const Type inferred() const {
		return type.get().force();
	}

	inline const Opt<const Type> tryGetInferred() const {
		return type.get();
	}

	inline bool isExpectingString(const StructInst* stringType) const {
		return type.get().has() && typeEquals(type.get().force(), Type{stringType});
	}

	const CheckedExpr check(ExprContext& ctx, const Type exprType, const Expr expr);

	// Note: this may infer type parameters
	bool setTypeNoDiagnostic(Arena& arena, const Type setType);
	const Opt<const Type> setTypeNoDiagnosticWorker(Arena& arena, const Type setType);
};
