#pragma once

#include "../concreteModel.h"
#include "../model.h"

#include "./allConstants.h"

struct TypeArgsScope {
	/*
	Suppose we have:
	pair<?ts> record
		a ?ts
		b ?ts
	make-pair pair ?tf(value ?tf)
		new value, value
	*/
	// When we instantiate the struct 'pair' for the return type, and are getting the type for 'a':
	// The StructInst* for that will already have an instantiated fieldTypes containing ?tf and not ?ts.
	// So the only type params and args we need here come from the concretefun.
	// TODO:PERF no need to store typeParams then.
	const Arr<const TypeParam> typeParams;
	const Arr<const ConcreteType> typeArgs;

	inline TypeArgsScope(const Arr<const TypeParam> _typeParams, const Arr<const ConcreteType> _typeArgs)
		: typeParams{_typeParams}, typeArgs{_typeArgs} {
		assert(sizeEq(typeParams, typeArgs));
	}

	static inline TypeArgsScope empty() {
		return TypeArgsScope{emptyArr<const TypeParam>(), emptyArr<const ConcreteType>()};
	}
};

struct ConcreteStructKey {
	const StructDecl* decl;
	const Arr<const ConcreteType> typeArgs;
};

Comparison compareConcreteStructKey(const ConcreteStructKey a, const ConcreteStructKey b);

// Unlike FunInst, this uses ConcreteType for type args instead of Type.
// Unlike ConcreteFun, this doesn't have specialized params yet.
struct ConcreteFunInst {
	const FunDecl* decl;
	const Arr<const ConcreteType> typeArgs;
	const Arr<const ConcreteFunInst> specImpls;

	inline ConcreteFunInst(
		const FunDecl* _decl,
		const Arr<const ConcreteType> _typeArgs,
		const Arr<const ConcreteFunInst> _specImpls
	) : decl{_decl}, typeArgs{_typeArgs}, specImpls{_specImpls} {
		assert(sizeEq(typeArgs, decl->typeParams));
		assert(size(specImpls) == decl->nSpecImpls());
	}

	inline const TypeArgsScope typeArgsScope() const {
		return TypeArgsScope{decl->typeParams, typeArgs};
	}

	inline const ConcreteFunInst withTypeArgs(const Arr<const ConcreteType> newTypeArgs) const {
		return ConcreteFunInst{decl, newTypeArgs, specImpls};
	}
};

inline size_t arity(const ConcreteFunInst c) {
	return arity(c.decl);
}

struct ConcreteFunKey {
	const ConcreteFunInst funInst;
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs;

	inline ConcreteFunKey(
		const ConcreteFunInst _funInst,
		const Arr<const ConstantOrLambdaOrVariable> _specializeOnArgs
	) : funInst{_funInst}, specializeOnArgs{_specializeOnArgs} {
		assert(arity(funInst) == size(specializeOnArgs));
	}

	inline const FunDecl* decl() const {
		return funInst.decl;
	}

	inline const TypeArgsScope typeArgsScope() const {
		return funInst.typeArgsScope();
	}

	inline const Arr<const ConcreteFunInst> specImpls() const {
		return funInst.specImpls;
	}
};

Comparison compareConcreteFunKey(const ConcreteFunKey a, const ConcreteFunKey b);

struct ConcreteFunSource {
	const ConcreteFun* concreteFun;
	// NOTE: for a lambda, this is for the *outermost* fun (the one with type args and spec impls).
	// The FunDecl is needed for its TypeParam declataions.
	const ConcreteFunInst containingFunInst;
	// Specializations on the parameters of *this* fun, e.g. the lambda and not the outer fun.
	// We don't need specializations of outer parameters because those are handled by closure specialization.
	// Note: this does not include the closure parameter (which is always specialized anyway)
	const Arr<const ConstantOrLambdaOrVariable> paramsSpecialize;
	// Similarly, body of the current fun, not the outer one.
	// For a lambda this is always an Expr.
	const FunBody body;
	const Opt<const KnownLambdaBody*> knownLambdaBody;

	inline const FunDecl* containingFunDecl() const {
		return containingFunInst.decl;
	}

	inline const Arr<const ConcreteType> typeArgs() const {
		return containingFunInst.typeArgs;
	}

	inline const TypeArgsScope typeArgsScope() const {
		return containingFunInst.typeArgsScope();
	}

	inline const Arr<const ConcreteFunInst> specImpls() const {
		return containingFunInst.specImpls;
	}
};

// Extra information I don't want to store in KnownLambdaBody because it's only used temporarily.
struct LambdaInfo {
	const ConcreteFunInst containingFunInst;
	// For FunAsLambda, this is synthetic
	const Expr* body;
};

struct ConcretizeCtx {
	Arena* arena;
	const FunDecl* allocFun;
	const FunDecl* getVatAndActorFun;
	const Arr<const FunDecl*> ifFuns;
	const Arr<const FunDecl*> callFuns;
	const StructInst* ctxStructInst;
	const CommonTypes* commonTypes;
	AllConstants* allConstants;
	MutDict<const ConcreteStructKey, ConcreteStruct*, compareConcreteStructKey> allConcreteStructs {};
	MutDict<const ConcreteFunKey, ConcreteFun*, compareConcreteFunKey> allConcreteFuns {};

	// Funs we still need to write the bodies for
	MutArr<ConcreteFun*> concreteFunsQueue {};
	// This will only have an entry while a ConcreteFun hasn't had it's body filled in yet.
	MutDict<const ConcreteFun*, const ConcreteFunSource, comparePtr<const ConcreteFun>> concreteFunToSource {};
	// This keeps the entry forever, because we don't know when we'll need to instantiate it again.
	// (When we do that we copy this to the ConcreteFunSource)
	MutDict<const KnownLambdaBody*, const LambdaInfo, comparePtr<const KnownLambdaBody>> knownLambdaBodyToInfo {};
	// TODO: do this eagerly
	Late<const ConcreteType> _boolType;
	Late<const ConcreteType> _charType;
	Late<const ConcreteType> _voidType;
	Late<const ConcreteType> _anyPtrType;
	Late<const ConcreteType> _ctxType;

	ConcretizeCtx(
		Arena* _arena,
		const FunDecl* _allocFun,
		const FunDecl* _getVatAndActorFun,
		const Arr<const FunDecl*> _ifFuns,
		const Arr<const FunDecl*> _callFuns,
		const StructInst* _ctxStructInst,
		const CommonTypes* _commonTypes
	) :
		arena{_arena},
		allocFun{_allocFun},
		getVatAndActorFun{_getVatAndActorFun},
		ifFuns{_ifFuns},
		callFuns{_callFuns},
		ctxStructInst{_ctxStructInst},
		commonTypes{_commonTypes},
		allConstants{newAllConstants(arena)}
	{}

	const ConcreteType boolType();
	const ConcreteType charType();
	const ConcreteType voidType();
	inline const ConcreteType anyPtrType();
	const ConcreteType ctxType();
};

const ConcreteFun* getOrAddNonTemplateConcreteFunAndFillBody(ConcretizeCtx* ctx, const FunDecl* decl);

const ConcreteFun* getOrAddConcreteFunAndFillBody(ConcretizeCtx* ctx, const ConcreteFunKey key);

const ConcreteFun* instantiateKnownLambdaBodyForDirectCall(
	ConcretizeCtx* ctx,
	const KnownLambdaBody* klb,
	const Arr<const ConstantOrLambdaOrVariable> args);
const ConcreteFun* instantiateKnownLambdaBodyForDynamic(ConcretizeCtx* ctx, const KnownLambdaBody* klb);
inline const ConcreteFun* getConcreteFunForCallAndFillBody(
	ConcretizeCtx* ctx,
	const ConcreteFunInst fun,
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs
) {
	return getOrAddConcreteFunAndFillBody(ctx, ConcreteFunKey{fun, specializeOnArgs});
}

const ConcreteType getConcreteType_forStructInst(
	ConcretizeCtx* ctx,
	const StructInst* i,
	const TypeArgsScope typeArgsScope);
// TODO: 't' may contain type params, must pass in current context
const ConcreteType getConcreteType(ConcretizeCtx* ctx, const Type t, const TypeArgsScope typeArgsScope);
const Arr<const ConcreteType> typesToConcreteTypes(
	ConcretizeCtx* ctx,
	const Arr<const Type> types,
	const TypeArgsScope typeArgsScope);

const Opt<const ConcreteType> concreteTypeFromFields(
	Arena* arena,
	const Arr<const ConcreteField> fields,
	const Str mangledName);
const Opt<const ConcreteType> concreteTypeFromFields_neverPointer(
	Arena* arena,
	const Arr<const ConcreteField> fields,
	const Str mangledName);

const Bool isCallFun(ConcretizeCtx* ctx, const FunDecl* decl);

// TODO:MOVE?
template <typename T>
const ConstantOrExpr nuExpr(
	Arena* arena,
	const ConcreteType type,
	const SourceRange range,
	const Opt<const KnownLambdaBody*> klb,
	T t
) {
	return ConstantOrExpr{nu<const ConcreteExpr>{}(arena, type, range, klb, t)};
}

template <typename T>
const ConstantOrExpr nuExpr(Arena* arena, const ConcreteType type, const SourceRange range, T t) {
	return nuExpr(arena, type, range, none<const KnownLambdaBody*>(), t);
}

inline const ConcreteFun* getAllocFun(ConcretizeCtx* ctx) {
	return getOrAddNonTemplateConcreteFunAndFillBody(ctx, ctx->allocFun);
}

inline const ConcreteFun* getGetVatAndActorFun(ConcretizeCtx* ctx) {
	return getOrAddNonTemplateConcreteFunAndFillBody(ctx, ctx->getVatAndActorFun);
}

