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
		assert(typeParams.size == typeArgs.size);
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

struct FunDeclAndTypeArgs {
	const FunDecl* decl;
	const Arr<const ConcreteType> typeArgs;

	inline FunDeclAndTypeArgs(const FunDecl* _decl, const Arr<const ConcreteType> _typeArgs)
		: decl{_decl}, typeArgs{_typeArgs} {
		assert(decl->typeParams.size == typeArgs.size);
	}

	inline const TypeArgsScope typeArgsScope() const {
		return TypeArgsScope{decl->typeParams, typeArgs};
	}

	inline const FunDeclAndTypeArgs withTypeArgs(const Arr<const ConcreteType> newTypeArgs) const {
		return FunDeclAndTypeArgs{decl, newTypeArgs};
	}
};

struct FunDeclAndTypeArgsAndSpecImpls {
	const FunDeclAndTypeArgs funDeclAndTypeArgs;
	const Arr<const FunDecl*> specImpls;

	inline const FunDecl* decl() const {
		return funDeclAndTypeArgs.decl;
	}

	inline const TypeArgsScope typeArgsScope() const {
		return funDeclAndTypeArgs.typeArgsScope();
	}
};

struct ConcreteFunKey {
	const FunDeclAndTypeArgsAndSpecImpls declAndTypeArgsAndSpecImpls;
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs;

	inline const FunDecl* decl() const {
		return declAndTypeArgsAndSpecImpls.decl();
	}

	inline const TypeArgsScope typeArgsScope() const {
		return declAndTypeArgsAndSpecImpls.typeArgsScope();
	}

	inline const Arr<const FunDecl*> specImpls() const {
		return declAndTypeArgsAndSpecImpls.specImpls;
	}
};

Comparison compareConcreteFunKey(const ConcreteFunKey a, const ConcreteFunKey b);

struct ConcreteFunSource {
	// NOTE: for a lambda, this is for the *outermost* fun (the one with type args and spec impls).
	// The FunDecl is needed for its TypeParam declataions.
	const FunDeclAndTypeArgsAndSpecImpls containingFunInfo;
	// Specializations on the parameters of *this* fun, e.g. the lambda and not the outer fun.
	// We don't need specializations of outer parameters because those are handled by closure specialization.
	// Note: this does not include the closure parameter (which is always specialized anyway)
	const Arr<const ConstantOrLambdaOrVariable> paramsSpecialize;
	// Similarly, body of the current fun, not the outer one.
	// For a lambda or iface this is always an Expr.
	const FunBody body;
	const Opt<const KnownLambdaBody*> knownLambdaBody;

	inline const FunDeclAndTypeArgs containingFunDeclAndTypeArgs() const {
		return containingFunInfo.funDeclAndTypeArgs;
	}

	inline const Arr<const ConcreteType> typeArgs() const {
		return containingFunDeclAndTypeArgs().typeArgs;
	}

	inline const FunDecl* containingFunDecl() const {
		return containingFunDeclAndTypeArgs().decl;
	}

	inline const TypeArgsScope typeArgsScope() const {
		return containingFunDeclAndTypeArgs().typeArgsScope();
	}
};

// Extra information I don't want to store in KnownLambdaBody because it's only used temporarily.
struct LambdaInfo {
	const FunDeclAndTypeArgsAndSpecImpls containingFunDeclAndTypeArgsAndSpecImpls;
	// For FunAsLambda, this is synthetic
	const Expr* body;
};

struct ConcretizeCtx {
	Arena& arena;
	const FunDecl* allocFun;
	const Arr<const FunDecl*> callFuns;
	const CommonTypes& commonTypes;
	AllConstants allConstants;
	MutDict<const ConcreteStructKey, ConcreteStruct*, compareConcreteStructKey> allConcreteStructs {};
	MutDict<const ConcreteFunKey, ConcreteFun*, compareConcreteFunKey> allConcreteFuns {};

	// Funs we still need to write the bodies for
	MutArr<ConcreteFun*> concreteFunsQueue {};
	// This will only have an entry while a ConcreteFun hasn't had it's body filled in yet.
	MutDict<const ConcreteFun*, const ConcreteFunSource, comparePointer<const ConcreteFun>> concreteFunToSource {};
	// This keeps the entry forever, because we don't know when we'll need to instantiate it again.
	// (When we do that we copy this to the ConcreteFunSource)
	MutDict<const KnownLambdaBody*, const LambdaInfo, comparePointer<const KnownLambdaBody>> knownLambdaBodyToInfo {};
	// TODO: do this eagerly
	Late<const ConcreteType> _boolType;
	Late<const ConcreteType> _charType;
	Late<const ConcreteType> _voidType;
	Late<const ConcreteType> _anyPtrType;
	Late<const ConcreteType> _ctxPtrType;

	ConcretizeCtx(Arena& _arena, const FunDecl* _allocFun, const Arr<const FunDecl*> _callFuns, const CommonTypes& _commonTypes)
		: arena{_arena}, allocFun{_allocFun}, callFuns{_callFuns}, commonTypes{_commonTypes}, allConstants{} {}

	const ConcreteType boolType();
	const ConcreteType charType();
	const ConcreteType voidType();
	inline const ConcreteType anyPtrType();
	const ConcreteType ctxPtrType();
};

const ConcreteFun* getOrAddNonGenericConcreteFunAndFillBody(ConcretizeCtx& ctx, const FunDecl* decl);

const ConcreteFun* getOrAddConcreteFunAndFillBody(ConcretizeCtx& ctx, const ConcreteFunKey key);

const ConcreteFun* instantiateKnownLambdaBodyForDirectCall(ConcretizeCtx& ctx, const KnownLambdaBody* klb, const Arr<const ConstantOrLambdaOrVariable> args);
const ConcreteFun* instantiateKnownLambdaBodyForDynamic(ConcretizeCtx& ctx, const KnownLambdaBody* klb);
inline const ConcreteFun* getConcreteFunForCallAndFillBody(
	ConcretizeCtx& ctx,
	const FunDecl* decl,
	const Arr<const ConcreteType> typeArgs,
	const Arr<const FunDecl*> specImpls,
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs
) {
	const ConcreteFunKey key = ConcreteFunKey{
		FunDeclAndTypeArgsAndSpecImpls{
			FunDeclAndTypeArgs{decl, typeArgs},
			specImpls},
		specializeOnArgs};
	return getOrAddConcreteFunAndFillBody(ctx, key);
}

const ConcreteType getConcreteType_forStructInst(ConcretizeCtx& ctx, const StructInst* i, const TypeArgsScope typeArgsScope);
// TODO: 't' may contain type params, must pass in current context
const ConcreteType getConcreteType(ConcretizeCtx& ctx, const Type t, const TypeArgsScope typeArgsScope);
const Arr<const ConcreteType> typesToConcreteTypes(ConcretizeCtx& ctx, const Arr<const Type> types, const TypeArgsScope typeArgsScope);

const Opt<const ConcreteType> concreteTypeFromFields(Arena& arena, const Arr<const ConcreteField> fields, const Str mangledName);
const Opt<const ConcreteType> concreteTypeFromFields_neverPointer(Arena& arena, const Arr<const ConcreteField> fields, const Str mangledName);

const Bool isCallFun(ConcretizeCtx& ctx, const FunDecl* decl);


//TODO:MOVE?
struct SpecializeOnArgs {
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs;
	// This has en entry for each in specializedOnArgs that is not constant
	const Arr<const ConstantOrExpr> notSpecializedArgs;

	inline SpecializeOnArgs(
		const Arr<const ConstantOrLambdaOrVariable> _specializeOnArgs,
		const Arr<const ConstantOrExpr> _notSpecializedArgs
	) : specializeOnArgs{_specializeOnArgs}, notSpecializedArgs{_notSpecializedArgs} {
		size_t nNotSpecialized = 0;
		for (const ConstantOrLambdaOrVariable arg : specializeOnArgs)
			if (!arg.isConstant())
				nNotSpecialized++;
		if (nNotSpecialized != notSpecializedArgs.size) {
			printf("nNotSpecialized: %zu, notSpecializedArgs: %zu\n", nNotSpecialized, notSpecializedArgs.size);
		}
		assert(nNotSpecialized == notSpecializedArgs.size);
	}
};
const SpecializeOnArgs getSpecializeOnArgsForLambdaClosure(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args);
const SpecializeOnArgs getSpecializeOnArgsForLambdaCall(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args, const Bool isSummon);
const SpecializeOnArgs getSpecializeOnArgsForFun(ConcretizeCtx& ctx, const SourceRange range, const FunDecl* f, const Arr<const ConstantOrExpr> args);

const Arr<const ConcreteField> concretizeClosureFieldsAndSpecialize(
	ConcretizeCtx& ctx,
	const Arr<const ClosureField*> closure,
	const Arr<const ConstantOrLambdaOrVariable> closureSpecialize,
	const TypeArgsScope typeArgsScope
);
const Arr<const ConcreteParam> concretizeParamsNoSpecialize(ConcretizeCtx& ctx, const Arr<const Param> params, const TypeArgsScope typeArgsScope);

const ConstantOrExpr makeLambdasDynamic(ConcretizeCtx& ctx, const SourceRange range, const ConstantOrExpr expr);
const Arr<const ConstantOrExpr> makeLambdasDynamic_arr(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> expr);

// TODO:MOVE?
template <typename T>
const ConstantOrExpr nuExpr(Arena& arena, const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, T t) {
	return ConstantOrExpr{arena.nu<const ConcreteExpr>()(type, range, klb, t)};
}

template <typename T>
const ConstantOrExpr nuExpr(Arena& arena, const ConcreteType type, const SourceRange range, T t) {
	return nuExpr(arena, type, range, none<const KnownLambdaBody*>(), t);
}

inline const ConcreteFun* getAllocFun(ConcretizeCtx& ctx) {
	return getOrAddNonGenericConcreteFunAndFillBody(ctx, ctx.allocFun);
}
