#pragma once

#include "../concreteModel.h"
#include "../model.h"

#include "./allConstants.h"
#include "./writer.h"

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

bool concreteStructKeyEq(const ConcreteStructKey a, const ConcreteStructKey b);

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

bool concreteFunKeyEq(const ConcreteFunKey a, const ConcreteFunKey b);

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
	// Only for lambda / iface
	const Opt<const Arr<const ConstantOrLambdaOrVariable>> closureSpecialize;

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
	MutDict<const ConcreteStructKey, ConcreteStruct*, concreteStructKeyEq> allConcreteStructs {};
	MutDict<const ConcreteFunKey, ConcreteFun*, concreteFunKeyEq> allConcreteFuns {};

	// Funs we still need to write the bodies for
	MutArr<ConcreteFun*> concreteFunsQueue {};
	// This will only have an entry while a ConcreteFun hasn't had it's body filled in yet.
	MutDict<const ConcreteFun*, const ConcreteFunSource, ptrEquals<const ConcreteFun>> concreteFunToSource {};
	// This keeps the entry forever, because we don't know when we'll need to instantiate it again.
	// (When we do that we copy this to the ConcreteFunSource)
	MutDict<const KnownLambdaBody*, const LambdaInfo, ptrEquals<const KnownLambdaBody>> knownLambdaBodyToInfo {};
	// TODO: do this eagerly
	Late<const ConcreteType> _voidPtrType;

	ConcretizeCtx(Arena& _arena, const FunDecl* _allocFun, const Arr<const FunDecl*> _callFuns, const CommonTypes& _commonTypes)
		: arena{_arena}, allocFun{_allocFun}, callFuns{_callFuns}, commonTypes{_commonTypes}, allConstants{arena} {}

	const ConcreteType voidPtrType();
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

void writeConcreteTypeForMangle(Writer& writer, const ConcreteType t);

bool isCallFun(ConcretizeCtx& ctx, const FunDecl* decl);


//TODO:MOVE?
struct SpecializeOnArgs {
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs;
	const Arr<const ConstantOrExpr> notSpecializedArgs;
};
const SpecializeOnArgs getSpecializeOnArgsForLambdaClosure(Arena& arena, const Arr<const ConstantOrExpr> args);
const SpecializeOnArgs getSpecializeOnArgsForLambdaCall(Arena& arena, const Arr<const ConstantOrExpr> args, const bool isSummon);
const SpecializeOnArgs getSpecializeOnArgsForFun(ConcretizeCtx& ctx, const FunDecl* f, const Arr<const ConstantOrExpr> args);

const Arr<const ConcreteField> concretizeClosureFieldsAndSpecialize(
	ConcretizeCtx& ctx,
	const Arr<const ClosureField*> closure,
	const Arr<const ConstantOrLambdaOrVariable> closureSpecialize,
	const TypeArgsScope typeArgsScope
);
const Arr<const ConcreteParam> concretizeParamsNoSpecialize(ConcretizeCtx& ctx, const Arr<const Param> params, const TypeArgsScope typeArgsScope);
