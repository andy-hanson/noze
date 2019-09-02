#pragma once

#include "./model.h"

struct TypeParamsScope {
	// TODO: consistent naming
	const Arr<const TypeParam> innerTypeParams;
};

struct TypeParamsAndArgs {
	const Arr<const TypeParam> typeParams;
	const Arr<const Type> typeArgs;

	inline TypeParamsAndArgs(const Arr<const TypeParam> _typeParams, const Arr<const Type> _typeArgs)
		: typeParams{_typeParams}, typeArgs{_typeArgs} {
		assert(sizeEq(typeParams, typeArgs));
	}
};

template <typename T>
inline Opt<T*> tryGetTypeArg(const Arr<const TypeParam> typeParams, const Arr<T> typeArgs, const TypeParam* typeParam) {
	const Bool hasTypeParam = ptrEquals(typeParams.begin() + typeParam->index, typeParam);
	return hasTypeParam
		? some<T*>(ptrAt(typeArgs, typeParam->index))
		: none<T*>();
}

inline const Opt<const Type> tryGetTypeArgFromTypeParamsAndArgs(
	const TypeParamsAndArgs typeParamsAndArgs,
	const TypeParam* typeParam
) {
	const Opt<const Type*> t = tryGetTypeArg(typeParamsAndArgs.typeParams, typeParamsAndArgs.typeArgs, typeParam);
	return has(t) ? some<const Type>(*force(t)) : none<const Type>();
}

using DelayStructInsts = const Opt<MutArr<StructInst*>*>;

const Type instantiateType(Arena* arena, const Type type, const TypeParamsAndArgs typeParamsAndArgs);

inline const Type instantiateType(Arena* arena, const Type type, const StructInst* structInst) {
	return instantiateType(arena, type, TypeParamsAndArgs{structInst->decl->typeParams, structInst->typeArgs});
}

const FunInst* instantiateFun(
	Arena* arena,
	const FunDecl* decl,
	const Arr<const Type> typeArgs,
	const Arr<const Called> specImpls);

inline const FunInst* instantiateNonTemplateFun(Arena* arena, const FunDecl* decl) {
	assert(!decl->isTemplate());
	return instantiateFun(arena, decl, emptyArr<const Type>(), emptyArr<const Called>());
}

const StructBody instantiateStructBody(Arena* arena, const StructDecl* decl, const Arr<const Type> typeArgs);

const StructInst* instantiateStruct(
	Arena* arena,
	const StructDecl* decl,
	const Arr<const Type> typeArgs,
	DelayStructInsts delayStructInsts);

const StructInst* instantiateStructInst(
	Arena* arena,
	const StructInst* structInst,
	const TypeParamsAndArgs typeParamsAndArgs);
inline const StructInst* instantiateStructInst(
	Arena* arena,
	const StructInst* structInst,
	const StructInst* contextStructInst
) {
	return instantiateStructInst(
		arena,
		structInst,
		TypeParamsAndArgs{contextStructInst->decl->typeParams, contextStructInst->typeArgs});
}

inline const StructInst* instantiateStructNeverDelay(
	Arena* arena,
	const StructDecl* decl,
	const Arr<const Type> typeArgs
) {
	return instantiateStruct(arena, decl, typeArgs, none<MutArr<StructInst*>*>());
}

const SpecInst* instantiateSpec(Arena* arena, const SpecDecl* decl, const Arr<const Type> typeArgs);
const SpecInst* instantiateSpecInst(Arena* arena, const SpecInst* specInst, const TypeParamsAndArgs typeParamsAndArgs);
