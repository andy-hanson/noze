#pragma once

#include "./ast.h"
#include "./checkCtx.h"
#include "../model.h"

struct TypeParamsScope {
	// TODO: consistent naming
	const Arr<const TypeParam> innerTypeParams;
	const Arr<const TypeParam> outer = emptyArr<const TypeParam>();
};

using DelayStructInsts = const Opt<MutArr<StructInst*>&>;

const Type instantiateType(Arena& arena, const Type type, const Arr<const TypeParam> typeParams, const Arr<const Type> typeArgs);

inline const Type instantiateType(Arena& arena, const Type type, const StructInst* structInst) {
	return instantiateType(arena, type, structInst->decl->typeParams, structInst->typeArgs);
}

const StructBody instantiateStructBody(Arena& arena, const StructDecl* decl, const Arr<const Type> typeArgs);

const StructInst* instantiateStruct(
	Arena& arena,
	const StructDecl* decl,
	const Arr<const Type> typeArgs,
	DelayStructInsts delayStructInsts);

const StructInst* instantiateStructInst(Arena& arena, const StructInst* structInst, const Arr<const TypeParam> typeParams, const Arr<const Type> typeArgs);
inline const StructInst* instantiateStructInst(Arena& arena, const StructInst* structInst, const StructInst* contextStructInst) {
	return instantiateStructInst(arena, structInst, contextStructInst->decl->typeParams, contextStructInst->typeArgs);
}

inline const StructInst* instantiateStructNeverDelay(Arena& arena, const StructDecl* decl, const Arr<const Type> typeArgs) {
	return instantiateStruct(arena, decl, typeArgs, none<MutArr<StructInst*>&>());
}

const Opt<const StructInst*> instStructFromAst(
	CheckCtx& ctx,
	const TypeAst::InstStruct ast,
	const StructsAndAliasesMap& structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts);

inline const Opt<const StructInst*> instStructFromAstNeverDelay(
	CheckCtx& ctx,
	const TypeAst::InstStruct ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const TypeParamsScope typeParamsScope
) {
	return instStructFromAst(ctx, ast, structsAndAliasesMap, typeParamsScope, none<MutArr<StructInst*>&>());
}

const Type typeFromAst(
	CheckCtx& ctx,
	const TypeAst ast,
	const StructsAndAliasesMap& structsAndAliasesMap,
	const TypeParamsScope& typeParamsScope,
	DelayStructInsts delayStructInsts
);

const Opt<const SpecDecl*> tryFindSpec(
	CheckCtx& ctx,
	const Str name,
	const SourceRange range,
	const SpecsMap specsMap);

const Arr<const Type> typeArgsFromAsts(
	CheckCtx& ctx,
	const Arr<const TypeAst> typeAsts,
	const StructsAndAliasesMap structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts
);

template <typename T>
inline Opt<T*> tryGetTypeArg(const Arr<const TypeParam> typeParams, Arr<T> typeArgs, const TypeParam* typeParam) {
	const Bool hasTypeParam = ptrEquals(typeParams.begin() + typeParam->index, typeParam);
	return hasTypeParam
		? some<T*>(typeArgs.getPtr(typeParam->index))
		: none<T*>();
}

const Bool typeIsPossiblySendable(const Type type);

const Type makeFutType(Arena& arena, const CommonTypes& commonTypes, const Type type);
