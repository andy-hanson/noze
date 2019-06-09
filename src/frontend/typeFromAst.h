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

const Type typeFromAst(
	CheckCtx& ctx,
	const TypeAst ast,
	const StructsAndAliasesMap& structsAndAliasesMap,
	const TypeParamsScope& typeParamsScope,
	DelayStructInsts delayStructInsts
);

const Opt<const StructInst*> instStructFromAst(
	CheckCtx& ctx,
	const TypeAst::InstStruct ast,
	const StructsAndAliasesMap& structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts);

const StructInst* instantiateStruct(
	Arena& arena,
	const StructDecl* decl,
	const Arr<const Type> typeArgs,
	DelayStructInsts delayStructInsts);

const StructBody instantiateStructBody(Arena& arena, const StructDecl* decl, const Arr<const Type> typeArgs);

bool typeIsPossiblySendable(const Type type);

const Type makeFutType(Arena& arena, const CommonTypes& commonTypes, const Type type);
