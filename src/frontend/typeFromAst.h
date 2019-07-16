#pragma once

#include "./ast.h"
#include "./checkCtx.h"
#include "../instantiate.h"
#include "../model.h"

const Opt<const StructInst*> instStructFromAst(
	CheckCtx* ctx,
	const TypeAst::InstStruct ast,
	const StructsAndAliasesMap& structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts);

inline const Opt<const StructInst*> instStructFromAstNeverDelay(
	CheckCtx* ctx,
	const TypeAst::InstStruct ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const TypeParamsScope typeParamsScope
) {
	return instStructFromAst(ctx, ast, structsAndAliasesMap, typeParamsScope, none<MutArr<StructInst*>*>());
}

const Type typeFromAst(
	CheckCtx* ctx,
	const TypeAst ast,
	const StructsAndAliasesMap& structsAndAliasesMap,
	const TypeParamsScope& typeParamsScope,
	DelayStructInsts delayStructInsts
);

const Opt<const SpecDecl*> tryFindSpec(
	CheckCtx* ctx,
	const Sym name,
	const SourceRange range,
	const SpecsMap specsMap);

const Arr<const Type> typeArgsFromAsts(
	CheckCtx* ctx,
	const Arr<const TypeAst> typeAsts,
	const StructsAndAliasesMap structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts
);

const Bool typeIsPossiblySendable(const Type type);

const Type makeFutType(Arena* arena, const CommonTypes& commonTypes, const Type type);
