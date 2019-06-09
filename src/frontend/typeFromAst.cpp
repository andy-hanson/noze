#include "./typeFromAst.h"

namespace {
	//TODO:MOVE
	template<typename T, typename Cb>
	const Opt<const T*> findInEither(const Arr<T> a, const Arr<T> b, Cb cb) {
		for (size_t i = 0; i < a.size; i++)
			if (cb(a[i]))
				return some<const T*>(a.getPtr(i));
		for (size_t i = 0; i < b.size; i++)
			if (cb(b[i]))
				return some<const T*>(b.getPtr(i));
		return none<const T*>();
	}
}

const Type typeFromAst(
	CheckCtx& ctx,
	const TypeAst ast,
	const StructsAndAliasesMap& structsAndAliasesMap,
	const TypeParamsScope& typeParamsScope,
	DelayStructInsts delayStructInsts
) {
	unused(ctx);
	unused(ast);
	unused(structsAndAliasesMap);
	unused(typeParamsScope);
	unused(delayStructInsts);
	return todo<const Type>("typeFromAst");
}

const StructInst* instantiateStruct(
	Arena& arena,
	const StructDecl* decl,
	const Arr<const Type> typeArgs,
	DelayStructInsts delayStructInsts
) {
	unused(arena);
	unused(decl);
	unused(typeArgs);
	unused(delayStructInsts);
	return todo<const StructInst*>("instantiateStruct");
}
