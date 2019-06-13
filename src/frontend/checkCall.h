#pragma once

#include "./inferringType.h"

template <typename Cb>
void eachFunInScope(ExprContext& ctx, const Str funName, Cb cb) {
	size_t specSigsTotalIndex = 0;
	for (const SpecUse* specUse : ptrsRange(ctx.outermostFun->specs)) {
		const Arr<const Sig> sigs = specUse->spec->sigs;
		for (const size_t i : Range{0, sigs.size})
			if (strEq(sigs[i].name, funName))
				cb(CalledDecl{CalledDecl::SpecUseSig{specUse, sigs.getPtr(i), specSigsTotalIndex + i}});
		specSigsTotalIndex += sigs.size;
	}

	for (const FunDecl* f : ctx.funsMap.get(funName))
		cb(CalledDecl{f});

	for (const Module* m : ctx.checkCtx.includeAndImportsRange())
		for (const FunDecl* f : m->funsMap.get(funName))
			if (f->isPublic)
				cb(CalledDecl{f});
}

const CheckedExpr checkCall(ExprContext& ctx, const SourceRange range, const CallAst ast, Expected& expected);

inline const CheckedExpr checkIdentifierCall(ExprContext& ctx, const SourceRange range, const Str name, Expected& expected) {
	return checkCall(ctx, range, CallAst{name, emptyArr<const TypeAst>(), emptyArr<const ExprAst>()}, expected);
}
