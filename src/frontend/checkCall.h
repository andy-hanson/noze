#pragma once

#include "./inferringType.h"

template <typename Cb>
void eachFunInScope(ExprCtx& ctx, const Str funName, Cb cb) {
	size_t totalIndex = 0;
	for (const SpecInst* specInst : ctx.outermostFun->specs) {
		const Arr<const Sig> sigs = specInst->sigs;
		//TODO:EACHWITHINDEX
		for (const size_t i : Range{sigs.size})
			if (strEq(at(sigs, i).name, funName))
				cb(CalledDecl{SpecSig{specInst, getPtr(sigs, i), totalIndex + i}});
		totalIndex += sigs.size;
	}

	for (const FunDecl* f : ctx.funsMap.get(funName))
		cb(CalledDecl{f});

	for (const Module* m : ctx.checkCtx.includeAndImportsRange())
		for (const FunDecl* f : m->funsMap.get(funName))
			if (f->isPublic)
				cb(CalledDecl{f});
}

const CheckedExpr checkCall(ExprCtx& ctx, const SourceRange range, const CallAst ast, Expected& expected);

inline const CheckedExpr checkIdentifierCall(ExprCtx& ctx, const SourceRange range, const Str name, Expected& expected) {
	return checkCall(ctx, range, CallAst{name, emptyArr<const TypeAst>(), emptyArr<const ExprAst>()}, expected);
}
