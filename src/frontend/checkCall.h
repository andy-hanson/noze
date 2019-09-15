#pragma once

#include "./inferringType.h"

template <typename Cb>
void eachFunInScope(ExprCtx* ctx, const Sym funName, Cb cb) {
	size_t totalIndex = 0;
	for (const SpecInst* specInst : ctx->outermostFun->specs) {
		specInst->body.match(
			[](const SpecBody::Builtin) {},
			[&](const Arr<const Sig> sigs) {
				for (const size_t i : indices(sigs))
					if (symEq(at(sigs, i).name, funName))
						cb(CalledDecl{SpecSig{specInst, ptrAt(sigs, i), totalIndex + i}});
				totalIndex += size(sigs);
			});
	}

	for (const FunDecl* f : multiDictGetAt<const Sym, const FunDecl*, compareSym>(ctx->funsMap, funName))
		cb(CalledDecl{f});

	for (const Module* m : ctx->checkCtx->allFlattenedImports)
		for (const FunDecl* f : multiDictGetAt<const Sym, const FunDecl*, compareSym>(m->funsMap, funName))
			if (f->isPublic)
				cb(CalledDecl{f});
}

const CheckedExpr checkCall(ExprCtx* ctx, const SourceRange range, const CallAst ast, Expected* expected);

inline const CheckedExpr checkIdentifierCall(
	ExprCtx* ctx,
	const SourceRange range,
	const Sym name,
	Expected* expected
) {
	return checkCall(ctx, range, CallAst{name, emptyArr<const TypeAst>(), emptyArr<const ExprAst>()}, expected);
}
