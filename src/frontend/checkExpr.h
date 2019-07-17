#pragma once

#include "../model.h"
#include "./ast.h"
#include "./checkCtx.h"
#include "./inferringType.h"

const Expr* checkFunctionBody(
	CheckCtx* checkCtx,
	const ExprAst ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const FunsMap funsMap,
	const FunDecl* fun,
	const CommonTypes* commonTypes
);

const Expr checkExpr(ExprCtx* ctx, const ExprAst ast, Expected* expected);
