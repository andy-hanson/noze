#pragma once

#include "../model.h"
#include "./ast.h"
#include "./checkCtx.h"

const Expr* checkFunctionBody(
	CheckCtx& checkCtx,
	const ExprAst ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const FunsMap funsMap,
	const FunDecl fun,
	const CommonTypes commonTypes);
