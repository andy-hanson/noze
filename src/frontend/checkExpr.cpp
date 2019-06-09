#include "./checkExpr.h"

const Expr* checkFunctionBody(
	CheckCtx& checkCtx,
	const ExprAst ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const FunsMap funsMap,
	const FunDecl fun,
	const CommonTypes commonTypes
) {
	unused(checkCtx);
	unused(ast);
	unused(structsAndAliasesMap);
	unused(funsMap);
	unused(fun);
	unused(commonTypes);
	return todo<const Expr*>("checkFunctionBody");
}
