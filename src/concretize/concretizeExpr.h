#include "../concreteModel.h"
#include "./concretizeCtx.h"

const ConcreteFunBody doConcretizeExpr(
	ConcretizeCtx& ctx,
	const ConcreteFunSource source,
	ConcreteFun* cf,
	const Expr e);
