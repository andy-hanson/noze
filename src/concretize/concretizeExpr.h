#include "../concreteModel.h"
#include "./concretizeCtx.h"

const ConstantOrExpr doConcretizeExpr(
	ConcretizeCtx& ctx,
	const ConcreteFunSource source,
	ConcreteFun* cf,
	const Expr e);
