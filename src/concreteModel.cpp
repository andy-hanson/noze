#include "./concreteModel.h"

namespace {
	bool constantOrLambdaOrVariableEq(const ConstantOrLambdaOrVariable a, const ConstantOrLambdaOrVariable b) {
		return a.match(
			[&](const ConstantOrLambdaOrVariable::Variable) {
				return b.isVariable();
			},
			[&](const Constant* ca) {
				return b.isConstant() && ptrEquals(ca, b.asConstant());
			},
			[&](const KnownLambdaBody* klba) {
				return b.isKnownLambdaBody() && ptrEquals(klba, b.asKnownLambdaBody());
			});
	}
}

bool constantOrLambdaOrVariableArrEq(const Arr<const ConstantOrLambdaOrVariable> a, const Arr<const ConstantOrLambdaOrVariable> b) {
	return arrEq<const ConstantOrLambdaOrVariable, constantOrLambdaOrVariableEq>(a, b);
}
