#include "./concreteModel.h"

ConstantKind::Lambda::Lambda(const KnownLambdaBody* klb)  : knownLambdaBody{klb} {
	// If it needs a closure it can't be a constant
	// (constant things closed over are omitted from the closure)
	assert(!klb->hasClosure());
}

const Constant* ConstantKind::Ptr::deref() const {
	return array->kind.asArray().elements()[index];
}

bool constantArrKeyEq(const ConstantArrKey a, const ConstantArrKey b) {
	return concreteTypeEq(a.elementType, b.elementType)
		&& arrEq<const Constant*, ptrEquals<const Constant>>(a.elements, b.elements);
}

bool constantOrLambdaOrVariableArrEq(const Arr<const ConstantOrLambdaOrVariable> a, const Arr<const ConstantOrLambdaOrVariable> b) {
	return arrEq<const ConstantOrLambdaOrVariable, constantOrLambdaOrVariableEq>(a, b);
}
