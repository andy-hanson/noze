#include "./concreteModel.h"

#include "./util/arrUtil.h"

ConstantKind::Lambda::Lambda(const KnownLambdaBody* klb)  : knownLambdaBody{klb} {
	// If it needs a closure it can't be a constant
	// (constant things closed over are omitted from the closure)
	assert(!klb->hasClosure());
}

const Constant* ConstantKind::Ptr::deref() const {
	return array->kind.asArray().elements()[index];
}
