#include "./concretizeUtil.h"

#include "../util/arrUtil.h"

namespace {
	const Arr<const Constant*> asAllConstant(Arena* arena, const Arr<const ConstantOrExpr> args) {
		return map<const Constant*>{}(arena, args, [](const ConstantOrExpr ca) {
			return ca.asConstant();
		});
	}

	const Arr<const Constant*> asAllConstant(Arena* arena, const Arr<const ConstantOrLambdaOrVariable> args) {
		return map<const Constant*>{}(arena, args, [](const ConstantOrLambdaOrVariable a) {
			return a.asConstant();
		});
	}
}

const Bool allConstant(const Arr<const ConstantOrExpr> args) {
	return every(args, [](const ConstantOrExpr c) {
		return c.isConstant();
	});
}

const Bool allConstant(const Arr<const ConstantOrLambdaOrVariable> args) {
	return every(args, [](const ConstantOrLambdaOrVariable c) {
		return c.isConstant();
	});
}

const Opt<const Arr<const Constant*>> tryGetAllConstant(Arena* arena, const Arr<const ConstantOrExpr> args) {
	return allConstant(args)
		? some<const Arr<const Constant*>>(asAllConstant(arena, args))
		: none<const Arr<const Constant*>>();
}

const Opt<const Arr<const Constant*>> tryGetAllConstant(Arena* arena, const Arr<const ConstantOrLambdaOrVariable> args) {
	return allConstant(args)
		? some<const Arr<const Constant*>>(asAllConstant(arena, args))
		: none<const Arr<const Constant*>>();
}

const Arr<const ConstantOrLambdaOrVariable> allVariable(Arena* arena, const size_t size) {
	// TODO:PERF pre-allocate these for each arity
	return fillArr<const ConstantOrLambdaOrVariable>{}(arena, size, [](const size_t) {
		return ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}};
	});
}

void writeConcreteTypeForMangle(Writer& writer, const ConcreteType t) {
	writeStatic(writer, "__");
	if (t.isPointer)
		writeStatic(writer, "ptr_");
	writeStr(writer, t.strukt->mangledName);
}
