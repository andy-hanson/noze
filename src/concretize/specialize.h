#pragma once

#include "../concreteModel.h"
#include "../util/arr.h"

#include "./concretizeCtx.h"

struct SpecializeOnArgs {
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs;
	// This has en entry for each in specializedOnArgs that is not constant
	const Arr<const ConstantOrExpr> nonOmittedArgs;

	inline SpecializeOnArgs(
		const Arr<const ConstantOrLambdaOrVariable> _specializeOnArgs,
		const Arr<const ConstantOrExpr> _nonOmittedArgs
	) : specializeOnArgs{_specializeOnArgs}, nonOmittedArgs{_nonOmittedArgs} {
		size_t nNotSpecialized = 0;
		for (const ConstantOrLambdaOrVariable arg : specializeOnArgs)
			if (!arg.isConstant())
				nNotSpecialized++;
		assert(nNotSpecialized == nonOmittedArgs.size);
	}
};
const SpecializeOnArgs getSpecializeOnArgsForLambdaClosure(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args);
const SpecializeOnArgs getSpecializeOnArgsForLambdaCall(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args, const Bool isSummon);
const SpecializeOnArgs getSpecializeOnArgsForFun(ConcretizeCtx& ctx, const SourceRange range, const FunDecl* f, const Arr<const ConstantOrExpr> args);

const Arr<const ConcreteField> concretizeClosureFieldsAndSpecialize(
	ConcretizeCtx& ctx,
	const Arr<const ClosureField*> closure,
	const Arr<const ConstantOrLambdaOrVariable> closureSpecialize,
	const TypeArgsScope typeArgsScope
);

const Arr<const ConcreteParam> concretizeParamsAndSpecialize(
	ConcretizeCtx& ctx,
	const Arr<const Param> params,
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs,
	const TypeArgsScope typeArgsScope);
const Arr<const ConcreteParam> concretizeParamsNoSpecialize(ConcretizeCtx& ctx, const Arr<const Param> params, const TypeArgsScope typeArgsScope);

// This is for instantiating a KnownLambdaBody.
const Arr<const ConcreteParam> specializeParamsForLambdaInstance(
	ConcretizeCtx& ctx,
	const Arr<const ConcreteParam> nonSpecializedParams,
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs);

const ConstantOrExpr makeLambdasDynamic(ConcretizeCtx& ctx, const SourceRange range, const ConstantOrExpr expr);
const Arr<const ConstantOrExpr> makeLambdasDynamic_arr(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> expr);

inline const Bool shouldAllocateClosureForDynamicLambda(const ConcreteType closureType) {
	// TODO:PERF we could avoid the pointer for closures that don't exceed pointer size.
	return _not(closureType.isPointer);
}

const Bool isNonSpecializableBuiltin(const FunDecl* f);
