#include "./specialize.h"

#include "./builtinInfo.h"
#include "./concretizeUtil.h"
#include "./mangleName.h"

namespace {
	const ConstantOrExpr makeLambdasDynamic_forConstant(ConcretizeCtx& ctx, const SourceRange range, const Constant* c) {
		if (c->kind.isLambda()) {
			const KnownLambdaBody* klb = c->kind.asLambda().knownLambdaBody;
			const ConcreteFun* fun = instantiateKnownLambdaBodyForDynamic(ctx, klb);
			const ConcreteType closureType = fun->closureType().force();
			const ConstantOrExpr closure = ConstantOrExpr{ctx.allConstants._null(ctx.arena, closureType)};
			const ConcreteExpr::LambdaToDynamic ld = ConcreteExpr::LambdaToDynamic{fun, closure};
			return nuExpr(ctx.arena, klb->dynamicType, range, ld);
		} else {
			return ConstantOrExpr{c};
		}
	}

	const ConstantOrExpr makeLambdasDynamic_forExpr(ConcretizeCtx& ctx, const SourceRange range, const ConcreteExpr* e) {
		if (e->knownLambdaBody().has()) {
			const KnownLambdaBody* klb = e->knownLambdaBody().force();
			const ConcreteFun* fun = instantiateKnownLambdaBodyForDynamic(ctx, klb);
			const ConcreteType closureType = klb->closureType().force();
			const ConstantOrExpr closure = shouldAllocateClosureForDynamicLambda(closureType)
				? nuExpr(
					ctx.arena,
					closureType.changeToByRef(),
					range,
					none<const KnownLambdaBody*>(),
					ConcreteExpr::Alloc{getAllocFun(ctx), e})
				: ConstantOrExpr{e};
			return nuExpr(ctx.arena, klb->dynamicType, range, ConcreteExpr::LambdaToDynamic{fun, closure});
		} else
			return ConstantOrExpr{e};
	}

	const SpecializeOnArgs yesSpecialize(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args, const Bool specializeOnAllConstants) {
		// TODO: helper to map to two different arrs
		ArrBuilder<const ConstantOrExpr> nonOmittedArgs {};
		const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs = map<const ConstantOrLambdaOrVariable>{}(ctx.arena, args, [&](const ConstantOrExpr ce) {
			return ce.match(
				[&](const Constant* c) {
					if (specializeOnAllConstants || c->kind.isLambda())
						// Still specialize on lambda constants
						return ConstantOrLambdaOrVariable{c};
					else {
						nonOmittedArgs.add(ctx.arena, ConstantOrExpr{c});
						return ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}};
					}
				},
				[&](const ConcreteExpr* e) {
					if (e->knownLambdaBody().has()) {
						nonOmittedArgs.add(ctx.arena, ConstantOrExpr{e});
						return ConstantOrLambdaOrVariable{e->knownLambdaBody().force()};
					} else {
						nonOmittedArgs.add(ctx.arena, makeLambdasDynamic_forExpr(ctx, range, e));
						return ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}};
					}
				});
		});
		return SpecializeOnArgs{specializeOnArgs, nonOmittedArgs.finish()};
	}

	const SpecializeOnArgs dontSpecialize(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args) {
		return SpecializeOnArgs{allVariable(ctx.arena, args.size), makeLambdasDynamic_arr(ctx, range, args)};
	}
}

const SpecializeOnArgs getSpecializeOnArgsForLambdaClosure(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args) {
	return yesSpecialize(ctx, range, args, /*specializeOnAllConstants*/ False);
}

const SpecializeOnArgs getSpecializeOnArgsForLambdaCall(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args, const Bool isSummon) {
	Cell<const Bool> allConstant { True };
	Cell<const Bool> someFun { False };
	for (const ConstantOrExpr c : args) {
		if (getKnownLambdaBodyFromConstantOrExpr(c).has())
			someFun.set(True);
		if (!c.isConstant())
			allConstant.set(False);
	}

	// Specialize on funs with all-constant parameters that are non-'summon' as these will likely evaluate to constants.
	// TODO: also require return type to be immutable
	const Bool isConstant = _and(allConstant.get(), !isSummon);

	// Always specialize on a fun, even if fun is summon
	return isConstant || someFun.get()
		? yesSpecialize(ctx, range, args, isConstant)
		: dontSpecialize(ctx, range, args);
}

const SpecializeOnArgs getSpecializeOnArgsForFun(ConcretizeCtx& ctx, const SourceRange range, const FunDecl* f, const Arr<const ConstantOrExpr> args) {
	// Don't specialize just because a single arg is a constant.
	// If *every* arg is a constant, we always specialize on all args.
	// Else, specialize if some arg has a known lambda body. (TODO: and that lambda is *only* called.)

	// Never specialize on 'call' -- though we do treat it specially in 'concretizeCall'
	return f->isExtern() || isNonSpecializableBuiltin(f) || isCallFun(ctx, f)
		? dontSpecialize(ctx, range, args)
		: getSpecializeOnArgsForLambdaCall(ctx, range, args, f->isSummon());
}

namespace {
	template <typename ForVariable>
	const Opt<const ConcreteType> getSpecializedParamType(const ConstantOrLambdaOrVariable clv, ForVariable forVariable) {
		return clv.match(
			[&](const ConstantOrLambdaOrVariable::Variable) {
				return some<const ConcreteType>(forVariable());
			},
			[&](const Constant*) {
				return none<const ConcreteType>();
			},
			[&](const KnownLambdaBody* klb) {
				return klb->closureType();
			});
	}
}

const Arr<const ConcreteField> concretizeClosureFieldsAndSpecialize(
	ConcretizeCtx& ctx,
	const Arr<const ClosureField*> closure,
	const Arr<const ConstantOrLambdaOrVariable> closureSpecialize,
	const TypeArgsScope typeArgsScope
) {
	ArrBuilder<const ConcreteField> res;
	for (const size_t i : Range{closure.size}) {
		const ClosureField* c = at(closure, i);
		const Opt<const ConcreteType> t = getSpecializedParamType(at(closureSpecialize, i), [&]() {
			return getConcreteType(ctx, c->type, typeArgsScope);
		});
		if (t.has())
			res.add(ctx.arena, ConcreteField{/*isMutable*/ False, copyStr(ctx.arena, c->name), t.force()});
	}
	return res.finish();
}

const Arr<const ConcreteParam> concretizeParamsAndSpecialize(
	ConcretizeCtx& ctx,
	const Arr<const Param> params,
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs,
	const TypeArgsScope typeArgsScope
) {
	assert(params.size == specializeOnArgs.size);
	// TODO: mapOpZip helper
	ArrBuilder<const ConcreteParam> res {};
	for (const size_t i : Range{params.size}) {
		const Param p = at(params, i);
		const Opt<const ConcreteType> t = getSpecializedParamType(at(specializeOnArgs, i), [&]() {
			return getConcreteType(ctx, p.type, typeArgsScope);
		});
		if (t.has())
			res.add(ctx.arena, ConcreteParam{mangleName(ctx.arena, p.name), t.force()});
	}
	return res.finish();
}

// This is for instantiating a KnownLambdaBody.
const Arr<const ConcreteParam> specializeParamsForLambdaInstance(
	ConcretizeCtx& ctx,
	const Arr<const ConcreteParam> nonSpecializedParams,
	const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs
) {
	ArrBuilder<const ConcreteParam> res {};
	//TODO: 'zip' helper
	for (const size_t i : Range{nonSpecializedParams.size}) {
		const ConcreteParam p = at(nonSpecializedParams, i);
		const Opt<const ConcreteType> t = getSpecializedParamType(at(specializeOnArgs, i), [&]() { return p.type; });
		if (t.has())
			res.add(ctx.arena, ConcreteParam{p.mangledName, t.force()});
	}
	return res.finish();
}

const Arr<const ConcreteParam> concretizeParamsNoSpecialize(ConcretizeCtx& ctx, const Arr<const Param> params, const TypeArgsScope typeArgsScope) {
	return concretizeParamsAndSpecialize(ctx, params, allVariable(ctx.arena, params.size), typeArgsScope);
}

const ConstantOrExpr makeLambdasDynamic(ConcretizeCtx& ctx, const SourceRange range, const ConstantOrExpr expr) {
	return expr.match(
		[&](const Constant* c) {
			return makeLambdasDynamic_forConstant(ctx, range, c);
		},
		[&](const ConcreteExpr* e) {
			return makeLambdasDynamic_forExpr(ctx, range, e);
		});
}

const Arr<const ConstantOrExpr> makeLambdasDynamic_arr(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> exprs) {
	return map<const ConstantOrExpr>{}(ctx.arena, exprs, [&](const ConstantOrExpr expr) {
		return makeLambdasDynamic(ctx, range, expr);
	});
}

const Bool isNonSpecializableBuiltin(const FunDecl* f) {
	return _and(f->body().isBuiltin(), getBuiltinFunInfo(f->sig).isNonSpecializable);
}
