#pragma once

#include "../concreteModel.h"
#include "./writer.h"

// TODO: we actually just care if arguments have no side-effects.
const Bool allConstant(const Arr<const ConstantOrExpr> args);
const Bool allConstant(const Arr<const ConstantOrLambdaOrVariable> args);

const Opt<const Arr<const Constant*>> tryGetAllConstant(Arena& arena, const Arr<const ConstantOrExpr> args);
const Opt<const Arr<const Constant*>> tryGetAllConstant(Arena& arena, const Arr<const ConstantOrLambdaOrVariable> args);

const Arr<const ConstantOrLambdaOrVariable> allVariable(Arena& arena, const size_t size);

inline const Opt<const KnownLambdaBody*> getKnownLambdaBodyFromConstantOrExpr(const ConstantOrExpr e) {
	return e.match(
		[](const Constant* c) {
			return c->kind.isLambda()
				? some<const KnownLambdaBody*>(c->kind.asLambda().knownLambdaBody)
				: none<const KnownLambdaBody*>();
		},
		[](const ConcreteExpr* e) {
			return e->knownLambdaBody();
		});
}

inline const Opt<const KnownLambdaBody*> getKnownLambdaBodyFromConstantOrLambdaOrVariable(const ConstantOrLambdaOrVariable e) {
	return e.match(
		[](const ConstantOrLambdaOrVariable::Variable) {
			return none<const KnownLambdaBody*>();
		},
		[](const Constant* c) {
			return c->kind.isLambda()
				? some<const KnownLambdaBody*>(c->kind.asLambda().knownLambdaBody)
				: none<const KnownLambdaBody*>();
		},
		[](const KnownLambdaBody* klb) {
			return some<const KnownLambdaBody*>(klb);
		});
}

inline const ConstantOrLambdaOrVariable constantOrLambdaOrVariableFromConcreteExpr(const ConcreteExpr e) {
	return e.knownLambdaBody().has()
		? ConstantOrLambdaOrVariable{e.knownLambdaBody().force()}
		: ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}};
}

inline const ConstantOrLambdaOrVariable constantOrLambdaOrVariableFromConstantOrExpr(const ConstantOrExpr e) {
	return e.match(
		[](const Constant* c) {
			return ConstantOrLambdaOrVariable{c};
		},
		[](const ConcreteExpr* e) {
			return constantOrLambdaOrVariableFromConcreteExpr(*e);
		});
}

inline const Opt<const KnownLambdaBody*> knownLambdaBodyFromConcreteFunBody(const ConcreteFunBody body) {
	// assume builtins dont' return lambdas
	return body.isConcreteExpr()
		? body.asConcreteExpr()->knownLambdaBody()
		: none<const KnownLambdaBody*>();
}

inline const Opt<const KnownLambdaBody*> knownLambdaBodyFromConstantOrExpr(const ConstantOrExpr body) {
	return body.isConcreteExpr()
		? body.asConcreteExpr()->knownLambdaBody()
		: none<const KnownLambdaBody*>();
}

inline const ConcreteFunBody toConcreteFunBody(const ConstantOrExpr ce) {
	return ce.match(
		[](const Constant* c) {
			return ConcreteFunBody{c};
		},
		[](const ConcreteExpr* e) {
			return ConcreteFunBody{e};
		});
}

// TODO: does this really belong here?
void writeConcreteTypeForMangle(Writer& writer, const ConcreteType t);
