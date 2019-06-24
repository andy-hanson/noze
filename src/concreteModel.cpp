#include "./concreteModel.h"

#include "./util/arrUtil.h"

ConstantKind::Lambda::Lambda(const KnownLambdaBody* klb) : knownLambdaBody{klb} {
	// If it needs a closure it can't be a constant
	// (constant things closed over are omitted from the closure)
	assert(!klb->hasClosure());
}

ConcreteExpr::CallConcreteFun::CallConcreteFun(const ConcreteFun* c, const Arr<const ConstantOrExpr> a) : called{c}, args{a} {
	assert(called->arityExcludingCtxIncludingClosure() == args.size);

	if (called->closureParam.has()) {
		assert(concreteTypeEq(args[0].typeWithKnownLambdaBody().force(), called->closureParam.force().type));
	}
	for (const size_t i : Range{called->paramsExcludingClosure().size}) {
		const ConcreteParam param = called->paramsExcludingClosure()[i];
		const size_t i2 = i + (called->closureParam.has() ? 1 : 0);
		// If the arg has a knownlambdabody but no closure, we shouldn't bother passing it.
		const Opt<const ConcreteType> argType = args[i2].typeWithKnownLambdaBody();
		if (!argType.has() || !concreteTypeEq(argType.force(), param.type)) {
			Arena arena {};
			Output out {};
			out << "CallConcreteFun argument type mismatch for "
				<< c->mangledName()
				<< "\nExpected: "
				<< param.type
				<<  "\nActual: ";
			if (argType.has())
				out << argType.force();
			else
				out << "<<none>>";
			out << "\n";
			assert(0);
		}
	}
}

ConcreteExpr::LambdaToDynamic::LambdaToDynamic(const ConcreteFun* _fun, const ConstantOrExpr _closure)
	: fun{_fun}, closure{_closure} {
	assert(fun->hasClosure()); // All dynamic funs take a closure, even if it's just void*
	const ConcreteType closureType = closure.typeWithoutKnownLambdaBody();
	assert(closureType.sizeOrPointerSize() == sizeof(void*));
	assert(concreteTypeEq(closureType, fun->closureType().force()));
	closure.match(
		[&](const Constant* c) {
			assert(c->kind.isNull());
		},
		[&](const ConcreteExpr*) {
			// assert(e->knownLambdaBody().has()); // Not true for Alloc
		});
}

const Constant* ConstantKind::Ptr::deref() const {
	return array->kind.asArray().elements()[index];
}

Output& operator<<(Output& out, const Constant* c) {
	c->kind.match(
		[&](const ConstantKind::Array a) {
			out << '[';
			writeWithCommas(out, a.elements());
			out << ']';
		},
		[&](const Bool b) {
			out << b;
		},
		[&](const char c) {
			out << '\'';
			writeEscapedChar(out, c);
			out << '\'';
		},
		[&](const ConstantKind::FunPtr) {
			todo<void>("output constant funptr");
		},
		[&](const Int64 i) {
			out << i;
		},
		[&](const ConstantKind::Lambda) {
			out << "constant_lambda";
		},
		[&](const Nat64 n) {
			out << n;
		},
		[&](const ConstantKind::Null) {
			out << "null";
		},
		[&](const ConstantKind::Ptr p) {
			out << "ptr" << p.index;
		},
		[&](const ConstantKind::Record) {
			todo<void>("output constant record");
		},
		[&](const ConstantKind::Union) {
			todo<void>("output constant union");
		},
		[&](const ConstantKind::Void) {
			out << "void";
		});
	return out;
}

Output& operator<<(Output& out, const ConstantOrLambdaOrVariable clv) {
	clv.match(
		[&](const ConstantOrLambdaOrVariable::Variable) {
			out << "variable";
		},
		[&](const Constant* c) {
			out << c;
		},
		[&](const KnownLambdaBody*) {
			out << "some_lambda";
		});
	return out;
}

const ConcreteType ConstantOrExpr::typeWithoutKnownLambdaBody() const {
	return match(
		[](const Constant* c) {
			return c->type();
		},
		[](const ConcreteExpr* e) {
			return e->typeWithoutKnownLambdaBody();
		});
}

const Opt<const ConcreteType> ConstantOrExpr::typeWithKnownLambdaBody() const {
	return match(
		[](const Constant* c) {
			return c->kind.isLambda()
				? none<const ConcreteType>()
				: some<const ConcreteType>(c->type());
		},
		[](const ConcreteExpr* e) {
			return e->typeWithKnownLambdaBody();
		});
}
