#include "./concreteModel.h"

#include "./concretize/concretizeUtil.h" // getKnownLambdaBodyFromConstantOrExpr
#include "./util/arrUtil.h"

ConstantKind::Lambda::Lambda(const KnownLambdaBody* klb) : knownLambdaBody{klb} {
	// If it needs a closure it can't be a constant
	// (constant things closed over are omitted from the closure)
	assert(!klb->hasClosure());
}

ConcreteExpr::Call::Call(const ConcreteFun* c, const Arr<const ConstantOrExpr> a) : called{c}, args{a} {
	assert(called->arityExcludingCtxIncludingClosure() == size(args));

	if (has(called->closureParam)) {
		assert(concreteTypeEq(force(at(args, 0).typeWithKnownLambdaBody()), force(called->closureParam).type));
	}
	for (const size_t i : Range{size(called->paramsExcludingCtxAndClosure())}) {
		const ConcreteParam param = at(called->paramsExcludingCtxAndClosure(), i);
		// If the arg has a knownlambdabody but no closure, we shouldn't bother passing it.
		const ConstantOrExpr arg = at(args, i + boolToNat(has(called->closureParam)));
		const Opt<const ConcreteType> argType = arg.typeWithKnownLambdaBody();
		if (!has(argType) || !concreteTypeEq(force(argType), param.type)) {
			Arena arena {};
			Writer writer { &arena };
			writeStatic(&writer, "Call argument type mismatch for ");
			writeStr(&writer, c->mangledName());
			writeStatic(&writer, "\nExpected: ");
			writeConcreteType(&writer, param.type);
			writeStatic(&writer, "\nActual: ");
			if (has(argType))
				writeConcreteType(&writer, force(argType));
			else
				writeStatic(&writer, "<<none>>");
			writeChar(&writer, '\n');
			debugPrint(finishWriterToCStr(&writer));
			assert(0);
		}
	}
}

ConcreteExpr::Cond::Cond(const ConcreteExpr* _cond, const ConstantOrExpr _then, const ConstantOrExpr _elze)
	: cond{_cond}, then{_then}, elze{_elze} {
	// Can't specialize with KnownLambdaBody when there are different branches
	assert(!has(getKnownLambdaBodyFromConstantOrExpr(then)) && !has(getKnownLambdaBodyFromConstantOrExpr(elze)));
}

ConcreteExpr::LambdaToDynamic::LambdaToDynamic(const ConcreteFun* _fun, const ConstantOrExpr _closure)
	: fun{_fun}, closure{_closure} {
	assert(fun->hasClosure()); // All dynamic funs take a closure, even if it's just void*
	const ConcreteType closureType = closure.typeWithoutKnownLambdaBody();
	assert(closureType.sizeOrPointerSizeBytes() == sizeof(void*));
	assert(concreteTypeEq(closureType, force(fun->closureType())));
	closure.match(
		[&](const Constant* c) {
			assert(c->kind.isNull());
		},
		[&](const ConcreteExpr*) {
			// assert(has(e->knownLambdaBody())); // Not true for Alloc
		});
}

const Constant* ConstantKind::Ptr::deref() const {
	return at(array->kind.asArray().elements(), index);
}

void writeConstant(Writer* writer, const Constant* c) {
	c->kind.match(
		[&](const ConstantKind::Array a) {
			writeChar(writer, '[');
			writeWithCommas(writer, a.elements(), [&](const Constant* element) {
				writeConstant(writer, element);
			});
			writeChar(writer, ']');
		},
		[&](const Bool b) {
			writeBool(writer, b);
		},
		[&](const char c) {
			writeChar(writer, '\'');
			writeEscapedChar(writer, c);
			writeChar(writer, '\'');
		},
		[&](const ConstantKind::FunPtr) {
			todo<void>("output constant funptr");
		},
		[&](const Int64 i) {
			writeInt(writer, i);
		},
		[&](const ConstantKind::Lambda) {
			writeStatic(writer, "some_constant_lambda");
		},
		[&](const Nat64 n) {
			writeNat(writer, n);
		},
		[&](const ConstantKind::Null) {
			writeStatic(writer, "null");
		},
		[&](const ConstantKind::Ptr p) {
			writeStatic(writer, "ptr");
			writeNat(writer, p.index);
		},
		[&](const ConstantKind::Record) {
			writeStatic(writer, "some_constant_record\n");
			//todo<void>("output constant record");
		},
		[&](const ConstantKind::Union) {
			todo<void>("output constant union");
		},
		[&](const ConstantKind::Void) {
			writeStatic(writer, "void");
		});
}

void writeConstantOrLambdaOrVariable(Writer* writer, const ConstantOrLambdaOrVariable clv) {
	clv.match(
		[&](const ConstantOrLambdaOrVariable::Variable) {
			writeStatic(writer, "variable");
		},
		[&](const Constant* c) {
			writeConstant(writer, c);
		},
		[&](const KnownLambdaBody*) {
			writeStatic(writer, "some_lambda");
		});
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
