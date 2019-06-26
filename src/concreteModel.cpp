#include "./concreteModel.h"

#include "./util/arrUtil.h"

// This determines whether the tyep must be by-reference
const Bool ConcreteStruct::isSelfMutable() const {
	return body().match(
		[](const ConcreteStructBody::Builtin) {
			return False;
		},
		[](const ConcreteStructBody::Fields f) {
			return exists(f.fields, [](const ConcreteField f) {
				return f.isMutable;
			});
		},
		[](const ConcreteStructBody::Union) {
			return False;
		},
		[](const ConcreteStructBody::Iface) {
			return False;
		});
}

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
			Writer writer { arena };
			writeStatic(writer, "CallConcreteFun argument type mismatch for ");
			writeStr(writer, c->mangledName());
			writeStatic(writer, "\nExpected: ");
			writeConcreteType(writer, param.type);
			writeStatic(writer, "\nActual: ");
			if (argType.has())
				writeConcreteType(writer, argType.force());
			else
				writeStatic(writer, "<<none>>");
			writeChar(writer, '\n');
			printf("%s", writer.finishCStr());
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

void writeConstant(Writer& writer, const Constant* c) {
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

void writeConstantOrLambdaOrVariable(Writer& writer, const ConstantOrLambdaOrVariable clv) {
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
