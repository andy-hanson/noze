#include "./builtinInfo.h"

namespace {
	const Bool isNamed(const Type t, const Sym name) {
		return _and(
			t.isStructInst(),
			symEq(t.asStructInst()->decl->name, name));
	}

	const Bool isFloat64(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("float"));
	}

	const Bool isInt64(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("int"));
	}

	const Bool isNat64(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("nat"));
	}

	const Bool isPtr(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("ptr"));
	}

	const Bool isVoid(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("void"));
	}

	const Bool isSomeFunPtr(const Type t) {
		if (!t.isStructInst())
			return False;
		else
			switch (t.asStructInst()->decl->name.value) {
				case shortSymAlphaLiteralValue("fun-ptr0"):
				case shortSymAlphaLiteralValue("fun-ptr1"):
				case shortSymAlphaLiteralValue("fun-ptr2"):
				case shortSymAlphaLiteralValue("fun-ptr3"):
				case shortSymAlphaLiteralValue("fun-ptr4"):
					return True;
				default:
					return False;
			}
	}

	const Opt<const BuiltinFunInfo> generate(const BuiltinFunKind kind) {
		return some<const BuiltinFunInfo>(BuiltinFunInfo{BuiltinFunEmit::generate, kind, False});
	}

	const Opt<const BuiltinFunInfo> special(const BuiltinFunKind kind) {
		return some<const BuiltinFunInfo>(BuiltinFunInfo{BuiltinFunEmit::special, kind, False});
	}

	const Opt<const BuiltinFunInfo> _operator(const BuiltinFunKind kind, const Bool isNonSpecializable = False) {
		return some<const BuiltinFunInfo>(BuiltinFunInfo{BuiltinFunEmit::_operator, kind, isNonSpecializable});
	}

	const Opt<const BuiltinFunInfo> constant(const BuiltinFunKind kind) {
		return some<const BuiltinFunInfo>(BuiltinFunInfo{BuiltinFunEmit::constant, kind, False});
	}

	const Opt<const BuiltinFunInfo> tryGetBuiltinFunInfo(const Sig sig) {
		const Sym name = sig.name;
		const Opt<const BuiltinFunInfo> no = none<const BuiltinFunInfo>();
		const Type rt = sig.returnType;
		const Type p0 = sig.params.size > 0 ? at(sig.params, 0).type : Type{Type::Bogus{}};

		Arena tempArena {};

		switch (name.value) {
			case shortSymOperatorLiteralValue("<=>"):
				return generate(BuiltinFunKind::compare);
			case shortSymOperatorLiteralValue("+"):
				return isFloat64(rt) ? _operator(BuiltinFunKind::addFloat64)
					: isPtr(rt) ? _operator(BuiltinFunKind::addPtr)
					: no;
			case shortSymOperatorLiteralValue("-"):
				return isFloat64(rt) ? _operator(BuiltinFunKind::subFloat64) : no;
			case shortSymOperatorLiteralValue("*"):
				return isFloat64(rt) ? _operator(BuiltinFunKind::mulFloat64) : no;

			case shortSymAlphaLiteralValue("and"):
				return _operator(BuiltinFunKind::_and);
			case shortSymAlphaLiteralValue("as"):
				return _operator(BuiltinFunKind::as);
			case shortSymAlphaLiteralValue("call"):
				return isSomeFunPtr(p0) ? _operator(BuiltinFunKind::callFunPtr) : no;
			case shortSymAlphaLiteralValue("deref"):
				return _operator(BuiltinFunKind::deref);
			case shortSymAlphaLiteralValue("false"):
				return constant(BuiltinFunKind::_false);
			case shortSymAlphaLiteralValue("get-ctx"):
				return _operator(BuiltinFunKind::getCtx);
			case shortSymAlphaLiteralValue("hard-fail"):
				return special(BuiltinFunKind::hardFail);
			case shortSymAlphaLiteralValue("if"):
				return _operator(BuiltinFunKind::_if);
			case shortSymAlphaLiteralValue("not"):
				return _operator(BuiltinFunKind::_not);
			case shortSymAlphaLiteralValue("null"):
				return constant(BuiltinFunKind::null);
			case shortSymAlphaLiteralValue("one"):
				return isFloat64(rt) ? todo<const Opt<const BuiltinFunInfo>>("one float")
					: isInt64(rt) ? constant(BuiltinFunKind::oneInt64)
					: isNat64(rt) ? constant(BuiltinFunKind::oneNat64)
					: no;
			case shortSymAlphaLiteralValue("or"):
				return _operator(BuiltinFunKind::_or);
			case shortSymAlphaLiteralValue("pass"):
				return isVoid(rt) ? constant(BuiltinFunKind::pass) : no;
			case shortSymAlphaLiteralValue("ptr-cast"):
				return _operator(BuiltinFunKind::ptrCast);
			case shortSymAlphaLiteralValue("ptr-to"):
				return _operator(BuiltinFunKind::ptrTo);
			case shortSymAlphaLiteralValue("ref-of-val"):
				return _operator(BuiltinFunKind::refOfVal);
			case shortSymAlphaLiteralValue("set"):
				return isPtr(p0) ? _operator(BuiltinFunKind::setPtr) : no;
			case shortSymAlphaLiteralValue("size-of"):
				return constant(BuiltinFunKind::sizeOf);
			case shortSymAlphaLiteralValue("true"):
				return constant(BuiltinFunKind::_true);
			case shortSymAlphaLiteralValue("unsafe-div"):
				return isFloat64(rt) ? _operator(BuiltinFunKind::unsafeDivFloat64)
					: isInt64(rt) ? _operator(BuiltinFunKind::unsafeDivInt64)
					: isNat64(rt) ? _operator(BuiltinFunKind::unsafeDivNat64)
					: no;
			case shortSymAlphaLiteralValue("unsafe-mod"):
				return _operator(BuiltinFunKind::unsafeModNat64);
			case shortSymAlphaLiteralValue("wrap-add"):
				return isInt64(rt) ? _operator(BuiltinFunKind::wrappingAddInt64)
					: isNat64(rt) ? _operator(BuiltinFunKind::wrappingAddNat64)
					: no;
			case shortSymAlphaLiteralValue("wrap-sub"):
				return isInt64(rt) ? _operator(BuiltinFunKind::wrappingSubInt64)
					: isNat64(rt) ? _operator(BuiltinFunKind::wrappingSubNat64)
					: no;
			case shortSymAlphaLiteralValue("wrap-mul"):
				return isInt64(rt) ? _operator(BuiltinFunKind::wrappingMulInt64)
					: isNat64(rt) ? _operator(BuiltinFunKind::wrappingMulNat64)
					: no;
			case shortSymAlphaLiteralValue("zero"):
				return isFloat64(rt) ? todo<const Opt<const BuiltinFunInfo>>("zero float")
					: isInt64(rt) ? constant(BuiltinFunKind::zeroInt64)
					: isNat64(rt) ? constant(BuiltinFunKind::zeroNat64)
					: no;
			default:
				if (symEqLongAlphaLiteral(name, "as-non-const"))
					return _operator(BuiltinFunKind::asNonConst, True);
				else if (symEqLongAlphaLiteral(name, "compare-exchange-strong"))
					return special(BuiltinFunKind::compareExchangeStrong);
				else if (symEqLongAlphaLiteral(name, "is-reference-type"))
					return constant(BuiltinFunKind::isReferenceType);
				else if (symEqLongAlphaLiteral(name, "unsafe-to-nat64"))
					return _operator(BuiltinFunKind::unsafeInt64ToNat64);
				else if (symEqLongAlphaLiteral(name, "unsafe-to-int64"))
					return _operator(BuiltinFunKind::unsafeNat64ToInt64);
				else
					return no;
		}
	}
}

const BuiltinFunInfo getBuiltinFunInfo(const Sig sig) {
	const Opt<const BuiltinFunInfo> res = tryGetBuiltinFunInfo(sig);
	if (!has(res)) {
		Arena arena {};
		printf("not a builtin fun: %s\n", symToCStr(&arena, sig.name));
		return todo<const BuiltinFunInfo>("not a builtin fun");
	}
	return force(res);
}

const BuiltinStructInfo getBuiltinStructInfo(const StructDecl* s) {
	switch (s->name.value) {
		case shortSymAlphaLiteralValue("bool"):
			return BuiltinStructInfo{BuiltinStructKind::_bool, sizeof(Bool)};
		case shortSymAlphaLiteralValue("byte"):
			return BuiltinStructInfo{BuiltinStructKind::byte, sizeof(byte)};
		case shortSymAlphaLiteralValue("char"):
			return BuiltinStructInfo{BuiltinStructKind::_char, sizeof(char)};
		case shortSymAlphaLiteralValue("float"):
			return BuiltinStructInfo{BuiltinStructKind::float64, sizeof(Float64)};
		case shortSymAlphaLiteralValue("fun-ptr0"):
		case shortSymAlphaLiteralValue("fun-ptr1"):
		case shortSymAlphaLiteralValue("fun-ptr2"):
		case shortSymAlphaLiteralValue("fun-ptr3"):
		case shortSymAlphaLiteralValue("fun-ptr4"):
			return BuiltinStructInfo{BuiltinStructKind::funPtrN, sizeof(void(*))};
		case shortSymAlphaLiteralValue("int"):
			return BuiltinStructInfo{BuiltinStructKind::int64, sizeof(Int64)};
		case shortSymAlphaLiteralValue("nat"):
			return BuiltinStructInfo{BuiltinStructKind::nat64, sizeof(Nat64)};
		case shortSymAlphaLiteralValue("ptr"):
			return BuiltinStructInfo{BuiltinStructKind::ptr, sizeof(void*)};
		case shortSymAlphaLiteralValue("void"):
			return BuiltinStructInfo{BuiltinStructKind::_void, sizeof(byte)};
		default:
			return todo<const BuiltinStructInfo>("not a recognized builtin struct");
	}
}
