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

	const Bool isInt16(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("int16"));
	}

	const Bool isInt32(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("int32"));
	}

	const Bool isInt64(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("int"));
	}

	const Bool isNat64(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("nat"));
	}

	const Bool isNat32(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("nat32"));
	}

	const Bool isNat16(const Type t) {
		return isNamed(t, shortSymAlphaLiteral("nat16"));
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
				case shortSymAlphaLiteralValue("fun-ptr5"):
				case shortSymAlphaLiteralValue("fun-ptr6"):
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
		const Type p0 = isEmpty(sig.params)
			? Type{Type::Bogus{}}
			: at(sig.params, 0).type;
		const Type p1 = size(sig.params) <= 1
			? Type{Type::Bogus{}}
			: at(sig.params, 1).type;

		Arena tempArena {};

		switch (name.value) {
			case shortSymOperatorLiteralValue("<=>"):
				return generate(BuiltinFunKind::compare);
			case shortSymOperatorLiteralValue("+"):
				return isFloat64(rt) ? _operator(BuiltinFunKind::addFloat64)
					: isPtr(rt) ? _operator(BuiltinFunKind::addPtr)
					: no;
			case shortSymOperatorLiteralValue("-"):
				return isFloat64(rt) ? _operator(BuiltinFunKind::subFloat64)
					: isPtr(rt) && isPtr(p0) && isNat64(p1) ? _operator(BuiltinFunKind::subPtrNat)
					: no;
			case shortSymOperatorLiteralValue("*"):
				return isFloat64(rt) ? _operator(BuiltinFunKind::mulFloat64) : no;

			case shortSymAlphaLiteralValue("and"):
				return _operator(BuiltinFunKind::_and);
			case shortSymAlphaLiteralValue("as"):
				return _operator(BuiltinFunKind::as);
			case shortSymAlphaLiteralValue("as-any-ptr"):
				return _operator(BuiltinFunKind::asAnyPtr);
			case shortSymAlphaLiteralValue("as-ref"):
				return _operator(BuiltinFunKind::asRef);
			case shortSymAlphaLiteralValue("bits-and"):
				return isNat16(rt) ? _operator(BuiltinFunKind::bitwiseAndNat16)
					: isNat32(rt) ? _operator(BuiltinFunKind::bitwiseAndNat32)
					: isNat64(rt) ? _operator(BuiltinFunKind::bitwiseAndNat64)
					: no;
			case shortSymAlphaLiteralValue("call"):
				return isSomeFunPtr(p0) ? _operator(BuiltinFunKind::callFunPtr) : no;
			case shortSymAlphaLiteralValue("deref"):
				return _operator(BuiltinFunKind::deref);
			case shortSymAlphaLiteralValue("false"):
				return constant(BuiltinFunKind::_false);
			case shortSymAlphaLiteralValue("get-ctx"):
				return _operator(BuiltinFunKind::getCtx);
			case shortSymAlphaLiteralValue("get-errno"):
				return special(BuiltinFunKind::getErrno);
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
					: isInt16(rt) ? constant(BuiltinFunKind::oneInt16)
					: isInt32(rt) ? constant(BuiltinFunKind::oneInt32)
					: isInt64(rt) ? constant(BuiltinFunKind::oneInt64)
					: isNat16(rt) ? constant(BuiltinFunKind::oneNat16)
					: isNat32(rt) ? constant(BuiltinFunKind::oneNat32)
					: isNat64(rt) ? constant(BuiltinFunKind::oneNat64)
					: no;
			case shortSymAlphaLiteralValue("or"):
				return _operator(BuiltinFunKind::_or);
			case shortSymAlphaLiteralValue("pass"):
				return isVoid(rt) ? constant(BuiltinFunKind::pass) : no;
			case shortSymAlphaLiteralValue("ptr-cast"):
				return _operator(BuiltinFunKind::ptrCast, /*isNonSpecializable*/ True);
			case shortSymAlphaLiteralValue("ptr-to"):
				return _operator(BuiltinFunKind::ptrTo, /*isNonSpecializable*/ True);
			case shortSymAlphaLiteralValue("ref-of-val"):
				return _operator(BuiltinFunKind::refOfVal, /*isNonSpecializable*/ True);
			case shortSymAlphaLiteralValue("set"):
				return isPtr(p0) ? _operator(BuiltinFunKind::setPtr) : no;
			case shortSymAlphaLiteralValue("size-of"):
				return constant(BuiltinFunKind::sizeOf);
			case shortSymAlphaLiteralValue("to-int"):
				return isInt16(p0) ? _operator(BuiltinFunKind::toIntFromInt16)
					: isInt32(p0) ? _operator(BuiltinFunKind::toIntFromInt32)
					: no;
			case shortSymAlphaLiteralValue("to-nat"):
				return isNat16(p0) ? _operator(BuiltinFunKind::toNatFromNat16)
					: isNat32(p0) ? _operator(BuiltinFunKind::toNatFromNat32)
					: isPtr(p0) ? _operator(BuiltinFunKind::toNatFromPtr)
					: no;
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
				return isInt16(rt) ? _operator(BuiltinFunKind::wrapAddInt16)
					: isInt32(rt) ? _operator(BuiltinFunKind::wrapAddInt32)
					: isInt64(rt) ? _operator(BuiltinFunKind::wrapAddInt64)
					: isNat16(rt) ? _operator(BuiltinFunKind::wrapAddNat16)
					: isNat32(rt) ? _operator(BuiltinFunKind::wrapAddNat32)
					: isNat64(rt) ? _operator(BuiltinFunKind::wrapAddNat64)
					: no;
			case shortSymAlphaLiteralValue("wrap-sub"):
				return isInt16(rt) ? _operator(BuiltinFunKind::wrapSubInt16)
					: isInt32(rt) ? _operator(BuiltinFunKind::wrapSubInt32)
					: isInt64(rt) ? _operator(BuiltinFunKind::wrapSubInt64)
					: isNat16(rt) ? _operator(BuiltinFunKind::wrapSubNat16)
					: isNat32(rt) ? _operator(BuiltinFunKind::wrapSubNat32)
					: isNat64(rt) ? _operator(BuiltinFunKind::wrapSubNat64)
					: no;
			case shortSymAlphaLiteralValue("wrap-mul"):
				return isInt64(rt) ? _operator(BuiltinFunKind::wrapMulInt64)
					: isNat32(rt) ? _operator(BuiltinFunKind::wrapMulNat32)
					: isNat64(rt) ? _operator(BuiltinFunKind::wrapMulNat64)
					: no;
			case shortSymAlphaLiteralValue("zero"):
				return isFloat64(rt) ? todo<const Opt<const BuiltinFunInfo>>("zero float")
					: isInt16(rt) ? constant(BuiltinFunKind::zeroInt16)
					: isInt32(rt) ? constant(BuiltinFunKind::zeroInt32)
					: isInt64(rt) ? constant(BuiltinFunKind::zeroInt64)
					: isNat16(rt) ? constant(BuiltinFunKind::zeroNat16)
					: isNat32(rt) ? constant(BuiltinFunKind::zeroNat32)
					: isNat64(rt) ? constant(BuiltinFunKind::zeroNat64)
					: no;
			default:
				if (symEqLongAlphaLiteral(name, "as-non-const"))
					return _operator(BuiltinFunKind::asNonConst, True);
				else if (symEqLongAlphaLiteral(name, "compare-exchange-strong"))
					return special(BuiltinFunKind::compareExchangeStrong);
				else if (symEqLongAlphaLiteral(name, "is-reference-type"))
					return constant(BuiltinFunKind::isReferenceType);
				else if (symEqLongAlphaLiteral(name, "unsafe-to-int"))
					return isNat64(p0) ? _operator(BuiltinFunKind::unsafeNat64ToInt64) : no;
				else if (symEqLongAlphaLiteral(name, "unsafe-to-nat"))
					return isInt64(p0) ? _operator(BuiltinFunKind::unsafeInt64ToNat64) : no;
				else if (symEqLongAlphaLiteral(name, "unsafe-to-nat32"))
					return isNat64(p0) ? _operator(BuiltinFunKind::unsafeNat64ToNat32) : no;
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
		case shortSymAlphaLiteralValue("fun-ptr5"):
		case shortSymAlphaLiteralValue("fun-ptr6"):
			return BuiltinStructInfo{BuiltinStructKind::funPtrN, sizeof(void(*))};
		case shortSymAlphaLiteralValue("int16"):
			return BuiltinStructInfo{BuiltinStructKind::int16, sizeof(Int16)};
		case shortSymAlphaLiteralValue("int32"):
			return BuiltinStructInfo{BuiltinStructKind::int32, sizeof(Int32)};
		case shortSymAlphaLiteralValue("int"):
			return BuiltinStructInfo{BuiltinStructKind::int64, sizeof(Int64)};
		case shortSymAlphaLiteralValue("nat16"):
			return BuiltinStructInfo{BuiltinStructKind::nat16, sizeof(Nat16)};
		case shortSymAlphaLiteralValue("nat32"):
			return BuiltinStructInfo{BuiltinStructKind::nat32, sizeof(Nat32)};
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
