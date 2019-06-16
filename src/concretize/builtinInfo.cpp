#include "./builtinInfo.h"

namespace {
	const Bool isNamed(const Type t, const char* name) {
		return _and(t.isStructInst(), strEqLiteral(t.asStructInst()->decl->name, name));
	}

	const Bool isFloat64(const Type t) {
		return isNamed(t, "float64");
	}

	const Bool isInt64(const Type t) {
		return isNamed(t, "int64");
	}

	const Bool isNat64(const Type t) {
		return isNamed(t, "nat64");
	}

	const Bool isPtr(const Type t) {
		return isNamed(t, "ptr");
	}

	const Bool isVoid(const Type t) {
		return isNamed(t, "void");
	}

	const Bool isSomeFunPtr(const Type t) {
		if (!t.isStructInst())
			return False;
		else {
			const Str name = t.asStructInst()->decl->name;
			return _and(name.size == 8, strEqLiteral(slice(name, 0, 7), "fun-ptr"));
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
		const Str name = sig.name;
		const Opt<const BuiltinFunInfo> no = none<const BuiltinFunInfo>();
		const Type rt = sig.returnType;
		const Type p0 = sig.params.size > 0 ? sig.params[0].type : Type{Type::Bogus{}};

		switch (name[0]) {
			case '<':
				return strEqLiteral(name, "<=>")
					? generate(BuiltinFunKind::compare)
					: no;
			case '+':
				return strEqLiteral(name, "+")
					? isFloat64(rt) ? _operator(BuiltinFunKind::addFloat64)
						: isPtr(rt) ? _operator(BuiltinFunKind::addPtr)
						: no
					: no;
			case '-':
				return strEqLiteral(name, "-") && isFloat64(rt) ? _operator(BuiltinFunKind::subFloat64) : no;
			case '*':
				return strEqLiteral(name, "*") && isFloat64(rt) ? _operator(BuiltinFunKind::mulFloat64) : no;
			case 'a':
				return strEqLiteral(name, "and") ? _operator(BuiltinFunKind::_and)
					: strEqLiteral(name, "as") ? _operator(BuiltinFunKind::as)
					: strEqLiteral(name, "as-non-const") ? _operator(BuiltinFunKind::asNonConst, True)
					: no;
			case 'c':
				return strEqLiteral(name, "call") && isSomeFunPtr(p0) ? _operator(BuiltinFunKind::callFunPtr) : no;
			case 'd':
				return strEqLiteral(name, "deref") ? _operator(BuiltinFunKind::deref) : no;
			case 'f':
				return strEqLiteral(name, "false") ? constant(BuiltinFunKind::_false) : no;
			case 'g':
				return strEqLiteral(name, "get-ctx") ? _operator(BuiltinFunKind::getCtx) : no;
			case 'h':
				return strEqLiteral(name, "hard-fail") ? special(BuiltinFunKind::hardFail) : no;
			case 'i':
				return strEqLiteral(name, "if") ? special(BuiltinFunKind::_if)
					: strEqLiteral(name, "is-reference-type") ? constant(BuiltinFunKind::isReferenceType)
					: no;
			case 'n':
				return strEqLiteral(name, "not") ? _operator(BuiltinFunKind::_not) : no;
			case 'o':
				return strEqLiteral(name, "one")
						? isFloat64(rt) ? todo<const Opt<const BuiltinFunInfo>>("one float")
						: isInt64(rt) ? constant(BuiltinFunKind::oneInt64)
						: isNat64(rt) ? constant(BuiltinFunKind::oneNat64)
						: no
					: strEqLiteral(name, "or") ? _operator(BuiltinFunKind::_or) : no;
			case 'p':
				return strEqLiteral(name, "pass")
						? isVoid(rt) ? constant(BuiltinFunKind::pass) : no
					: strEqLiteral(name, "ptr-cast") ? _operator(BuiltinFunKind::ptrCast)
					: no;
			case 'r':
				return strEqLiteral(name, "ref-of-val") ? _operator(BuiltinFunKind::refOfVal) : no;
			case 's':
				return strEqLiteral(name, "set")
						? isPtr(p0) ? _operator(BuiltinFunKind::setPtr) : no
					: strEqLiteral(name, "size-of") ? constant(BuiltinFunKind::sizeOf)
					: no;
			case 't':
				return strEqLiteral(name, "true")
					? constant(BuiltinFunKind::_true)
					: no;
			case 'u':
				return strEqLiteral(name, "unsafe-div")
					? isFloat64(rt) ? _operator(BuiltinFunKind::unsafeDivFloat64)
						: isInt64(rt) ? _operator(BuiltinFunKind::unsafeDivInt64)
						: isNat64(rt) ? _operator(BuiltinFunKind::unsafeDivNat64)
						: no
					: no;
			case 'w':
				return strEqLiteral(name, "wrapping-add")
						? isInt64(rt) ? _operator(BuiltinFunKind::wrappingAddInt64)
							: isNat64(rt) ? _operator(BuiltinFunKind::wrappingAddNat64)
							: no
					: strEqLiteral(name, "wrapping-sub")
						? isInt64(rt) ? _operator(BuiltinFunKind::wrappingSubInt64)
							: isNat64(rt) ? _operator(BuiltinFunKind::wrappingSubNat64)
							: no
					: strEqLiteral(name, "wrapping-mul")
						? isInt64(rt) ? _operator(BuiltinFunKind::wrappingMulInt64)
						: isNat64(rt) ? _operator(BuiltinFunKind::wrappingMulNat64)
						: no
					: no;
			case 'z':
				return strEqLiteral(name, "zero")
					? isFloat64(rt) ? todo<const Opt<const BuiltinFunInfo>>("zero float")
						: isInt64(rt) ? constant(BuiltinFunKind::zeroInt64)
						: isNat64(rt) ? constant(BuiltinFunKind::zeroNat64)
						: no
					: no;
			default:
				return no;
		}
	}
}

const BuiltinFunInfo getBuiltinFunInfo(const Sig sig) {
	const Opt<const BuiltinFunInfo> res = tryGetBuiltinFunInfo(sig);
	if (!res.has()) {
		return todo<const BuiltinFunInfo>("not a builtin fun");
	}
	return res.force();
}

const BuiltinStructInfo getBuiltinStructInfo(const StructDecl* s) {
	const Str name = s->name;
	return strEqLiteral(name, "bool") ? BuiltinStructInfo{BuiltinStructKind::_bool, sizeof(Bool)}
		: strEqLiteral(name, "byte") ? BuiltinStructInfo{BuiltinStructKind::byte, sizeof(byte)}
		: strEqLiteral(name, "char") ? BuiltinStructInfo{BuiltinStructKind::_char, sizeof(char)}
		: strEqLiteral(name, "float64") ? BuiltinStructInfo{BuiltinStructKind::float64, sizeof(Float64)}
		: strEqLiteral(name, "fun-ptr0")
			|| strEqLiteral(name, "fun-ptr1")
			|| strEqLiteral(name, "fun-ptr2")
			|| strEqLiteral(name, "fun-ptr3")
			|| strEqLiteral(name, "fun-ptr4")
			? BuiltinStructInfo{BuiltinStructKind::funPtrN, sizeof(void(*))}
		: strEqLiteral(name, "int64") ? BuiltinStructInfo{BuiltinStructKind::int64, sizeof(Int64)}
		: strEqLiteral(name, "nat64") ? BuiltinStructInfo{BuiltinStructKind::nat64, sizeof(Nat64)}
		: strEqLiteral(name, "ptr") ? BuiltinStructInfo{BuiltinStructKind::ptr, sizeof(void*)}
		: strEqLiteral(name, "void") ? BuiltinStructInfo{BuiltinStructKind::_void, sizeof(byte)}
		: todo<const BuiltinStructInfo>("not a recognized builtin struct");
}
