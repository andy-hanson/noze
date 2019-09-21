#pragma once

#include "./util/late.h"
#include "./util/mutDict.h"
#include "./util/sourceRange.h"
#include "./util/writer.h"

#include "./concretize/mangleName.h"

enum class BuiltinStructKind {
	_bool,
	byte,
	_char,
	float64,
	funPtrN, // fun-ptr0, fun-ptr1, etc...
	int16,
	int32,
	int64,
	nat16,
	nat32,
	nat64,
	ptr,
	_void,
};

struct BuiltinStructInfo {
	const BuiltinStructKind kind;
	const size_t sizeBytes;
};

enum class BuiltinFunKind {
	addFloat64,
	addPtr,
	_and,
	as,
	asAnyPtr,
	asNonConst,
	asRef,
	bitShiftLeftInt32,
	bitShiftRightInt32,
	bitwiseAndInt16,
	bitwiseAndInt32,
	bitwiseAndInt64,
	bitwiseAndNat16,
	bitwiseAndNat32,
	bitwiseAndNat64,
	callFunPtr,
	compareExchangeStrong,
	compare, // the `<=>` operator
	deref,
	_false,
	getCtx,
	getErrno,
	hardFail,
	_if,
	isReferenceType,
	mulFloat64,
	_not,
	null,
	oneInt16,
	oneInt32,
	oneInt64,
	oneNat16,
	oneNat32,
	oneNat64,
	_or,
	pass,
	ptrCast,
	ptrTo,
	refOfVal,
	setPtr,
	sizeOf,
	subFloat64,
	subPtrNat,
	toIntFromInt16,
	toIntFromInt32,
	toNatFromNat16,
	toNatFromNat32,
	toNatFromPtr,
	_true,
	unsafeDivFloat64,
	unsafeDivInt64,
	unsafeDivNat64,
	unsafeInt64ToNat64,
	unsafeInt64ToInt16,
	unsafeInt64ToInt32,
	unsafeModNat64,
	unsafeNat64ToInt64,
	unsafeNat64ToNat32,
	unsafeNat64ToNat16,
	wrapAddInt16,
	wrapAddInt32,
	wrapAddInt64,
	wrapAddNat16,
	wrapAddNat32,
	wrapAddNat64,
	wrapMulInt16,
	wrapMulInt32,
	wrapMulInt64,
	wrapMulNat16,
	wrapMulNat32,
	wrapMulNat64,
	wrapSubInt16,
	wrapSubInt32,
	wrapSubInt64,
	wrapSubNat16,
	wrapSubNat32,
	wrapSubNat64,
	zeroInt16,
	zeroInt32,
	zeroInt64,
	zeroNat16,
	zeroNat32,
	zeroNat64,
};

enum class BuiltinFunEmit {
	generate,
	_operator,
	constant,
	special,
};

struct BuiltinFunInfo {
	const BuiltinFunEmit emit;
	const BuiltinFunKind kind;
	const Bool isNonSpecializable;
};

struct ConcreteField;
struct ConcreteType;
struct ConcreteSig;

struct ConcreteStructBody {
	struct Bogus {};

	struct Builtin {
		const BuiltinStructInfo info;
		const Arr<const ConcreteType> typeArgs;
	};

	struct Record {
		const Arr<const ConcreteField> fields;
	};

	struct Union {
		const Arr<const ConcreteType> members;
	};

private:
	enum class Kind {
		bogus,
		builtin,
		record,
		_union,
	};
	const Kind kind;
	union {
		const Bogus bogus;
		const Builtin builtin;
		const Record record;
		const Union _union;
	};

public:
	explicit inline ConcreteStructBody(const Bogus _bogus) : kind{Kind::bogus}, bogus{_bogus} {}
	explicit inline ConcreteStructBody(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
	explicit inline ConcreteStructBody(const Record _record) : kind{Kind::record}, record{_record} {}
	explicit inline ConcreteStructBody(const Union __union) : kind{Kind::_union}, _union{__union} {}

	inline const Bool isBuiltin() const {
		return enumEq(kind, Kind::builtin);
	}
	inline const Bool isRecord() const {
		return enumEq(kind, Kind::record);
	}
	inline const Bool isUnion() const {
		return enumEq(kind, Kind::_union);
	}
	inline const Builtin asBuiltin() const {
		assert(isBuiltin());
		return builtin;
	}
	inline const Record asRecord() const {
		assert(isRecord());
		return record;
	}
	inline const Union asUnion() const {
		assert(isUnion());
		return _union;
	}

	template <
		typename CbBuiltin,
		typename CbRecord,
		typename CbUnion
	>
	inline auto match(
		CbBuiltin cbBuiltin,
		CbRecord cbRecord,
		CbUnion cbUnion
	) const {
		switch (kind) {
			case Kind::bogus:
				assert(0);
			case Kind::builtin:
				return cbBuiltin(builtin);
			case Kind::record:
				return cbRecord(record);
			case Kind::_union:
				return cbUnion(_union);
			default:
				assert(0);
		}
	}
};

struct ConcreteStruct;

struct ConcreteType {
	// NOTE: ConcreteType for 'ptr' (e.g. 'ptr byte') will *not* have isPointer set -- since it's not a ptr*
	const Bool isPointer;
	const ConcreteStruct* strukt;

	inline const Bool eq(const ConcreteType other) const {
		return _and(isPointer == other.isPointer, ptrEquals(strukt, other.strukt));
	}

	inline const ConcreteStruct* mustBePointer() const {
		assert(isPointer);
		return strukt;
	}

	// Union should never be a pointer
	inline const ConcreteStruct* mustBeNonPointer() const {
		assert(!isPointer);
		return strukt;
	}
};

inline bool concreteTypeEqual(const ConcreteType a, const ConcreteType b) {
	return a.isPointer == b.isPointer && a.strukt == b.strukt;
}

struct SpecialStructInfo {
	enum class Kind {
		arr,
	};
	const Kind kind;
	const ConcreteType elementType;
};

struct ConcreteStructInfo {
	const ConcreteStructBody body;
	const size_t sizeBytes;
	const Bool isSelfMutable;
	const Bool defaultIsPointer;
};

struct ConcreteStruct {
	const Str mangledName;
	const Opt<const SpecialStructInfo> special;
	Late<const ConcreteStructInfo> _info;

	ConcreteStruct(const ConcreteStruct&) = delete;

	inline ConcreteStruct(const Str mn, const Opt<const SpecialStructInfo> sp) : mangledName{mn}, special{sp} {}
	inline ConcreteStruct(const Str mn, const Opt<const SpecialStructInfo> sp, const ConcreteStructInfo info)
		: mangledName{mn}, special{sp}, _info{info} {}

	inline const ConcreteStructInfo info() const {
		return lateGet(&_info);
	}

	inline const ConcreteStructBody body() const {
		return info().body;
	}

	inline size_t sizeBytes() const {
		return info().sizeBytes;
	}

	inline const Bool isSelfMutable() const {
		return info().isSelfMutable;
	}

	inline const Bool defaultIsPointer() const {
		return info().defaultIsPointer;
	}

	inline const Bool isRecord() const {
		return body().isRecord();
	}

	inline const Bool isUnion() const {
		return body().isUnion();
	}
};

inline const Opt<const ConcreteType> getConcreteTypeAsArrElementType(const ConcreteType t) {
	if (!t.isPointer && has(t.strukt->special)) {
		const SpecialStructInfo s = force(t.strukt->special);
		return s.kind == SpecialStructInfo::Kind::arr
			? some<const ConcreteType>(s.elementType)
			: none<const ConcreteType>();
	} else
		return none<const ConcreteType>();
}

inline const Bool concreteTypeIsArr(const ConcreteType t) {
	return has(getConcreteTypeAsArrElementType(t));
}

inline size_t sizeOrPointerSizeBytes(const ConcreteType t) {
	return t.isPointer ? sizeof(void*) : t.strukt->sizeBytes();
}

inline const ConcreteType concreteType_pointer(const ConcreteStruct* strukt) {
	return ConcreteType{True, strukt};
}

inline const ConcreteType concreteType_value(const ConcreteStruct* strukt) {
	return ConcreteType{False, strukt};
}

inline const ConcreteType byRef(const ConcreteType t) {
	return concreteType_pointer(t.strukt);
}

inline const ConcreteType byVal(const ConcreteType t) {
	return concreteType_value(t.strukt);
}

inline const ConcreteType changeToByRef(const ConcreteType t) {
	assert(!t.isPointer);
	return byRef(t);
}

inline const ConcreteType concreteType_fromStruct(const ConcreteStruct* s) {
	return ConcreteType{s->defaultIsPointer(), s};
}

inline void writeConcreteType(Writer* writer, const ConcreteType t) {
	writeStr(writer, t.strukt->mangledName);
	if (t.isPointer)
		writeChar(writer, '*');
}

inline Comparison compareConcreteType(const ConcreteType a, const ConcreteType b) {
	const Comparison res = comparePtr(a.strukt, b.strukt);
	return res != Comparison::equal ? res : compareBool(a.isPointer, b.isPointer);
}

inline Bool concreteTypeEq(const ConcreteType a, const ConcreteType b) {
	return enumEq(compareConcreteType(a, b), Comparison::equal);
}

struct ConcreteField {
	const Bool isMutable;
	const Str mangledName;
	const ConcreteType type;
};

struct ConcreteParam {
	const Str mangledName;
	const ConcreteType type;

	inline ConcreteParam(const Str _mangledName, const ConcreteType _type)
		: mangledName{_mangledName}, type{_type} {
		assert(!isEmpty(mangledName));
	}

	inline const ConcreteParam withType(const ConcreteType newType) const {
		return ConcreteParam{mangledName, newType};
	}
};

struct ConcreteSig {
	// This should be globally unique.
	const Str mangledName;
	const ConcreteType returnType;
	// NOTE: when a parameter has been specialized to a constant, it should be omitted here.
	// When a parameter has been specialized with a KnownLambdaBody,
	// this should store the closure (or be omitted if none).
	const Arr<const ConcreteParam> params;

	inline ConcreteSig(const Str _mangledName, const ConcreteType _returnType, const Arr<const ConcreteParam> _params)
		: mangledName{_mangledName}, returnType{_returnType}, params{_params} {
		assert(isMangledName(mangledName));
	}

	inline size_t arity() const {
		return size(params);
	}
};

struct Constant;

struct ConstantArrayBackingKey {
	const ConcreteType elementType;
	const Arr<const Constant*> elements;
};

inline Comparison compareConstantArrayBackingKey(const ConstantArrayBackingKey a, const ConstantArrayBackingKey b) {
	const Comparison res = compareConcreteType(a.elementType, b.elementType);
	return res != Comparison::equal
		? res
		: compareArr<const Constant*, comparePtr<const Constant>>(a.elements, b.elements);
}

// Note: There is no ConstantKind::Array.
// Instead, that's just a Record of tha size and data.
// The data is a ConstantKind::Ptr, which references this backing.
// This ensures we can slice arrays at compile time using normal constant-record code.
// Different ConstantKind::Ptr_s can share the same ConstantArrayBacking.
struct ConstantArrayBacking {
	const Arr<const Constant*> elements;
	const ConcreteType elementType;
	const Nat64 id;

	inline size_t size() const {
		return ::size(elements);
	}
};

struct ConcreteFun;
struct KnownLambdaBody;

struct ConstantKind {
	// Unlike a Lambda this has no closure.
	// We get these by accessing the '.fun' property on a constant Lambda.
	struct FunPtr {
		const ConcreteFun* fun;
	};

	// Note: A fun-ref is never constant since that needs at least a vat id.
	// A fun-ptr is a ConstantKind::FunPtr, not a ConstantKind::Lambda.
	struct Lambda {
		const KnownLambdaBody* knownLambdaBody;

		Lambda(const KnownLambdaBody* klb);
	};

	struct Null {};

	struct Ptr {
		const ConstantArrayBacking* array;
		const size_t index;

		inline Ptr(const ConstantArrayBacking* _array, const size_t _index)
			: array{_array}, index{_index} {
			assert(index < array->size());
		}

		const Constant* deref() const;
	};

	struct Record {
		// TODO: isn't this redundant to constant->type()?
		const ConcreteType type;
		const Arr<const Constant*> args;
	};

	struct Union {
		const ConcreteStruct* unionType;
		const size_t memberIndex;
		const Constant* member;
	};

	struct Void {};

//TODO: private:
	enum class Kind {
		_bool,
		_char,
		funPtr,
		int16,
		int32,
		int64,
		lambda,
		nat16,
		nat32,
		nat64,
		_null,
		ptr,
		record,
		_union,
		_void,
	};
	const Kind kind;
	union {
		const Bool _bool;
		const char _char;
		const FunPtr funPtr;
		const Int16 int16;
		const Int32 int32;
		const Int64 int64;
		const Lambda lambda;
		const Nat16 nat16;
		const Nat32 nat32;
		const Nat64 nat64;
		const Null _null;
		const Ptr ptr;
		const Record record;
		const Union _union;
		const Void _void;
	};

public:
	explicit inline ConstantKind(const Bool a) : kind{Kind::_bool}, _bool{a} {}
	explicit inline ConstantKind(const char a) : kind{Kind::_char}, _char{a} {}
	explicit inline ConstantKind(const FunPtr a) : kind{Kind::funPtr}, funPtr{a} {}
	explicit inline ConstantKind(const Int16 a) : kind{Kind::int16}, int16{a} {}
	explicit inline ConstantKind(const Int32 a) : kind{Kind::int32}, int32{a} {}
	explicit inline ConstantKind(const Int64 a) : kind{Kind::int64}, int64{a} {}
	explicit inline ConstantKind(const Lambda a) : kind{Kind::lambda}, lambda{a} {}
	explicit inline ConstantKind(const Nat16 a) : kind{Kind::nat16}, nat16{a} {}
	explicit inline ConstantKind(const Nat32 a) : kind{Kind::nat32}, nat32{a} {}
	explicit inline ConstantKind(const Nat64 a) : kind{Kind::nat64}, nat64{a} {}
	explicit inline ConstantKind(const Null a) : kind{Kind::_null}, _null{a} {}
	explicit inline ConstantKind(const Ptr a) : kind{Kind::ptr}, ptr{a} {}
	explicit inline ConstantKind(const Record a) : kind{Kind::record}, record{a} {}
	explicit inline ConstantKind(const Union a) : kind{Kind::_union}, _union{a} {}
	explicit inline ConstantKind(const Void a) : kind{Kind::_void}, _void{a} {}

	inline const Bool isBool() const {
		return enumEq(kind, Kind::_bool);
	}
	inline const Bool isChar() const {
		return enumEq(kind, Kind::_char);
	}
	inline const Bool isFunPtr() const {
		return enumEq(kind, Kind::funPtr);
	}
	inline const Bool isInt16() const {
		return enumEq(kind, Kind::int16);
	}
	inline const Bool isInt32() const {
		return enumEq(kind, Kind::int32);
	}
	inline const Bool isInt64() const {
		return enumEq(kind, Kind::int64);
	}
	inline const Bool isLambda() const {
		return enumEq(kind, Kind::lambda);
	}
	inline const Bool isNat16() const {
		return enumEq(kind, Kind::nat16);
	}
	inline const Bool isNat32() const {
		return enumEq(kind, Kind::nat32);
	}
	inline const Bool isNat64() const {
		return enumEq(kind, Kind::nat64);
	}
	inline const Bool isNull() const {
		return enumEq(kind, Kind::_null);
	}
	inline const Bool isPtr() const {
		return enumEq(kind, Kind::ptr);
	}
	inline const Bool isRecord() const {
		return enumEq(kind, Kind::record);
	}
	inline const Bool isUnion() const {
		return enumEq(kind, Kind::_union);
	}
	inline const Bool isVoid() const {
		return enumEq(kind, Kind::_void);
	}

	inline const Bool asBool() const {
		assert(isBool());
		return _bool;
	}
	inline char asChar() const {
		assert(isChar());
		return _char;
	}
	inline FunPtr asFunPtr() const {
		assert(isFunPtr());
		return funPtr;
	}
	inline Int16 asInt16() const {
		assert(isInt16());
		return int16;
	}
	inline Int32 asInt32() const {
		assert(isInt32());
		return int32;
	}
	inline Int64 asInt64() const {
		assert(isInt64());
		return int64;
	}
	inline Lambda asLambda() const {
		assert(isLambda());
		return lambda;
	}
	inline Nat16 asNat16() const {
		assert(isNat16());
		return nat16;
	}
	inline Nat32 asNat32() const {
		assert(isNat32());
		return nat32;
	}
	inline Nat64 asNat64() const {
		assert(isNat64());
		return nat64;
	}
	inline Null asNull() const {
		assert(isNull());
		return _null;
	}
	inline Ptr asPtr() const {
		assert(isPtr());
		return ptr;
	}
	inline Record asRecord() const {
		assert(isRecord());
		return record;
	}
	inline Union asUnion() const {
		assert(isUnion());
		return _union;
	}
	inline Void asVoid() const {
		assert(isVoid());
		return _void;
	}

	template <
		typename CbBool,
		typename CbChar,
		typename CbFunPtr,
		typename CbInt16,
		typename CbInt32,
		typename CbInt64,
		typename CbLambda,
		typename CbNat16,
		typename CbNat32,
		typename CbNat64,
		typename CbNull,
		typename CbPtr,
		typename CbRecord,
		typename CbUnion,
		typename CbVoid
	>
	inline auto match(
		CbBool cbBool,
		CbChar cbChar,
		CbFunPtr cbFunPtr,
		CbInt16 cbInt16,
		CbInt32 cbInt32,
		CbInt64 cbInt64,
		CbLambda cbLambda,
		CbNat16 cbNat16,
		CbNat32 cbNat32,
		CbNat64 cbNat64,
		CbNull cbNull,
		CbPtr cbPtr,
		CbRecord cbRecord,
		CbUnion cbUnion,
		CbVoid cbVoid
	) const {
		switch (kind) {
			case Kind::_bool:
				return cbBool(_bool);
			case Kind::_char:
				return cbChar(_char);
			case Kind::funPtr:
				return cbFunPtr(funPtr);
			case Kind::int16:
				return cbInt16(int16);
			case Kind::int32:
				return cbInt32(int32);
			case Kind::int64:
				return cbInt64(int64);
			case Kind::lambda:
				return cbLambda(lambda);
			case Kind::nat16:
				return cbNat16(nat16);
			case Kind::nat32:
				return cbNat32(nat32);
			case Kind::nat64:
				return cbNat64(nat64);
			case Kind::_null:
				return cbNull(_null);
			case Kind::ptr:
				return cbPtr(ptr);
			case Kind::record:
				return cbRecord(record);
			case Kind::_union:
				return cbUnion(_union);
			case Kind::_void:
				return cbVoid(_void);
			default:
				assert(0);
		}
	}
};

struct Constant {
	const ConcreteType _type;
	const ConstantKind kind;
	const Nat64 id;
	inline Constant(const ConcreteType type, const ConstantKind _kind, const Nat64 _id)
		: _type{type}, kind{_kind}, id{_id} {}

	inline const ConcreteType type() const {
		return _type;
	}
};

void debugWriteConstant(Writer* writer, const Constant* c);

// NOTE: a Constant can still have a KnownLambdaBody of course!
struct ConstantOrLambdaOrVariable {
	struct Variable {};

	enum class Kind {
		variable,
		constant,
		knownLambdaBody,
	};
	const Kind kind;
private:
	union {
		const Variable variable;
		const Constant* constant;
		const KnownLambdaBody* knownLambdaBody;
	};

public:
	explicit inline ConstantOrLambdaOrVariable(const Variable _variable) : kind{Kind::variable}, variable{_variable} {}
	explicit inline ConstantOrLambdaOrVariable(const Constant* _constant)
		: kind{Kind::constant}, constant{_constant} {}
	explicit inline ConstantOrLambdaOrVariable(const KnownLambdaBody* _knownLambdaBody)
		: kind{Kind::knownLambdaBody}, knownLambdaBody{_knownLambdaBody} {}

	inline const Bool isVariable() const {
		return enumEq(kind, Kind::variable);
	}
	inline const Bool isConstant() const {
		return enumEq(kind, Kind::constant);
	}
	inline const Bool isKnownLambdaBody() const {
		return enumEq(kind, Kind::knownLambdaBody);
	}

	inline const Constant* asConstant() const {
		assert(isConstant());
		return constant;
	}
	inline const KnownLambdaBody* asKnownLambdaBody() const {
		assert(isKnownLambdaBody());
		return knownLambdaBody;
	}

	template <
		typename CbVariable,
		typename CbConstant,
		typename CbKnownLambdaBody
	>
	inline auto match(
		CbVariable cbVariable,
		CbConstant cbConstant,
		CbKnownLambdaBody cbKnownLambdaBody
	) const {
		switch (kind) {
			case Kind::variable:
				return cbVariable(variable);
			case Kind::constant:
				return cbConstant(constant);
			case Kind::knownLambdaBody:
				return cbKnownLambdaBody(knownLambdaBody);
			default:
				assert(0);
		}
	}
};

void debugWriteConstantOrLambdaOrVariable(Writer* writer, const ConstantOrLambdaOrVariable clv);

inline Comparison compareConstantOrLambdaOrVariable(
	const ConstantOrLambdaOrVariable a,
	const ConstantOrLambdaOrVariable b
) {
	// variable < constant < knownlambdabody
	if (a.kind != b.kind)
		return comparePrimitive(a.kind, b.kind);
	else
		return a.match(
			[](const ConstantOrLambdaOrVariable::Variable) {
				return Comparison::equal;
			},
			[&](const Constant* ca) {
				return comparePtr(ca, b.asConstant());
			},
			[&](const KnownLambdaBody* klba) {
				return comparePtr(klba, b.asKnownLambdaBody());
			});
}

inline Comparison compareConstantOrLambdaOrVariableArr(
	const Arr<const ConstantOrLambdaOrVariable> a,
	const Arr<const ConstantOrLambdaOrVariable> b
) {
	return compareArr<const ConstantOrLambdaOrVariable, compareConstantOrLambdaOrVariable>(a, b);
}

struct ConcreteExpr;

struct ConstantOrExpr {
private:
	enum class Kind {
		constant,
		concreteExpr,
	};
	const Kind kind;
	union {
		const Constant* constant;
		const ConcreteExpr* concreteExpr;
	};
public:
	explicit inline ConstantOrExpr(const Constant* _constant) : kind{Kind::constant}, constant{_constant} {}
	explicit inline ConstantOrExpr(const ConcreteExpr* _concreteExpr)
		: kind{Kind::concreteExpr}, concreteExpr{_concreteExpr} {}

	inline const Bool isConstant() const {
		return enumEq(kind, Kind::constant);
	}
	inline const Bool isConcreteExpr() const {
		return enumEq(kind, Kind::concreteExpr);
	}
	inline const Constant* asConstant() const {
		assert(isConstant());
		return constant;
	}
	inline const ConcreteExpr* asConcreteExpr() const {
		assert(isConcreteExpr());
		return concreteExpr;
	}

	template <
		typename CbConstant,
		typename CbConcreteExpr
	>
	inline auto match(
		CbConstant cbConstant,
		CbConcreteExpr cbConcreteExpr
	) const {
		switch (kind) {
			case Kind::constant:
				return cbConstant(constant);
			case Kind::concreteExpr:
				return cbConcreteExpr(concreteExpr);
			default:
				assert(0);
		}
	}

	// Note: This ignores KnownLambdaBody!
	const ConcreteType typeWithoutKnownLambdaBody() const;
	// If this has a KnownLambdaBody, this is the closure type
	const Opt<const ConcreteType> typeWithKnownLambdaBody() const;
};

struct ConcreteLocal {
	const Str mangledName;
	const ConcreteType type;
	const ConstantOrLambdaOrVariable constantOrLambdaOrVariable;
};

struct ConcreteFunExprBody {
	const Arr<const ConcreteLocal*> allLocals;
	const ConcreteExpr* expr;
};

struct ConcreteFunBody {
	struct Bogus {};
	struct Builtin {
		const BuiltinFunInfo builtinInfo;
		const Arr<const ConcreteType> typeArgs;
	};
	struct Extern {
		const Bool isGlobal;
	};
private:
	enum class Kind {
		bogus,
		builtin,
		_extern,
		constant,
		concreteFunExprBody,
	};
	const Kind kind;
	union {
		const Bogus bogus;
		const Builtin builtin;
		const Extern _extern;
		const Constant* constant;
		const ConcreteFunExprBody concreteFunExprBody;
	};
public:
	explicit inline ConcreteFunBody(const Bogus _bogus) : kind{Kind::bogus}, bogus{_bogus} {}
	explicit inline ConcreteFunBody(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
	explicit inline ConcreteFunBody(const Extern __extern) : kind{Kind::_extern}, _extern{__extern} {}
	explicit inline ConcreteFunBody(const Constant* _constant) : kind{Kind::constant}, constant{_constant} {}
	explicit inline ConcreteFunBody(const ConcreteFunExprBody _concreteFunExprBody)
		: kind{Kind::concreteFunExprBody}, concreteFunExprBody{_concreteFunExprBody} {}

	inline const Bool isBogus() const {
		return enumEq(kind, Kind::bogus);
	}
	inline const Bool isBuiltin() const {
		return enumEq(kind, Kind::builtin);
	}
	inline const Bool isExtern() const {
		return enumEq(kind, Kind::_extern);
	}
	inline const Bool isConstant() const {
		return enumEq(kind, Kind::constant);
	}
	inline const Bool isConcreteFunExprBody() const {
		return enumEq(kind, Kind::concreteFunExprBody);
	}
	inline const Bogus asBogus() const {
		assert(isBogus());
		return bogus;
	}
	inline const Builtin asBuiltin() const {
		assert(isBuiltin());
		return builtin;
	}
	inline const Extern asExtern() const {
		assert(isExtern());
		return _extern;
	}
	inline const Constant* asConstant() const {
		assert(isConstant());
		return constant;
	}
	inline const ConcreteFunExprBody asConcreteFunExprBody() const {
		assert(isConcreteFunExprBody());
		return concreteFunExprBody;
	}

	template <
		typename CbBogus,
		typename CbBuiltin,
		typename CbExtern,
		typename CbConstant,
		typename CbConcreteFunExprBody
	>
	inline auto match(
		CbBogus cbBogus,
		CbBuiltin cbBuiltin,
		CbExtern cbExtern,
		CbConstant cbConstant,
		CbConcreteFunExprBody cbConcreteFunExprBody
	) const {
		switch (kind) {
			case Kind::bogus:
				return cbBogus(bogus);
			case Kind::builtin:
				return cbBuiltin(builtin);
			case Kind::_extern:
				return cbExtern(_extern);
			case Kind::constant:
				return cbConstant(constant);
			case Kind::concreteFunExprBody:
				return cbConcreteFunExprBody(concreteFunExprBody);
			default:
				assert(0);
		}
	}

	inline const Bool isGlobal() const {
		return _and(isExtern(), asExtern().isGlobal);
	}
};

// We generate a ConcreteFun for:
// Each instantiation of a FunDecl
// Each *instance* of a KnownLambdaBody inside a ConcreteFun
// (since these may be specialized at each callsite)
struct ConcreteFun {
	const Bool needsCtx;
	const Opt<const ConcreteParam> closureParam;
	// Note: This does not include the ctx or closure params.
	const ConcreteSig sig;
	const Bool isCallFun; // `call` is not a builtin, but we treat it specially

	mutable Late<const Constant*> _asLambda {};
	Late<const ConcreteFunBody> _body {};
	size_t nextLambdaIndex = 0;

	inline ConcreteFun(
		const Bool _needsCtx,
		const Opt<const ConcreteParam> _closureParam,
		const ConcreteSig _sig,
		const Bool _isCallFun
	) : needsCtx{_needsCtx}, closureParam{_closureParam}, sig{_sig}, isCallFun{_isCallFun} {}

	inline const ConcreteFunBody body() const {
		return lateGet(&_body);
	}
	inline void setBody(const ConcreteFunBody value) {
		lateSet<const ConcreteFunBody>(&_body, value);
	}

	inline const Str mangledName() const {
		return sig.mangledName;
	}
	inline const ConcreteType returnType() const {
		return sig.returnType;
	}
	inline const Arr<const ConcreteParam> paramsExcludingCtxAndClosure() const {
		return sig.params;
	}

	inline const Bool hasClosure() const {
		return has(closureParam);
	}

	inline const Opt<const ConcreteType> closureType() const {
		return has(closureParam)
			? some<const ConcreteType>(force(closureParam).type)
			: none<const ConcreteType>();
	}

	inline size_t arityExcludingCtxAndClosure() const {
		return sig.arity();
	}

	inline size_t arityExcludingCtxIncludingClosure() const {
		return (has(closureParam) ? 1 : 0) + arityExcludingCtxAndClosure();
	}

	inline size_t arityIncludingCtxAndClosure() const {
		return (needsCtx ? 1 : 0)  + arityExcludingCtxIncludingClosure();
	}

	inline const Bool isExtern() const {
		return body().isExtern();
	}

	inline const Bool isGlobal() const {
		return body().isGlobal();
	}
};

struct ClosureSingleSpecialize {
	const ConstantOrLambdaOrVariable clv;
	const Opt<const ConcreteField*> field;

	inline ClosureSingleSpecialize(const ConstantOrLambdaOrVariable _clv, const Opt<const ConcreteField*> _field)
		: clv{_clv}, field{_field} {
		assert(clv.isConstant() != has(field));
	}
};

// An expression that's supposed to return a Fn may return its closure instead.
// So we attach a KnownLambdaBody to it.
// If we don't need it specialized, we can use this to convert to a dynamic Fn, which removes the KnownLambdaBody.
// If we can specialize on it, we'll use this to instantiate the lambda to a ConcreteFun when we call it.
//
// KnownLambdaBody is also used with Constant.Kind.Lambda.
// In which case we treat it the same -- specialize when calling, if not specializing, wrap.
//
// NOTE: More information needed to instantiate this is stored in `struct LambdaInfo` in concretizeCtx.d.
//
// NOTE: we *do* create one of these for each FunAsLambda as that makes it easier to handle consistently.
struct KnownLambdaBody {
	const ConcreteType dynamicType; // Type this lambda would satisfy if converted to dynamic
	// Note: closure is not part of the sig.
	const ConcreteSig nonSpecializedSig; // When we instantiate this to a ConcreteFun, we may specialize the signature.
	// Used for the closure struct, and for generating mangled names of funs that specialize on this
	const Str mangledName;
	// The 'closure' type will only contain those closure fields which are not constant.
	const Opt<const ConcreteParam> closureParam;
	// For each closure in the Expr, we may make it a constant or KnownLambdaBody.
	const Arr<const ClosureSingleSpecialize> closureSpecialize;

	// We'll generate this (and cache it here) if this needs to be converted to Fun. This never specializes on args.
	// The dynamic fun will have a closure parameter added even if we don't need it,
	// because all lambdas that might be dynamically called need identical signatures.
	mutable Late<const ConcreteFun*> dynamicInstance {};
	// When this is called directly, we will instantiate it -- this may remove parameters that are constants.
	mutable MutDict<
		const Arr<const ConstantOrLambdaOrVariable>,
		const ConcreteFun*,
		compareConstantOrLambdaOrVariableArr
	> directCallInstances {};

	KnownLambdaBody(const KnownLambdaBody&) = delete;

	inline const Bool hasClosure() const {
		return has(closureParam);
	}

	// WARN: closureType is non-pointer by default,
	// but a ConcreteFun for a dynamically-called lambda should take it by pointer.
	inline const Opt<const ConcreteType> closureType() const {
		return has(closureParam)
			? some<const ConcreteType>(force(closureParam).type)
			: none<const ConcreteType>();
	}

	inline const Arr<const ConcreteField> closureFields() {
		return has(closureParam)
			? force(closureParam).type.strukt->body().asRecord().fields
			: emptyArr<const ConcreteField>();
	}
};

struct ConcreteExpr {
	// Only exists temporarily
	struct Bogus {};

	struct Alloc {
		const ConcreteFun* alloc;
		const ConcreteExpr* inner;
	};

	struct Call {
		const ConcreteFun* called;
		// Note: this should filter out any constant args that 'called' specialized on as constants.
		// (But may include non-specialized-on constant args.)
		const Arr<const ConstantOrExpr> args;

		Call(const ConcreteFun* c, const Arr<const ConstantOrExpr> a);

		inline const ConcreteType returnType() const {
			return called->returnType();
		}
	};

	struct Cond {
		const ConcreteExpr* cond;
		const ConstantOrExpr then;
		const ConstantOrExpr elze;

		Cond(const ConcreteExpr* _cond, const ConstantOrExpr _then, const ConstantOrExpr _elze);
	};

	struct CreateArr {
		const ConcreteStruct* arrType;
		const ConcreteType elementType;
		const ConcreteFun* alloc;
		// Needed because we must first allocate the array, then write to each field. That requires a local.
		const ConcreteLocal* local;
		// Note: if all args were constant, this would be a Constant.ConstantArr instead
		const Arr<const ConstantOrExpr> args;

		inline size_t sizeBytes() const {
			return size(args) * sizeOrPointerSizeBytes(elementType);
		}
	};

	// Note: CreateRecord always creates a record by-value. This may be wrapped in Alloc.
	struct CreateRecord {
		const Arr<const ConstantOrExpr> args;
	};

	struct ImplicitConvertToUnion {
		const size_t memberIndex;
		// TODO:PERF support unions as constants; then the arg here will never be a constant
		const ConstantOrExpr arg;
	};

	struct Let {
		const ConcreteLocal* local;
		const ConcreteExpr* value; // If a constant, we just use 'then' in place of the Let
		const ConstantOrExpr then;
	};

	// NOTE: This if for a lambda of *known* closure and KnownLambdaBody.
	// A dynamic lambda will be LambdaToDynamic.
	// This expression is for instantiating the closure of the lambda;
	// the lambda body itself is stored in knownLambdaBody on this ConcreteExpr.
	struct Lambda {
		// These are only the non-specialized-to-constant closures.
		const Arr<const ConstantOrExpr> closureInit;
	};

	// The ConcreteExpr containing this will have *no* KnownLambdaBody;
	struct LambdaToDynamic {
		const ConcreteFun* fun; // instantiation of 'closure's knownLambdaBody (or from ConstantKind::Lambda).
		// A constant closure should always be Null
		const ConstantOrExpr closure;

		LambdaToDynamic(const ConcreteFun* _fun, const ConstantOrExpr _closure);
	};

	struct LocalRef {
		const ConcreteLocal* local;
	};

	struct Match {
		struct Case {
			const Opt<const ConcreteLocal*> local;
			const ConstantOrExpr then;
		};

		const ConcreteLocal* matchedLocal;
		const ConcreteExpr* matchedValue;
		const Arr<const Case> cases;

		inline Match(
			const ConcreteLocal* _matchedLocal,
			const ConcreteExpr* _matchedValue,
			const Arr<const Case> _cases
		) : matchedLocal{_matchedLocal}, matchedValue{_matchedValue}, cases{_cases} {
			assert(sizeEq(matchedUnionMembers(), cases));
		}

		inline const Arr<const ConcreteType> matchedUnionMembers() const {
			return matchedLocal->type.mustBeNonPointer()->body().asUnion().members;
		}
	};

	struct ParamRef {
		const ConcreteParam* param;
	};

	struct RecordFieldAccess {
		const Bool targetIsPointer;
		const ConstantOrExpr target; // TODO: should never be a constant
		const ConcreteField* field;
	};

	struct RecordFieldSet {
		// TODO: maybe just store a ConcreteType on every ConcreteExpr
		const Bool targetIsPointer;
		const ConcreteExpr* target;
		const ConcreteField* field;
		const ConstantOrExpr value;

		inline RecordFieldSet(
			const Bool _targetIsPointer,
			const ConcreteExpr* _target,
			const ConcreteField* _field,
			const ConstantOrExpr _value
		)
			: targetIsPointer{_targetIsPointer}, target{_target}, field{_field}, value{_value} {
			assert(field->isMutable);
		}
	};

	struct Seq {
		// In the rare event this is a constant, it would have been optimized away.
		const ConcreteExpr* first;
		const ConstantOrExpr then;
	};

	struct SpecialUnary {
		enum class Kind {
			deref,
		};
		const Kind kind;
		const ConcreteExpr* arg;
	};

	struct SpecialBinary {
		enum class Kind {
			add,
			eq,
			less,
			_or,
			sub,
		};

		const Kind kind;
		const ConstantOrExpr left;
		const ConstantOrExpr right;
	};

private:
	enum class Kind {
		bogus,
		alloc,
		call,
		cond,
		createArr,
		createRecord,
		implicitConvertToUnion,
		lambda,
		lambdaToDynamic,
		let,
		localRef,
		match,
		paramRef,
		recordFieldAccess,
		recordFieldSet,
		seq,
		specialUnary,
		specialBinary,
	};
	// This is the type it's *supposed* to have. KnownLambdaBody may better reflect the actual type.
	const ConcreteType _type;
	const SourceRange _range;
	// If the expression has a KnownLambdaBody, we do not pass it as a Fun.
	// We pass it as its closure. (If that were empty this would be a Constant and not a ConcreteExpr.)
	const Opt<const KnownLambdaBody*> _knownLambdaBody;
	const Kind kind;
	union {
		const Bogus bogus;
		const Alloc alloc;
		const Call call;
		const Cond cond;
		const CreateArr createArr;
		const CreateRecord createRecord;
		const ImplicitConvertToUnion implicitConvertToUnion;
		const Lambda lambda;
		const LambdaToDynamic lambdaToDynamic;
		const Let let;
		const LocalRef localRef;
		const Match _match;
		const ParamRef paramRef;
		const RecordFieldAccess recordFieldAccess;
		const RecordFieldSet recordFieldSet;
		const Seq seq;
		const SpecialUnary specialUnary;
		const SpecialBinary specialBinary;
	};

public:
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const Bogus a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::bogus}, bogus{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const Alloc a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::alloc}, alloc{a} {
		assert(type.isPointer);
		// We never alloc a lambda, unless we're making the lambda dynamic (in which case klb is thrown away)
		assert(!has(klb));
		assert(!force(a.inner->typeWithKnownLambdaBody()).isPointer);
	}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const Call a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::call}, call{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const Cond a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::cond}, cond{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const CreateArr a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::createArr}, createArr{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const CreateRecord a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::createRecord}, createRecord{a} {
		assert(!type.isPointer);
	}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const ImplicitConvertToUnion a
	) :
		_type{type},
		_range{range},
		_knownLambdaBody{klb},
		kind{Kind::implicitConvertToUnion},
		implicitConvertToUnion{a}
	{
		assert(a.memberIndex < size(type.strukt->body().asUnion().members));
	}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const Lambda a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::lambda}, lambda{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const LambdaToDynamic a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::lambdaToDynamic}, lambdaToDynamic{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const Let a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::let}, let{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const LocalRef a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::localRef}, localRef{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const Match a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::match}, _match{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const ParamRef a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::paramRef}, paramRef{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const RecordFieldAccess a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::recordFieldAccess}, recordFieldAccess{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const RecordFieldSet a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::recordFieldSet}, recordFieldSet{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const Seq a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::seq}, seq{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const SpecialUnary a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::specialUnary}, specialUnary{a} {}
	inline ConcreteExpr(
		const ConcreteType type,
		const SourceRange range,
		const Opt<const KnownLambdaBody*> klb,
		const SpecialBinary a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::specialBinary}, specialBinary{a} {}

	inline const Bool isCond() const {
		return enumEq(kind, Kind::cond);
	}

	inline const ConcreteType typeWithoutKnownLambdaBody() const {
		return _type;
	}

	inline const Opt<const ConcreteType> typeWithKnownLambdaBody() const {
		if (has(_knownLambdaBody))
			return force(_knownLambdaBody)->closureType();
		else
			return some<const ConcreteType>(typeWithoutKnownLambdaBody());
	}

	inline const SourceRange range() const {
		return _range;
	}

	inline const Opt<const KnownLambdaBody*> knownLambdaBody() const {
		return _knownLambdaBody;
	}

	template <
		typename CbBogus,
		typename CbAlloc,
		typename CbCall,
		typename CbCond,
		typename CbCreateArr,
		typename CbCreateRecord,
		typename CbImplicitConvertToUnion,
		typename CbLambda,
		typename CbLambdaToDynamic,
		typename CbLet,
		typename CbLocalRef,
		typename CbMatch,
		typename CbParamRef,
		typename CbRecordFieldAccess,
		typename CbRecordFieldSet,
		typename CbSeq,
		typename CbSpecialUnary,
		typename CbSpecialBinary
	>
	inline auto match(
		CbBogus cbBogus,
		CbAlloc cbAlloc,
		CbCall cbCall,
		CbCond cbCond,
		CbCreateArr cbCreateArr,
		CbCreateRecord cbCreateRecord,
		CbImplicitConvertToUnion cbImplicitConvertToUnion,
		CbLambda cbLambda,
		CbLambdaToDynamic cbLambdaToDynamic,
		CbLet cbLet,
		CbLocalRef cbLocalRef,
		CbMatch cbMatch,
		CbParamRef cbParamRef,
		CbRecordFieldAccess cbRecordFieldAccess,
		CbRecordFieldSet cbRecordFieldSet,
		CbSeq cbSeq,
		CbSpecialUnary cbSpecialUnary,
		CbSpecialBinary cbSpecialBinary
	) const {
		switch (kind) {
			case Kind::bogus:
				return cbBogus(bogus);
			case Kind::alloc:
				return cbAlloc(alloc);
			case Kind::call:
				return cbCall(call);
			case Kind::cond:
				return cbCond(cond);
			case Kind::createArr:
				return cbCreateArr(createArr);
			case Kind::createRecord:
				return cbCreateRecord(createRecord);
			case Kind::implicitConvertToUnion:
				return cbImplicitConvertToUnion(implicitConvertToUnion);
			case Kind::lambda:
				return cbLambda(lambda);
			case Kind::lambdaToDynamic:
				return cbLambdaToDynamic(lambdaToDynamic);
			case Kind::let:
				return cbLet(let);
			case Kind::localRef:
				return cbLocalRef(localRef);
			case Kind::match:
				return cbMatch(_match);
			case Kind::paramRef:
				return cbParamRef(paramRef);
			case Kind::recordFieldAccess:
				return cbRecordFieldAccess(recordFieldAccess);
			case Kind::recordFieldSet:
				return cbRecordFieldSet(recordFieldSet);
			case Kind::seq:
				return cbSeq(seq);
			case Kind::specialUnary:
				return cbSpecialUnary(specialUnary);
			case Kind::specialBinary:
				return cbSpecialBinary(specialBinary);
			default:
				assert(0);
		}
	}
};

// Note: everything in the returned ConcreteProgram should have isReferenced set to true.
struct ConcreteProgram {
	const Arr<const ConcreteStruct*> allStructs;
	const Arr<const Constant*> allConstants;
	const Arr<const ConcreteFun*> allFuns;
	const Arr<const ConstantArrayBacking*> allArrayBackings;
	const ConcreteFun* rtMain;
	const ConcreteFun* userMain;
	const ConcreteStruct* ctxType;
};
