#pragma once

#include "./util/late.h"
#include "./util/mutDict.h"
#include "./util/sourceRange.h"
#include "./util/writer.h"

enum class BuiltinStructKind {
	_bool,
	byte,
	_char,
	float64,
	funPtrN, // fun-ptr0, fun-ptr1, etc...
	int64,
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
	asNonConst,
	callFunPtr,
	compare, // the `<=>` operator
	deref,
	_false,
	getCtx,
	hardFail,
	_if,
	isReferenceType,
	mulFloat64,
	_not,
	oneInt64,
	oneNat64,
	_or,
	pass,
	ptrCast,
	refOfVal,
	setPtr,
	sizeOf,
	subFloat64,
	_true,
	unsafeDivFloat64,
	unsafeDivInt64,
	unsafeDivNat64,
	unsafeModNat64,
	unsafeNat64ToInt64,
	unsafeInt64ToNat64,
	wrappingAddInt64,
	wrappingAddNat64,
	wrappingMulInt64,
	wrappingMulNat64,
	wrappingSubInt64,
	wrappingSubNat64,
	zeroInt64,
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
	struct Builtin {
		const BuiltinStructInfo info;
		const Arr<const ConcreteType> typeArgs;
	};

	struct Fields {
		const Arr<const ConcreteField> fields;
	};

	struct Union {
		const Arr<const ConcreteType> members;
	};

	struct Iface {
		const Arr<const ConcreteSig> messages;
	};

private:
	enum class Kind {
		builtin,
		fields,
		_union,
		iface,
	};
	const Kind kind;
	union {
		const Builtin builtin;
		const Fields fields;
		const Union _union;
		const Iface iface;
	};

public:
	explicit inline ConcreteStructBody(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
	explicit inline ConcreteStructBody(const Fields _fields) : kind{Kind::fields}, fields{_fields} {}
	explicit inline ConcreteStructBody(const Union __union) : kind{Kind::_union}, _union{__union} {}
	explicit inline ConcreteStructBody(const Iface _iface) : kind{Kind::iface}, iface{_iface} {}

	inline const Bool isBuiltin() const {
		return enumEq(kind, Kind::builtin);
	}
	inline const Bool isFields() const {
		return enumEq(kind, Kind::fields);
	}
	inline const Bool isUnion() const {
		return enumEq(kind, Kind::_union);
	}
	inline const Bool isIface() const {
		return enumEq(kind, Kind::iface);
	}
	inline const Builtin asBuiltin() const {
		assert(isBuiltin());
		return builtin;
	}
	inline const Fields asFields() const {
		assert(isFields());
		return fields;
	}
	inline const Union asUnion() const {
		assert(isUnion());
		return _union;
	}
	inline const Iface asIface() const {
		assert(isIface());
		return iface;
	}

	template <
		typename CbBuiltin,
		typename CbFields,
		typename CbUnion,
		typename CbIface
	>
	inline auto match(
		CbBuiltin cbBuiltin,
		CbFields cbFields,
		CbUnion cbUnion,
		CbIface cbIface
	) const {
		switch (kind) {
			case Kind::builtin:
				return cbBuiltin(builtin);
			case Kind::fields:
				return cbFields(fields);
			case Kind::_union:
				return cbUnion(_union);
			case Kind::iface:
				return cbIface(iface);
			default:
				assert(0);
		}
	}
};

enum class SpecialStructKind {
	arr,
	mutArr,
};

struct ConcreteStruct {
	const Str mangledName;
	const Opt<const SpecialStructKind> special;
	Late<const ConcreteStructBody> _body;
	Late<const size_t> _sizeBytes;

	ConcreteStruct(const ConcreteStruct&) = delete;

	inline ConcreteStruct(const Str mn, const Opt<const SpecialStructKind> sp) : mangledName{mn}, special{sp} {}
	inline ConcreteStruct(const Str mn, const Opt<const SpecialStructKind> sp, const ConcreteStructBody body)
		: mangledName{mn}, special{sp}, _body{body} {}

	inline const ConcreteStructBody body() const {
		return _body.get();
	}
	inline void setBody(const ConcreteStructBody body) {
		_body.set(body);
	}

	inline size_t sizeBytes() const {
		if (!_sizeBytes.isSet()) {
			Arena arena {};
			todo<void>("sizeBytes failed");
		}
		return _sizeBytes.get();
	}

	const Bool isSelfMutable() const;

	inline const Bool defaultIsPointer() const {
		return _or(isSelfMutable(), gt(sizeBytes(), sizeof(void*) * 2));
	}

	inline const Bool isRecord() const {
		return body().isFields();
	}

	inline const Bool isUnion() const {
		return body().isUnion();
	}
};

struct ConcreteType {
	// NOTE: ConcreteType for 'ptr' (e.g. 'ptr byte') will *not* have isPointer set -- since it's not a ptr*
	const Bool isPointer;
	const ConcreteStruct* strukt;

	inline ConcreteType(const Bool _isPointer, const ConcreteStruct* _strukt) : isPointer{_isPointer}, strukt{_strukt} {
		const ConcreteStructBody body = strukt->body();
		if (!body.isFields() && !(body.isBuiltin() && body.asBuiltin().info.kind == BuiltinStructKind::_void)) {
			// union/iface are never pointers
			assert(!isPointer);
		}
	}

	static inline ConcreteType pointer(const ConcreteStruct* strukt) {
		return ConcreteType{True, strukt};
	}

	static inline ConcreteType value(const ConcreteStruct* strukt) {
		return ConcreteType{False, strukt};
	}

	static inline const ConcreteType fromStruct(const ConcreteStruct* s) {
		return ConcreteType{s->defaultIsPointer(), s};
	}

	inline const Bool eq(const ConcreteType other) const {
		return _and(isPointer == other.isPointer, ptrEquals(strukt, other.strukt));
	}

	inline size_t sizeOrPointerSize() const {
		return isPointer ? sizeof(void*) : strukt->sizeBytes();
	}

	inline const ConcreteStruct* mustBePointer() const {
		assert(isPointer);
		return strukt;
	}

	// Union and iface should never be pointers
	inline const ConcreteStruct* mustBeNonPointer() const {
		assert(!isPointer);
		return strukt;
	}

	inline const ConcreteType changeToByRef() const {
		assert(!isPointer);
		return byRef();
	}

	inline const ConcreteType byRef() const {
		return ConcreteType::pointer(strukt);
	}

	inline const ConcreteType byVal() const {
		return ConcreteType::value(strukt);
	}
};

inline void writeConcreteType(Writer& writer, const ConcreteType t) {
	writeStr(writer, t.strukt->mangledName);
	if (t.isPointer)
		writeChar(writer, '*');
}

inline Comparison compareConcreteType(const ConcreteType a, const ConcreteType b) {
	const Comparison res = comparePointer(a.strukt, b.strukt);
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

	inline const ConcreteParam withType(const ConcreteType newType) const {
		return ConcreteParam{mangledName, newType};
	}
};

struct ConcreteSig {
	// For a ConcreteFun this should be globally unique. For an iface it just needs to be unique among its messages.
	const Str mangledName;
	const ConcreteType returnType;
	// NOTE: when a parameter has been specialized to a constant, it should be omitted here.
	// When a parameter has been specialized with a KnownLambdaBody, this should store the closure (or be omitted if none).
	// (Above does not apply to iface messages which are never specialized)
	const Arr<const ConcreteParam> params;

	inline size_t arity() const {
		return params.size;
	}
};

struct Constant;

struct ConstantArrKey {
	const ConcreteType elementType;
	const Arr<const Constant*> elements;
};

inline Comparison compareConstantArrKey(const ConstantArrKey a, const ConstantArrKey b) {
	const Comparison res = compareConcreteType(a.elementType, b.elementType);
	return res != Comparison::equal ? res : compareArr<const Constant*, comparePointer<const Constant>>(a.elements, b.elements);
}

struct ConcreteFun;
struct KnownLambdaBody;

struct ConstantKind {
	struct Array {
		const ConstantArrKey key;

		inline const ConcreteType elementType() const {
			return key.elementType;
		}
		inline size_t size() const {
			return elements().size;
		}
		inline const Arr<const Constant*> elements() const {
			return key.elements;
		}
	};

	// Unlike a Lambda this has no closure.
	// We get these by accessing the '.fun' property on a constant Lambda.
	struct FunPtr {
		const ConcreteFun* fun;
	};

	// Note: A remote-fun is never constant since that needs at least a vat id.
	struct Lambda {
		const KnownLambdaBody* knownLambdaBody;

		Lambda(const KnownLambdaBody* klb);
	};

	struct Null {};

	struct Ptr {
		const Constant* array;
		const size_t index;

		const Constant* deref() const;
	};

	struct Record {
		const ConcreteType type;
		const Arr<const Constant*> args;
	};

	struct Union {
		const ConcreteStruct* unionType;
		const size_t memberIndex;
		const Constant* member;
	};

	struct Void {};

private:
	enum class Kind {
		array,
		_bool,
		_char,
		funPtr,
		int64,
		lambda,
		nat64,
		_null,
		ptr,
		record,
		_union,
		_void,
	};
	const Kind kind;
	union {
		const Array array;
		const Bool _bool;
		const char _char;
		const FunPtr funPtr;
		const Int64 int64;
		const Lambda lambda;
		const Nat64 nat64;
		const Null _null;
		const Ptr ptr;
		const Record record;
		const Union _union;
		const Void _void;
	};

public:
	explicit inline ConstantKind(const Array a) : kind{Kind::array}, array{a} {}
	explicit inline ConstantKind(const Bool a) : kind{Kind::_bool}, _bool{a} {}
	explicit inline ConstantKind(const char a) : kind{Kind::_char}, _char{a} {}
	explicit inline ConstantKind(const FunPtr a) : kind{Kind::funPtr}, funPtr{a} {}
	explicit inline ConstantKind(const Int64 a) : kind{Kind::int64}, int64{a} {}
	explicit inline ConstantKind(const Lambda a) : kind{Kind::lambda}, lambda{a} {}
	explicit inline ConstantKind(const Nat64 a) : kind{Kind::nat64}, nat64{a} {}
	explicit inline ConstantKind(const Null a) : kind{Kind::_null}, _null{a} {}
	explicit inline ConstantKind(const Ptr a) : kind{Kind::ptr}, ptr{a} {}
	explicit inline ConstantKind(const Record a) : kind{Kind::record}, record{a} {}
	explicit inline ConstantKind(const Union a) : kind{Kind::_union}, _union{a} {}
	explicit inline ConstantKind(const Void a) : kind{Kind::_void}, _void{a} {}

	inline const Bool isArray() const {
		return enumEq(kind, Kind::array);
	}
	inline const Bool isBool() const {
		return enumEq(kind, Kind::_bool);
	}
	inline const Bool isChar() const {
		return enumEq(kind, Kind::_char);
	}
	inline const Bool isFunPtr() const {
		return enumEq(kind, Kind::funPtr);
	}
	inline const Bool isInt64() const {
		return enumEq(kind, Kind::int64);
	}
	inline const Bool isLambda() const {
		return enumEq(kind, Kind::lambda);
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

	inline Array asArray() const {
		assert(isArray());
		return array;
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
	inline Int64 asInt64() const {
		assert(isInt64());
		return int64;
	}
	inline Lambda asLambda() const {
		assert(isLambda());
		return lambda;
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
		typename CbArray,
		typename CbBool,
		typename CbChar,
		typename CbFunPtr,
		typename CbInt64,
		typename CbLambda,
		typename CbNat64,
		typename CbNull,
		typename CbPtr,
		typename CbRecord,
		typename CbUnion,
		typename CbVoid
	>
	inline auto match(
		CbArray cbArray,
		CbBool cbBool,
		CbChar cbChar,
		CbFunPtr cbFunPtr,
		CbInt64 cbInt64,
		CbLambda cbLambda,
		CbNat64 cbNat64,
		CbNull cbNull,
		CbPtr cbPtr,
		CbRecord cbRecord,
		CbUnion cbUnion,
		CbVoid cbVoid
	) const {
		switch (kind) {
			case Kind::array:
				return cbArray(array);
			case Kind::_bool:
				return cbBool(_bool);
			case Kind::_char:
				return cbChar(_char);
			case Kind::funPtr:
				return cbFunPtr(funPtr);
			case Kind::int64:
				return cbInt64(int64);
			case Kind::lambda:
				return cbLambda(lambda);
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
	const size_t id;
	inline Constant(const ConcreteType type, const ConstantKind _kind, const size_t _id)
		: _type{type}, kind{_kind}, id{_id} {}

	inline const ConcreteType type() const {
		return _type;
	}
};

void writeConstant(Writer& writer, const Constant* c);

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

void writeConstantOrLambdaOrVariable(Writer& writer, const ConstantOrLambdaOrVariable clv);

inline Comparison compareConstantOrLambdaOrVariable(const ConstantOrLambdaOrVariable a, const ConstantOrLambdaOrVariable b) {
	// variable < constant < knownlambdabody
	if (a.kind != b.kind)
		return comparePrimitive(a.kind, b.kind);
	else
		return a.match(
			[](const ConstantOrLambdaOrVariable::Variable) {
				return Comparison::equal;
			},
			[&](const Constant* ca) {
				return comparePointer(ca, b.asConstant());
			},
			[&](const KnownLambdaBody* klba) {
				return comparePointer(klba, b.asKnownLambdaBody());
			});
}

inline Comparison compareConstantOrLambdaOrVariableArr(const Arr<const ConstantOrLambdaOrVariable> a, const Arr<const ConstantOrLambdaOrVariable> b) {
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
	explicit inline ConstantOrExpr(const ConcreteExpr* _concreteExpr) : kind{Kind::concreteExpr}, concreteExpr{_concreteExpr} {}

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

struct ConcreteFunBody {
	struct Builtin {
		const BuiltinFunInfo builtinInfo;
		const Arr<const ConcreteType> typeArgs;
	};
	struct Extern {};
private:
	enum class Kind {
		builtin,
		_extern,
		constant,
		concreteExpr,
	};
	const Kind kind;
	union {
		const Builtin builtin;
		const Extern _extern;
		const Constant* constant;
		const ConcreteExpr* concreteExpr;
	};
public:
	explicit inline ConcreteFunBody(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
	explicit inline ConcreteFunBody(const Extern __extern) : kind{Kind::_extern}, _extern{__extern} {}
	explicit inline ConcreteFunBody(const Constant* _constant) : kind{Kind::constant}, constant{_constant} {}
	explicit inline ConcreteFunBody(const ConcreteExpr* _concreteExpr) : kind{Kind::concreteExpr}, concreteExpr{_concreteExpr} {}

	inline const Bool isBuiltin() const {
		return enumEq(kind, Kind::builtin);
	}
	inline const Bool isExtern() const {
		return enumEq(kind, Kind::_extern);
	}
	inline const Bool isConstant() const {
		return enumEq(kind, Kind::constant);
	}
	inline const Bool isConcreteExpr() const {
		return enumEq(kind, Kind::concreteExpr);
	}
	inline Builtin asBuiltin() const {
		assert(isBuiltin());
		return builtin;
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
		typename CbBuiltin,
		typename CbExtern,
		typename CbConstant,
		typename CbConcreteExpr
	>
	inline auto match(
		CbBuiltin cbBuiltin,
		CbExtern cbExtern,
		CbConstant cbConstant,
		CbConcreteExpr cbConcreteExpr
	) const {
		switch (kind) {
			case Kind::builtin:
				return cbBuiltin(builtin);
			case Kind::_extern:
				return cbExtern(_extern);
			case Kind::constant:
				return cbConstant(constant);
			case Kind::concreteExpr:
				return cbConcreteExpr(concreteExpr);
		}
	}
};

// We generate a ConcreteFun for:
// Each instantiation of a FunDecl
// Each *instance* of a KnownLambdaBody inside a ConcreteFun
//   (since these may be specialized at each callsite)
// TODO: and for iface impls
struct ConcreteFun {
	const Bool needsCtx;
	const Opt<const ConcreteParam> closureParam;
	// Note: This does not include the ctx or closure params.
	const ConcreteSig sig;
	const Bool isCallFun; // `call` is not a builtin, but we treat it specially

	Late<const ConcreteFunBody> _body = {};
	size_t nextNewIfaceImplIndex = 0;
	size_t nextLambdaIndex = 0;

	inline ConcreteFun(const Bool _needsCtx, const Opt<const ConcreteParam> _closureParam, const ConcreteSig _sig, const Bool _isCallFun)
		: needsCtx{_needsCtx}, closureParam{_closureParam}, sig{_sig}, isCallFun{_isCallFun} {}

	inline const ConcreteFunBody body() const {
		return _body.get();
	}
	inline void setBody(const ConcreteFunBody value) {
		_body.set(value);
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
		return closureParam.has();
	}

	inline const Opt<const ConcreteType> closureType() const {
		return closureParam.has() ? closureParam.force().type : none<const ConcreteType>();
	}

	inline size_t arityExcludingCtxAndClosure() const {
		return sig.arity();
	}

	inline size_t arityExcludingCtxIncludingClosure() const {
		return (closureParam.has() ? 1 : 0) + arityExcludingCtxAndClosure();
	}

	inline size_t arityIncludingCtxAndClosure() const {
		return (needsCtx ? 1 : 0)  + arityExcludingCtxIncludingClosure();
	}
};

struct ClosureSingleSpecialize {
	const ConstantOrLambdaOrVariable clv;
	const Opt<const ConcreteField*> field;

	inline ClosureSingleSpecialize(const ConstantOrLambdaOrVariable _clv, const Opt<const ConcreteField*> _field)
		: clv{_clv}, field{_field} {
		assert(clv.isConstant() != field.has());
	}
};

// An expression that's supposed to return a Fn may return its closure instead.
// So we attach a KnownLambdaBody to it.
// If we don't need it specialized, we can use this to convert to a dynamic Fn, which removes the KnownLambdaBody.
// If we can specialize on it, we'll use this to instantiate the lambda to a ConcreteFun when we call it.
//
// KnownLambdaBody is also used with Constant.Kind.Lambda. In which case we treat it the same -- specialize when calling, if not specializing, wrap.
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
		return closureParam.has();
	}

	// WARN: closureType is non-pointer by default, but a ConcreteFun for a dynamically-called lambda should take it by pointer.
	inline const Opt<const ConcreteType> closureType() const {
		return closureParam.has()
			? some<const ConcreteType>(closureParam.force().type)
			: none<const ConcreteType>();
	}

	inline const Arr<const ConcreteField> closureFields() {
		return closureParam.has()
			? closureParam.force().type.strukt->body().asFields().fields
			: emptyArr<const ConcreteField>();
	}
};

struct ConcreteLocal {
	const Str mangledName;
	const ConcreteType type;
	const ConstantOrLambdaOrVariable constantOrLambdaOrVariable;
};

struct ConcreteExpr {
	// Only exists temporarily
	struct Bogus {};

	struct Alloc {
		const ConcreteFun* alloc;
		const ConcreteExpr* inner;
	};

	struct CallConcreteFun {
		const ConcreteFun* called;
		// Note: this should filter out any constant args that 'called' specialized on as constants.
		// (But may include non-specialized-on constant args.)
		const Arr<const ConstantOrExpr> args;

		CallConcreteFun(const ConcreteFun* c, const Arr<const ConstantOrExpr> a);
	};

	struct Cond {
		const ConcreteExpr* cond;
		const ConstantOrExpr then;
		const ConstantOrExpr elze;
	};

	struct CreateArr {
		const ConcreteStruct* arrType;
		const ConcreteType elementType;
		const ConcreteFun* alloc;
		// Note: if all args were constant, this would be a Constant.ConstantArr instead
		const Arr<const ConstantOrExpr> args;
	};

	// Note: CreateRecord always creates a record by-value. This may be wrapped in Alloc.
	struct CreateRecord {
		const Arr<const ConstantOrExpr> args;
	};

	struct ImplicitConvertToUnion {
		// unionType comes from the ConcreteExpr
		const ConcreteType memberType;
		// TODO:PERF support unions as constants; then the arg here will never be a constant
		const ConstantOrExpr arg;
	};

	struct Let {
		const ConcreteLocal* local;
		const ConcreteExpr* value; // If a constant, we just use 'then' in place of the Let
		const ConstantOrExpr then;
	};

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

		const ConcreteExpr* matched;
		const ConcreteStruct* matchedUnion;
		const Arr<const Case> cases;

		inline Match(const ConcreteExpr* _matched, const ConcreteStruct* _matchedUnion, const Arr<const Case> _cases)
			: matched{_matched}, matchedUnion{_matchedUnion}, cases{_cases} {
			assert(matchedUnionMembers().size == cases.size);
		}

		inline const Arr<const ConcreteType> matchedUnionMembers() const {
			return matchedUnion->body().asUnion().members;
		}
	};

	struct MessageSend {
		const ConstantOrExpr target;
		const ConcreteSig* message;
		const Arr<const ConstantOrExpr> args;
	};

	struct NewIfaceImpl {
		//TODO: use ConcreteFun for this
		struct MessageImpl {
			const Str mangledName;
			const ConstantOrExpr body;
		};

		const ConcreteStruct* iface;
		const Opt<const ConcreteType> fieldsStruct;
		// TODO:PERF if it's a constant, don't make it a field
		const Arr<const ConstantOrExpr> fieldInitializers;
		const Arr<const MessageImpl> messageImpls;

		inline NewIfaceImpl(
			const ConcreteStruct* _iface,
			const Opt<const ConcreteType> _fieldsStruct,
			const Arr<const ConstantOrExpr> _fieldInitializers,
			const Arr<const MessageImpl> _messageImpls
		) : iface{_iface}, fieldsStruct{_fieldsStruct}, fieldInitializers{_fieldInitializers}, messageImpls{_messageImpls} {
			if (fieldsStruct.has()) {
				const ConcreteStruct* strukt = fieldsStruct.force().strukt;
				assert(strukt->body().asFields().fields.size == fieldInitializers.size);
			} else
				assert(isEmpty(fieldInitializers));

			assert(iface->body().asIface().messages.size == messageImpls.size);
		}

		inline const Arr<const ConcreteSig> messagesSigs() const {
			return iface->body().asIface().messages;
		}
	};

	struct ParamRef {
		const ConcreteParam* param;
	};

	struct Seq {
		// In the rare event this is a constant, it would have been optimized away.
		const ConcreteExpr* first;
		const ConstantOrExpr then;
	};

	struct SpecialBinary {
		enum class Kind {
			less,
			_or,
		};

		const Kind kind;
		const ConstantOrExpr left;
		const ConstantOrExpr right;
	};

	struct StructFieldAccess {
		const Bool targetIsPointer;
		const ConstantOrExpr target; // TODO: should never be a constant
		const ConcreteField* field;
	};

	struct StructFieldSet {
		// TODO: maybe just store a ConcreteType on every ConcreteExpr
		const Bool targetIsPointer;
		const ConcreteExpr* target;
		const ConcreteField* field;
		const ConstantOrExpr value;

		inline StructFieldSet(const Bool _targetIsPointer, const ConcreteExpr* _target, const ConcreteField* _field, const ConstantOrExpr _value)
			: targetIsPointer{_targetIsPointer}, target{_target}, field{_field}, value{_value} {
			assert(field->isMutable);
		}
	};

private:
	enum class Kind {
		bogus,
		alloc,
		callConcreteFun,
		cond,
		createArr,
		createRecord,
		implicitConvertToUnion,
		lambda,
		lambdaToDynamic,
		let,
		localRef,
		match,
		messageSend,
		newIfaceImpl,
		paramRef,
		seq,
		specialBinary,
		structFieldAccess,
		structFieldSet,
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
		const CallConcreteFun callConcreteFun;
		const Cond cond;
		const CreateArr createArr;
		const CreateRecord createRecord;
		const ImplicitConvertToUnion implicitConvertToUnion;
		const Lambda lambda;
		const LambdaToDynamic lambdaToDynamic;
		const Let let;
		const LocalRef localRef;
		const Match _match;
		const MessageSend messageSend;
		const NewIfaceImpl newIfaceImpl;
		const ParamRef paramRef;
		const Seq seq;
		const SpecialBinary specialBinary;
		const StructFieldAccess structFieldAccess;
		const StructFieldSet structFieldSet;
	};

public:
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Bogus a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::bogus}, bogus{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Alloc a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::alloc}, alloc{a} {
		assert(type.isPointer);
		// We never alloc a lambda, unless we're making the lambda dynamic (in which case klb is thrown away)
		assert(!klb.has());
		assert(!a.inner->typeWithKnownLambdaBody().force().isPointer);
	}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const CallConcreteFun a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::callConcreteFun}, callConcreteFun{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Cond a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::cond}, cond{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const CreateArr a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::createArr}, createArr{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const CreateRecord a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::createRecord}, createRecord{a} {
		assert(!type.isPointer);
	}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const ImplicitConvertToUnion a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::implicitConvertToUnion}, implicitConvertToUnion{a} {

		assert(exists(type.strukt->body().asUnion().members, [&](const ConcreteType ct) {
			return ct.eq(a.memberType);
		}));
	}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Lambda a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::lambda}, lambda{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const LambdaToDynamic a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::lambdaToDynamic}, lambdaToDynamic{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Let a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::let}, let{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const LocalRef a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::localRef}, localRef{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Match a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::match}, _match{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const MessageSend a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::messageSend}, messageSend{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const NewIfaceImpl a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::newIfaceImpl}, newIfaceImpl{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const ParamRef a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::paramRef}, paramRef{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Seq a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::seq}, seq{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const SpecialBinary a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::specialBinary}, specialBinary{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const StructFieldAccess a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::structFieldAccess}, structFieldAccess{a} {}
	inline ConcreteExpr(const ConcreteType type, const SourceRange range, const Opt<const KnownLambdaBody*> klb, const StructFieldSet a)
		: _type{type}, _range{range}, _knownLambdaBody{klb}, kind{Kind::structFieldSet}, structFieldSet{a} {}

	inline const Bool isCond() const {
		return enumEq(kind, Kind::cond);
	}

	inline const ConcreteType typeWithoutKnownLambdaBody() const {
		return _type;
	}

	inline const Opt<const ConcreteType> typeWithKnownLambdaBody() const {
		if (_knownLambdaBody.has())
			return _knownLambdaBody.force()->closureType();
		else
			return typeWithoutKnownLambdaBody();
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
		typename CbCallConcreteFun,
		typename CbCond,
		typename CbCreateArr,
		typename CbCreateRecord,
		typename CbImplicitConvertToUnion,
		typename CbLambda,
		typename CbLambdaToDynamic,
		typename CbLet,
		typename CbLocalRef,
		typename CbMatch,
		typename CbMessageSend,
		typename CbNewIfaceImpl,
		typename CbParamRef,
		typename CbSeq,
		typename CbSpecialBinary,
		typename CbStructFieldAccess,
		typename CbStructFieldSet
	>
	inline auto match(
		CbBogus cbBogus,
		CbAlloc cbAlloc,
		CbCallConcreteFun cbCallConcreteFun,
		CbCond cbCond,
		CbCreateArr cbCreateArr,
		CbCreateRecord cbCreateRecord,
		CbImplicitConvertToUnion cbImplicitConvertToUnion,
		CbLambda cbLambda,
		CbLambdaToDynamic cbLambdaToDynamic,
		CbLet cbLet,
		CbLocalRef cbLocalRef,
		CbMatch cbMatch,
		CbMessageSend cbMessageSend,
		CbNewIfaceImpl cbNewIfaceImpl,
		CbParamRef cbParamRef,
		CbSeq cbSeq,
		CbSpecialBinary cbSpecialBinary,
		CbStructFieldAccess cbStructFieldAccess,
		CbStructFieldSet cbStructFieldSet
	) const {
		switch (kind) {
			case Kind::bogus:
				return cbBogus(bogus);
			case Kind::alloc:
				return cbAlloc(alloc);
			case Kind::callConcreteFun:
				return cbCallConcreteFun(callConcreteFun);
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
			case Kind::messageSend:
				return cbMessageSend(messageSend);
			case Kind::newIfaceImpl:
				return cbNewIfaceImpl(newIfaceImpl);
			case Kind::paramRef:
				return cbParamRef(paramRef);
			case Kind::seq:
				return cbSeq(seq);
			case Kind::specialBinary:
				return cbSpecialBinary(specialBinary);
			case Kind::structFieldAccess:
				return cbStructFieldAccess(structFieldAccess);
			case Kind::structFieldSet:
				return cbStructFieldSet(structFieldSet);
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
	const Arr<const ConcreteExpr::NewIfaceImpl> allNewIfaceImpls;
	const ConcreteStruct* ctxType;
};
