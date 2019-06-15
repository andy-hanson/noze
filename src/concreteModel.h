#pragma once

#include "./util.h"
#include "./util/SourceRange.h"

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
	const bool isNonSpecializable;
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
	inline ConcreteStructBody(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
	inline ConcreteStructBody(const Fields _fields) : kind{Kind::fields}, fields{_fields} {}
	inline ConcreteStructBody(const Union __union) : kind{Kind::_union}, _union{__union} {}
	inline ConcreteStructBody(const Iface _iface) : kind{Kind::iface}, iface{_iface} {}

	inline bool isBuiltin() const {
		return kind == Kind::builtin;
	}
	inline bool isFields() const {
		return kind == Kind::fields;
	}
	inline bool isUnion() const {
		return kind == Kind::_union;
	}
	inline bool isIface() const {
		return kind == Kind::iface;
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
	mutable bool isStructReferenced = false;
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
		return _sizeBytes.get();
	}

	inline bool defaultIsPointer() const {
		return sizeBytes() > sizeof(void*) * 2;
	}

	inline bool isRecord() const {
		return body().isFields();
	}

	inline bool isUnion() const {
		return body().isUnion();
	}
};

struct ConcreteType {
	const bool isPointer;
	const ConcreteStruct* strukt;

	inline ConcreteType(const bool isp, const ConcreteStruct* s) : isPointer{isp}, strukt{s} {
		const ConcreteStructBody body = strukt->body();
		if (!body.isFields() && !(body.isBuiltin() && body.asBuiltin().info.kind == BuiltinStructKind::_void)) {
			// union/iface are never pointers
			assert(!isPointer);
		}
	}

	static inline const ConcreteType fromStruct(const ConcreteStruct* s) {
		return ConcreteType{s->defaultIsPointer(), s};
	}

	inline bool eq(const ConcreteType other) const {
		return isPointer == other.isPointer && ptrEquals(strukt, other.strukt);
	}

	inline size_t sizeOrPointerSize() const {
		return isPointer ? sizeof(void*) : strukt->sizeBytes();
	}

	// Union and iface should never be pointers
	inline const ConcreteStruct* mustBeNonPointer() const {
		assert(!isPointer);
		return strukt;
	}

	inline const ConcreteType byRef() const {
		return ConcreteType{true, strukt};
	}

	inline const ConcreteType byVal() const {
		return ConcreteType{false, strukt};
	}
};

inline Comparison compareConcreteType(const ConcreteType a, const ConcreteType b) {
	const Comparison res = comparePointer(a.strukt, b.strukt);
	return res != Comparison::equal ? res : compareBool(a.isPointer, b.isPointer);
}

struct ConcreteField {
	const Str mangledName;
	const ConcreteType type;
};

struct ConcreteParam {
	const Str mangledName;
	const ConcreteType type;
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
		const ConcreteStruct* arrayType;

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
		const bool _bool;
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
	inline ConstantKind(const Array a) : kind{Kind::array}, array{a} {}
	inline ConstantKind(const bool a) : kind{Kind::_bool}, _bool{a} {}
	inline ConstantKind(const char a) : kind{Kind::_char}, _char{a} {}
	inline ConstantKind(const FunPtr a) : kind{Kind::funPtr}, funPtr{a} {}
	inline ConstantKind(const Int64 a) : kind{Kind::int64}, int64{a} {}
	inline ConstantKind(const Lambda a) : kind{Kind::lambda}, lambda{a} {}
	inline ConstantKind(const Nat64 a) : kind{Kind::nat64}, nat64{a} {}
	inline ConstantKind(const Null a) : kind{Kind::_null}, _null{a} {}
	inline ConstantKind(const Ptr a) : kind{Kind::ptr}, ptr{a} {}
	inline ConstantKind(const Record a) : kind{Kind::record}, record{a} {}
	inline ConstantKind(const Union a) : kind{Kind::_union}, _union{a} {}
	inline ConstantKind(const Void a) : kind{Kind::_void}, _void{a} {}

	inline bool isArray() const {
		return kind == Kind::array;
	}
	inline bool isBool() const {
		return kind == Kind::_bool;
	}
	inline bool isChar() const {
		return kind == Kind::_char;
	}
	inline bool isFunPtr() const {
		return kind == Kind::funPtr;
	}
	inline bool isInt64() const {
		return kind == Kind::int64;
	}
	inline bool isLambda() const {
		return kind == Kind::lambda;
	}
	inline bool isNat64() const {
		return kind == Kind::nat64;
	}
	inline bool isNull() const {
		return kind == Kind::_null;
	}
	inline bool isPtr() const {
		return kind == Kind::ptr;
	}
	inline bool isRecord() const {
		return kind == Kind::record;
	}
	inline bool isUnion() const {
		return kind == Kind::_union;
	}
	inline bool isVoid() const {
		return kind == Kind::_void;
	}

	inline Array asArray() const {
		assert(isArray());
		return array;
	}
	inline bool asBool() const {
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
	const ConstantKind kind;
	const size_t id;
	mutable bool isConstantReferenced;
	inline Constant(const ConstantKind _kind, const size_t _id)
		: kind{_kind}, id{_id}, isConstantReferenced{false} {}
};

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
	inline ConstantOrLambdaOrVariable(const Variable _variable) : kind{Kind::variable}, variable{_variable} {}
	inline ConstantOrLambdaOrVariable(const Constant* _constant)
		: kind{Kind::constant}, constant{_constant} {}
	inline ConstantOrLambdaOrVariable(const KnownLambdaBody* _knownLambdaBody)
		: kind{Kind::knownLambdaBody}, knownLambdaBody{_knownLambdaBody} {}

	inline bool isVariable() const {
		return kind == Kind::variable;
	}
	inline bool isConstant() const {
		return kind == Kind::constant;
	}
	inline bool isKnownLambdaBody() const {
		return kind == Kind::knownLambdaBody;
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
	inline ConstantOrExpr(const Constant* _constant) : kind{Kind::constant}, constant{_constant} {}
	inline ConstantOrExpr(const ConcreteExpr* _concreteExpr) : kind{Kind::concreteExpr}, concreteExpr{_concreteExpr} {}

	inline bool isConstant() const {
		return kind == Kind::constant;
	}
	inline bool isConcreteExpr() const {
		return kind == Kind::concreteExpr;
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
	inline ConcreteFunBody(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
	inline ConcreteFunBody(const Extern __extern) : kind{Kind::_extern}, _extern{__extern} {}
	inline ConcreteFunBody(const Constant* _constant) : kind{Kind::constant}, constant{_constant} {}
	inline ConcreteFunBody(const ConcreteExpr* _concreteExpr) : kind{Kind::concreteExpr}, concreteExpr{_concreteExpr} {}

	inline bool isBuiltin() const {
		return kind == Kind::builtin;
	}
	inline bool isExtern() const {
		return kind == Kind::_extern;
	}
	inline bool isConstant() const {
		return kind == Kind::constant;
	}
	inline bool isConcreteExpr() const {
		return kind == Kind::concreteExpr;
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
	const bool needsCtx;
	const Opt<const ConcreteType> closureType;
	// Note: This does not include the ctx or closure params.
	const ConcreteSig sig;
	const bool isCallFun; // `call` is not a builtin, but we treat it specially

	Late<const ConcreteFunBody> _body = {};
	size_t nextNewIfaceImplIndex = 0;
	size_t nextLambdaIndex = 0;

	// We create ConcreteFun lazily, but still possible that we'll create it,
	// then discover that it can always be evaluated as a constant and doesn't need to be in the compiled program.
	mutable bool isFunReferenced = false;

	inline ConcreteFun(const bool _needsCtx, const Opt<const ConcreteType> _closureType, const ConcreteSig _sig, const bool _isCallFun)
		: needsCtx{_needsCtx}, closureType{_closureType}, sig{_sig}, isCallFun{_isCallFun} {}

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
	inline const Arr<const ConcreteParam> paramsExcludingClosure() const {
		return sig.params;
	}
	inline size_t arity() const {
		return sig.arity();
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
	// Note: closure is not part of the sig.
	const ConcreteSig nonSpecializedSig; // When we instantiate this to a ConcreteFun, we may specialize the signature.
	// Used for the closure struct, and for generating mangled names of funs that specialize on this
	const Str mangledName;
	// The 'closure' type will only contain those closure fields which are not constant.
	const Opt<const ConcreteType> closureType;
	// For each closure in the Expr, we may make it a constant or KnownLambdaBody.
	const Arr<const ConstantOrLambdaOrVariable> closureSpecialize;

	// We'll generate this (and cache it here) if this needs to be converted to Fun. This never specializes on args.
	// The dynamic fun will have a closure parameter added even if we don't need it,
	// because all lambdas that might be dynamically called need identical signatures.
	mutable Late<const ConcreteFun*> dynamicInstance;
	// When this is called directly, we will instantiate it -- this may remove parameters that are constants.
	mutable MutDict<
		const Arr<const ConstantOrLambdaOrVariable>,
		const ConcreteFun*,
		compareConstantOrLambdaOrVariableArr
	> directCallInstances;

	KnownLambdaBody(const KnownLambdaBody&) = default;
	KnownLambdaBody(
		const ConcreteSig _nonSpecializedSig,
		const Str _mangledName,
		const Opt<const ConcreteType> _closureType,
		const Arr<const ConstantOrLambdaOrVariable> _closureSpecialize
	) :
		nonSpecializedSig{_nonSpecializedSig},
		mangledName{_mangledName},
		closureType{_closureType},
		closureSpecialize{_closureSpecialize}
	{
		// Closure is passed as void*, so it must fit in that size.
		assert(!closureType.has() || closureType.force().sizeOrPointerSize() == sizeof(void*));
	}

	inline bool hasClosure() const {
		return closureType.has();
	}

	inline const Arr<const ConcreteField> closureFields() {
		return closureType.has()
			? closureType.force().strukt->body().asFields().fields
			: emptyArr<const ConcreteField>();
	}

	inline const Str closureStructMangledName() const {
		assert(hasClosure());
		return mangledName;
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

	struct CallConcreteFun {
		const ConcreteFun* called;
		// Note: this should filter out any constant args that 'called' specialized on as constants.
		// (But may include non-specialized-on constant args.)
		const Arr<const ConstantOrExpr> args;

		inline CallConcreteFun(const ConcreteFun* c, const Arr<const ConstantOrExpr> a) : called{c}, args{a} {
			const size_t closureParams = called->closureType.has() ? 1 : 0;
			const size_t other = called->paramsExcludingClosure().size;
			assert(closureParams + other == args.size);
		}
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

	struct CreateRecord {
		const ConcreteType type;
		const Opt<const ConcreteFun*> alloc;
		const Arr<const ConstantOrExpr> args;

		inline CreateRecord(const ConcreteType _type, const Opt<const ConcreteFun*> _alloc, const Arr<const ConstantOrExpr> _args)
			: type{_type}, alloc{_alloc}, args{_args} {
			assert(alloc.has() == type.isPointer);
		}
	};

	struct ImplicitConvertToUnion {
		const ConcreteType unionType;
		const ConcreteType memberType;
		// TODO:PERF support unions as constants; then the arg here will never be a constant
		const ConstantOrExpr arg;

		inline ImplicitConvertToUnion(const ConcreteType _unionType, const ConcreteType _memberType, const ConstantOrExpr _arg)
			: unionType{_unionType}, memberType{_memberType}, arg{_arg} {
			assert(exists(unionType.strukt->body().asUnion().members, [&](const ConcreteType ct) {
				return ct.eq(memberType);
			}));
		}
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
		const ConcreteExpr* inner;
		const ConcreteFun* fun; // instantiation of 'inner'.

		inline LambdaToDynamic(const ConcreteExpr* _inner, const ConcreteFun* _fun)
			: inner{_inner}, fun{_fun} {
			assert(inner->knownLambdaBody().has());
		}
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
		const ConstantOrExpr target; // TODO: should never be a constant
		const ConcreteField* field;
	};

private:
	enum class Kind {
		bogus,
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
	};
	const SourceRange _range;
	// If the expression has a KnownLambdaBody, we do not pass it as a Fun.
	// We pass it as its closure. (If that were empty this would be a Constant and not a ConcreteExpr.)
	const Opt<const KnownLambdaBody*> _knownLambdaBody;
	const Kind kind;
	union {
		const Bogus bogus;
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
	};

public:
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Bogus a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::bogus}, bogus{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const CallConcreteFun a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::callConcreteFun}, callConcreteFun{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Cond a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::cond}, cond{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const CreateArr a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::createArr}, createArr{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const CreateRecord a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::createRecord}, createRecord{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const ImplicitConvertToUnion a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::implicitConvertToUnion}, implicitConvertToUnion{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Lambda a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::lambda}, lambda{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const LambdaToDynamic a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::lambdaToDynamic}, lambdaToDynamic{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Let a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::let}, let{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const LocalRef a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::localRef}, localRef{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Match a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::match}, _match{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const MessageSend a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::messageSend}, messageSend{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const NewIfaceImpl a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::newIfaceImpl}, newIfaceImpl{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const ParamRef a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::paramRef}, paramRef{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const Seq a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::seq}, seq{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const SpecialBinary a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::specialBinary}, specialBinary{a} {}
	inline ConcreteExpr(const SourceRange range, const Opt<const KnownLambdaBody*> klb, const StructFieldAccess a)
		: _range{range}, _knownLambdaBody{klb}, kind{Kind::structFieldAccess}, structFieldAccess{a} {}

	inline bool isCond() const {
		return kind == Kind::cond;
	}

	inline const SourceRange range() const {
		return _range;
	}

	inline const Opt<const KnownLambdaBody*> knownLambdaBody() const {
		return _knownLambdaBody;
	}

	template <
		typename CbBogus,
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
		typename CbStructFieldAccess
	>
	inline auto match(
		CbBogus cbBogus,
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
		CbStructFieldAccess cbStructFieldAccess
	) const {
		switch (kind) {
			case Kind::bogus:
				return cbBogus(bogus);
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
};
