#pragma once

#include "./util/dict.h"
#include "./util/lineAndColumnGetter.h"
#include "./util/late.h"
#include "./util/path.h"
#include "./util/sexpr.h"
#include "./util/sourceRange.h"
#include "./util/sym.h"
#include "./util/writer.h"

// A module came from the global imports directory or locally
enum class StorageKind {
	global,
	local,
};

struct PathAndStorageKind {
	const Path* path;
	const StorageKind storageKind;
};

inline Comparison comparePathAndStorageKind(const PathAndStorageKind a, const PathAndStorageKind b) {
	const Comparison res = comparePrimitive(a.storageKind, b.storageKind);
	return res != Comparison::equal ? res : comparePath(a.path, b.path);
}

struct AbsolutePathsGetter {
	const AbsolutePath globalPath;
	const AbsolutePath localPath;

	inline const AbsolutePath getBasePath(const StorageKind sk) const {
		switch (sk) {
			case StorageKind::global:
				return globalPath;
			case StorageKind::local:
				return localPath;
			default:
				assert(0);
		}
	}

	inline const AbsolutePath getAbsolutePath(Arena* arena, const PathAndStorageKind p) const {
		return addManyChildren(arena, getBasePath(p.storageKind), p.path);
	}
};

using LineAndColumnGetters = Dict<const PathAndStorageKind, const LineAndColumnGetter, comparePathAndStorageKind>;

enum class Purity {
	data,
	sendable,
	mut,
};

inline const Bool isPurityWorse(const Purity a, const Purity b) {
	return gt(static_cast<size_t>(a), static_cast<size_t>(b));
}

inline Purity worsePurity(const Purity a, const Purity b) {
	return isPurityWorse(a, b) ? a : b;
}

struct TypeParam {
	const SourceRange range;
	const Sym name;
	const size_t index;
};

struct StructInst;

struct Type {
	struct Bogus {};
	enum class Kind {
		bogus,
		typeParam,
		structInst,
	};
private:
	const Kind kind;
	union {
		const Bogus bogus;
		const TypeParam* typeParam;
		const StructInst* structInst;
	};
public:
	explicit inline Type(const Bogus _bogus) : kind{Kind::bogus}, bogus{_bogus} {}
	explicit inline Type(const TypeParam* _typeParam) : kind{Kind::typeParam}, typeParam{_typeParam} {}
	explicit inline Type(const StructInst* _structInst) : kind{Kind::structInst}, structInst{_structInst} {}

	template <
		typename CbBogus,
		typename CbTypeParam,
		typename CbStructInst
	>
	inline auto match(
		CbBogus cbBogus,
		CbTypeParam cbTypeParam,
		CbStructInst cbStructInst
	) const {
		switch (kind) {
			case Kind::bogus:
				return cbBogus(bogus);
			case Kind::typeParam:
				return cbTypeParam(typeParam);
			case Kind::structInst:
				return cbStructInst(structInst);
			default:
				assert(0);
		}
	}

	inline const Bool isBogus() const {
		return enumEq(kind, Kind::bogus);
	}
	inline const Bool isTypeParam() const {
		return enumEq(kind, Kind::typeParam);
	}
	inline const TypeParam* asTypeParam() const {
		assert(isTypeParam());
		return typeParam;
	}
	inline const Bool isStructInst() const {
		return enumEq(kind, Kind::structInst);
	}
	inline const StructInst* asStructInst() const {
		assert(isStructInst());
		return structInst;
	}

	const Bool containsUnresolvedTypeParams() const;
	const Bool typeEquals(const Type other) const;
	Purity purity() const;
};

//TODO:MOVE?
inline const Bool typeEquals(const Type a, const Type b) {
	return a.match(
		[&](const Type::Bogus) {
			return b.isBogus();
		},
		[&](const TypeParam* p) {
			return _and(b.isTypeParam(), ptrEquals(p, b.asTypeParam()));
		},
		[&](const StructInst* s) {
			return _and(b.isStructInst(), ptrEquals(s, b.asStructInst()));
		});
}

struct Param {
	const SourceRange range;
	const Sym name;
	const Type type;
	const size_t index;

	inline const Param withType(const Type t) const {
		return Param{range, name, t, index};
	}
};

struct Sig {
	const SourceRange range;
	const Sym name;
	const Type returnType;
	const Arr<const Param> params;
};

inline size_t arity(const Sig s) {
	return s.params.size;
}

struct Message {
	Sig sig;
	const size_t index;
};

struct StructField {
	const SourceRange range;
	const Bool isMutable;
	const Sym name;
	const Type type;
	const size_t index;

	inline const StructField withType(const Type newType) const {
		return StructField{range, isMutable, name, newType, index};
	}
};

enum class ForcedByValOrRef {
	byVal,
	byRef,
};

struct StructBody {
	struct Bogus {};
	struct Builtin {};
	struct Record {
		const Opt<const ForcedByValOrRef> forcedByValOrRef;
		const Arr<const StructField> fields;
	};
	struct Union {
		const Arr<const StructInst*> members;
	};
	struct Iface {
		const Arr<const Message> messages;
	};

private:
	enum class Kind {
		bogus,
		builtin,
		record,
		_union,
		iface,
	};
	const Kind kind;
	union {
		const Bogus bogus;
		const Builtin builtin;
		const Record record;
		const Union _union;
		const Iface iface;
	};

public:
	explicit inline StructBody(const Bogus _bogus)
		: kind{Kind::bogus}, bogus{_bogus} {}
	explicit inline StructBody(const Builtin _builtin)
		: kind{Kind::builtin}, builtin{_builtin} {}
	explicit inline StructBody(const Record _record)
		: kind{Kind::record}, record{_record} {}
	explicit inline StructBody(const Union __union)
		: kind{Kind::_union}, _union{__union} {}
	explicit inline StructBody(const Iface _iface)
		: kind{Kind::iface}, iface{_iface} {}

	inline const Bool isBogus() const {
		return enumEq(kind, Kind::bogus);
	}
	inline const Bool isBuiltin() const {
		return enumEq(kind, Kind::builtin);
	}
	inline const Bool isRecord() const {
		return enumEq(kind, Kind::record);
	}
	inline const Record asRecord() const {
		assert(isRecord());
		return record;
	}
	inline const Bool isUnion() const {
		return enumEq(kind, Kind::_union);
	}
	inline const Union asUnion() const {
		assert(isUnion());
		return _union;
	}
	inline const Bool isIface() const {
		return enumEq(kind, Kind::iface);
	}
	inline const Iface asIface() const {
		assert(isIface());
		return iface;
	}

	template <
		typename CbBogus,
		typename CbBuiltin,
		typename CbRecord,
		typename CbUnion,
		typename CbIface
	>
	inline auto match(
		CbBogus cbBogus,
		CbBuiltin cbBuiltin,
		CbRecord cbRecord,
		CbUnion cbUnion,
		CbIface cbIface
	) const {
		switch (kind) {
			case Kind::bogus:
				return cbBogus(bogus);
			case Kind::builtin:
				return cbBuiltin(builtin);
			case Kind::record:
				return cbRecord(record);
			case Kind::_union:
				return cbUnion(_union);
			case Kind::iface:
				return cbIface(iface);
			default:
				assert(0);
		}
	}
};

struct StructAlias {
	const SourceRange range;
	const Bool isPublic;
	const Sym name;
	const Arr<const TypeParam> typeParams;
private:
	Late<const StructInst*> _target;
public:
	StructAlias(const SourceRange _range, const Bool _isPublic, const Sym _name, const Arr<const TypeParam> _typeParams)
		: range{_range}, isPublic{_isPublic}, name{_name}, typeParams{_typeParams} {}

	inline const StructInst* target() const {
		return _target.get();
	}
	inline void setTarget(const StructInst* value) {
		_target.set(value);
	}
};

struct StructDecl {
	const SourceRange range;
	const Bool isPublic;
	const Sym name;
	const Arr<const TypeParam> typeParams;
	// Note: purity on the decl does not take type args into account
	const Purity purity;
	const Bool forceSendable;
	mutable MutArr<const StructInst*> insts {};
private:
	Late<const StructBody> _body {};
public:
	inline const Bool bodyIsSet() const {
		return _body.isSet();
	}
	inline StructBody body() const {
		return _body.get();
	}
	inline void setBody(StructBody value) {
		if (value.isIface())
			assert(purity == Purity::sendable);
		_body.set(value);
	}

	// TODO: why do I need to specify a constructor here?
	inline StructDecl(const SourceRange _range, const Bool _isPublic, const Sym _name, const Arr<const TypeParam> _typeParams, const Purity _purity, const Bool _forceSendable)
		: range{_range}, isPublic{_isPublic}, name{_name}, typeParams{_typeParams}, purity{_purity}, forceSendable{_forceSendable} {}
};

struct StructInst {
	const StructDecl* decl;
	const Arr<const Type> typeArgs;
	const Purity purity;
private:
	// Like decl->body, but has type args filled in.
	Late<StructBody> _body;
public:
	inline StructInst(const StructDecl* d, const Arr<const Type> t, const Purity p) : decl{d}, typeArgs{t}, purity{p}, _body{} {
		assert(d->typeParams.size == t.size);
	}
	inline StructBody body() const {
		return _body.get();
	}
	inline void setBody(StructBody value) {
		_body.set(value);
	}
};

struct SpecInst;

struct SpecDecl {
	const SourceRange range;
	const Bool isPublic;
	const Sym name;
	const Arr<const TypeParam> typeParams;
	const Arr<const Sig> sigs;
	mutable MutArr<const SpecInst*> insts {};
};

// Don't instantiate directly, use instantiateSpec
struct SpecInst {
	const SpecDecl* decl;
	const Arr<const Type> typeArgs;
	// Instantiated signatures
	const Arr<const Sig> sigs;

	inline const Sym name() const {
		return decl->name;
	}
};

struct Expr;

struct FunBody {
	struct Builtin {};
	struct Extern {};
private:
	enum class Kind {
		builtin,
		_extern,
		expr,
	};
	const Kind kind;
	union {
		const Builtin builtin;
		const Extern _extern;
		const Expr* expr;
	};
public:
	explicit inline FunBody(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
	explicit inline FunBody(const Extern __extern) : kind{Kind::_extern}, _extern{__extern} {}
	explicit inline FunBody(const Expr* _expr) : kind{Kind::expr}, expr{_expr} {}

	inline const Bool isBuiltin() const {
		return enumEq(kind, Kind::builtin);
	}
	inline const Bool isExtern() const {
		return enumEq(kind, Kind::_extern);
	}
	inline const Bool isExpr() const {
		return enumEq(kind, Kind::expr);
	}

	template <typename CbBuiltin, typename CbExtern, typename CbExpr>
	inline auto match(CbBuiltin cbBuiltin, CbExtern cbExtern, CbExpr cbExpr) const {
		switch (kind) {
			case Kind::builtin:
				return cbBuiltin(builtin);
			case Kind::_extern:
				return cbExtern(_extern);
			case Kind::expr:
				return cbExpr(expr);
			default:
				assert(0);
		}
	}
};

struct FunFlags {
	const Bool noCtx;
	const Bool summon;
	const Bool unsafe;
	const Bool trusted;

	static inline FunFlags none() {
		return FunFlags{False, False, False, False};
	}
};

struct FunInst;

struct Module;

struct FunDecl {
	const Bool isPublic;
	const FunFlags flags;
	const Sig sig;
	const Arr<const TypeParam> typeParams;
	const Arr<const SpecInst*> specs;
	Late<FunBody> _body {};
	Late<const Module*> _containingModule {};
	mutable MutArr<const FunInst*> insts {};

	inline const FunBody body() const {
		return _body.get();
	}
	inline void setBody(const FunBody body) {
		_body.set(body);
	}

	inline const Module* containingModule() const {
		return _containingModule.get();
	}
	inline void setContainingModule(const Module* m) {
		_containingModule.set(m);
	}

	inline const SourceRange range() const {
		return sig.range;
	}

	inline const Bool isBuiltin() const {
		return body().isBuiltin();
	}

	inline const Bool isExtern() const {
		return body().isExtern();
	}

	inline const Bool noCtx() const {
		return flags.noCtx;
	}

	inline const Sym name() const {
		return sig.name;
	}

	inline const Type returnType() const {
		return sig.returnType;
	}

	inline const Arr<const Param> params() const {
		return sig.params;
	}

	inline size_t nSpecImpls() const {
		size_t n = 0;
		for (const SpecInst* s : specs)
			n += s->sigs.size;
		return n;
	}

	inline const Bool isTemplate() const {
		return _or(!isEmpty(typeParams), !isEmpty(specs));
	}

	inline const Bool isSummon() const {
		return flags.summon;
	}
};

inline size_t arity(const FunDecl* f) {
	return arity(f->sig);
}

struct Called;

// Cached by FunDecl
// Unlike ConcreteFun, the type arguments here may just be other type parameters.
struct FunInst {
	const FunDecl* decl;
	const Arr<const Type> typeArgs;
	const Arr<const Called> specImpls;
	const Sig sig;

	inline FunInst(const FunDecl* _decl, const Arr<const Type> _typeArgs, const Arr<const Called> _specImpls, const Sig _sig)
		: decl{_decl}, typeArgs{_typeArgs}, specImpls{_specImpls}, sig{_sig} {
		assert(typeArgs.size == decl->typeParams.size);
		assert(specImpls.size == decl->nSpecImpls());
	}

	inline const Sym name() const {
		return decl->name();
	}
};

inline size_t arity(const FunInst* f) {
	return arity(f->decl);
}

struct SpecSig {
	const SpecInst* specInst;
	// Note: this is the instantiated sig
	const Sig* sig;
	const size_t indexOverAllSpecUses;

	inline const Sym name() const {
		return sig->name;
	}
};

// Like 'Called', but we haven't fully instantiated yet. (This is used for Candidate when checking a call expr.)
struct CalledDecl {
private:
	enum class Kind {
		funDecl,
		specSig,
	};
	const Kind kind;
	union {
		const FunDecl* funDecl;
		const SpecSig specSig;
	};

public:
	explicit inline CalledDecl(const FunDecl* _funDecl) : kind{Kind::funDecl}, funDecl{_funDecl} {}
	explicit inline CalledDecl(const SpecSig _specSig) : kind{Kind::specSig}, specSig{_specSig} {}

	template <typename CbFunDecl, typename CbSpecSig>
	auto match(CbFunDecl cbFunDecl, CbSpecSig cbSpecSig) const {
		switch (kind) {
			case Kind::funDecl:
				return cbFunDecl(funDecl);
			case Kind::specSig:
				return cbSpecSig(specSig);
			default:
				assert(0);
		}
	}

	inline const Sig sig() const {
		return match(
			[](const FunDecl* f) {
				return f->sig;
			},
			[](const SpecSig s) {
				return *s.sig;
			});
	}

	inline const Sym name() const {
		return sig().name;
	}

	inline const Type returnType() const {
		return sig().returnType;
	}

	inline const Arr<const Param> params() const {
		return sig().params;
	}

	inline const Arr<const TypeParam> typeParams() const {
		return match(
			[](const FunDecl* f) {
				return f->typeParams;
			},
			[](const SpecSig) {
				return emptyArr<const TypeParam>();
			});
	}
};

inline size_t arity(const CalledDecl c) {
	return arity(c.sig());
}

struct Called {
private:
	enum class Kind {
		funInst,
		specSig,
	};
	const Kind kind;
	union {
		const FunInst* funInst;
		const SpecSig specSig;
	};

public:
	explicit inline Called(const FunInst* _funInst) : kind{Kind::funInst}, funInst{_funInst} {}
	explicit inline Called(const SpecSig _specSig) : kind{Kind::specSig}, specSig{_specSig} {}

	inline const Bool isFunInst() const {
		return enumEq(kind, Kind::funInst);
	}
	inline const Bool isSpecSig() const {
		return enumEq(kind, Kind::specSig);
	}
	inline const FunInst* asFunInst() const {
		assert(isFunInst());
		return funInst;
	}
	inline const SpecSig asSpecSig() const {
		assert(isSpecSig());
		return specSig;
	}

	template <typename CbFunInst, typename CbSpecSig>
	auto match(CbFunInst cbFunInst, CbSpecSig cbSpecSig) const {
		switch (kind) {
			case Kind::funInst:
				return cbFunInst(funInst);
			case Kind::specSig:
				return cbSpecSig(specSig);
			default:
				assert(0);
		}
	}

	inline const Sig sig() const {
		return match(
			[](const FunInst* f) {
				return f->sig;
			},
			[](const SpecSig s) {
				return *s.sig;
			});
	}

	inline const Sym name() const {
		return match(
			[](const FunInst* f) { return f->name(); },
			[](const SpecSig s) { return s.name(); });
	}

	inline const Type returnType() const {
		return sig().returnType;
	}

	inline const Arr<const Param> params() const {
		return sig().params;
	}
};

inline size_t arity(const Called c) {
	return arity(c.sig());
}

struct StructOrAlias {
private:
	enum class Kind {
		alias,
		decl,
	};
	const Kind kind;
	union {
		const StructAlias* alias;
		const StructDecl* decl;
	};

public:
	explicit inline StructOrAlias(const StructAlias* _alias) : kind{Kind::alias}, alias{_alias} {}
	explicit inline StructOrAlias(const StructDecl* _decl) : kind{Kind::decl}, decl{_decl} {}

	inline const Bool isAlias() const {
		return enumEq(kind, Kind::alias);
	}
	inline const Bool isDecl() const {
		return enumEq(kind, Kind::decl);
	}
	inline const StructAlias* asAlias() const {
		assert(isAlias());
		return alias;
	}
	inline const StructDecl* asDecl() const {
		assert(isDecl());
		return decl;
	}

	template <typename CbAlias, typename CbDecl>
	inline auto match(CbAlias cbAlias, CbDecl cbDecl) const {
		switch (kind) {
			case Kind::alias:
				return cbAlias(alias);
			case Kind::decl:
				return cbDecl(decl);
			default:
				assert(0);
		}
	}

	inline const Arr<const TypeParam> typeParams() const {
		return match(
			[](const StructAlias* a) { return a->typeParams; },
			[](const StructDecl* d) { return d->typeParams; }
		);
	}

	inline const SourceRange range() const {
		return match(
			[](const StructAlias* a) { return a->range; },
			[](const StructDecl* d) { return d->range; }
		);
	}

	inline const Bool isPublic() const {
		return match(
			[](const StructAlias* a) { return a->isPublic; },
			[](const StructDecl* d) { return d->isPublic; }
		);
	}

	inline const Sym name() const {
		return match(
			[](const StructAlias* a) { return a->name; },
			[](const StructDecl* d) { return d->name; }
		);
	}
};

using StructsAndAliasesMap = Dict<const Sym, const StructOrAlias, compareSym>;
using SpecsMap = Dict<const Sym, const SpecDecl*, compareSym>;
using FunsMap = MultiDict<const Sym, const FunDecl*, compareSym>;

struct Module {
	const PathAndStorageKind pathAndStorageKind;
	const Arr<const Module*> imports;
	const Arr<const StructDecl> structs;
	const Arr<const SpecDecl> specs;
	const Arr<const FunDecl> funs;
	const StructsAndAliasesMap structsAndAliasesMap;
	const SpecsMap specsMap;
	const FunsMap funsMap;

	const Str name() const {
		return pathAndStorageKind.path->baseName;
	}
};

#define N_FUN_TYPES 3

struct CommonTypes {
	const StructInst* _bool; // Needed for 'when'
	const StructInst* _char;
	const StructInst* ctx;
	const StructInst* int64; // 'main' returns this
	const StructInst* str; // Needed for str literals
	const StructInst* _void;
	const StructInst* anyPtr;
	const Arr<const StructDecl*> optionSomeNone;
	const StructDecl* byVal;
	const StructDecl* arr;
	const StructDecl* mutArr;
	const StructDecl* fut;
	const Arr<const StructDecl*> funTypes;
	const Arr<const StructDecl*> sendFunTypes;

	struct LambdaInfo {
		const Bool isSend;
		const StructDecl* nonSend;
	};

	inline const Bool isNonsendFunType(const StructDecl* s) const {
		for (const StructDecl* p : funTypes)
			if (ptrEquals(p, s))
				return True;
		return False;
	}

	const Opt<const LambdaInfo> getFunStructInfo(const StructDecl* s) const;
};

struct Program {
	const Module* includeModule;
	const Module* mainModule;
	// Includes 'include.nz"'
	const Arr<const Module*> allModules;
	const CommonTypes commonTypes;
	const LineAndColumnGetters lineAndColumnGetters;
};

struct Local {
	const Sym name;
	const Type type;
};

struct ClosureField {
	const Sym name;
	const Type type;
	const Expr* expr;
	const size_t index;
};

struct Expr {
	struct Bogus {};

	struct Call {
		const Called called;
		const Arr<const Expr> args;
	};

	struct ClosureFieldRef {
		const ClosureField* field;
		inline size_t index() const {
			return field->index;
		}
	};

	struct CreateArr {
		const StructInst* arrType;
		const Arr<const Expr> args;

		inline const Type elementType() const {
			return only(arrType->typeArgs);
		}
	};

	struct CreateRecord {
		const StructInst* structInst;
		const Arr<const Expr> args;
	};

	// Currently this only works on a non-template fun.
	struct FunAsLambda {
		const FunDecl* fun;
		const StructInst* type;
		const Bool isSendFun;

		inline FunAsLambda(
			const FunDecl* _fun, const StructInst* _type, const Bool _isSendFun)
			: fun{_fun}, type{_type}, isSendFun{_isSendFun} {
			assert(!fun->isTemplate());
		}
	};

	struct ImplicitConvertToUnion {
		const StructInst* unionType;
		const size_t memberIndex;
		const Expr* inner;
	};

	// type is the lambda's type (not the body's return type), e.g. a Fun1 or sendFun1 instance.
	struct Lambda {
		const Arr<const Param> params;
		const Expr* body;
		const Arr<const ClosureField*> closure;
		// This is the funN type.
		const StructInst* type;
		const StructDecl* nonSendType;
		const Bool isSendFun;
		// For sendFun this includes 'fut'
		const Type returnType;

		// For a sendFun this is missing the 'fut'
		inline const Type nonFutReturnType() const {
			return at(type->typeArgs, 0);
		}
	};

	struct Let {
		const Local* local;
		const Expr* value;
		const Expr* then;
	};

	struct LocalRef {
		const Local* local;
	};

	struct Match {
		struct Case {
			const Opt<const Local*> local;
			const Expr* then;
		};

		const Expr* matched;
		const StructInst* matchedUnion;
		const Arr<const Case> cases;
		const Type type;
	};

	struct MessageSend {
		const Expr* target;
		const StructInst* iface;
		const size_t messageIndex;
		const Arr<const Expr> args;

		inline const Type getType() const {
			return at(iface->body().asIface().messages, messageIndex).sig.returnType;
		}
	};

	struct NewIfaceImpl {
		struct Field {
			const Bool isMutable;
			const Sym name;
			const Expr* expr;
			const Type type;
			const size_t index;
		};

		const StructInst* iface;
		const Arr<const Field> fields;
		// Corresponds to each message of the iface
		const Arr<const Expr> messageImpls; // parmams are on the iface

		inline NewIfaceImpl(
			const StructInst* _iface, const Arr<const Field> _fields, const Arr<const Expr> _messageImpls)
			: iface{_iface}, fields{_fields}, messageImpls{_messageImpls} {
			assert(messageImpls.size == ifaceBody().messages.size);
		}

		inline StructBody::Iface ifaceBody() const {
			return iface->body().asIface();
		}
	};

	struct IfaceImplFieldRef {
		const NewIfaceImpl::Field* field;
	};

	struct ParamRef {
		const Param* param;
	};

	struct Seq {
		const Expr* first;
		const Expr* then;
	};

	struct StringLiteral {
		const Str literal;
	};

	struct StructFieldAccess {
		const Expr* target;
		const StructInst* targetType;
		const StructField* field; // This is the field from the StructInst, not the StructDecl

		//TODO:KILL (just write field->type everywhere)
		inline const Type accessedFieldType() const {
			return field->type;
		}

		inline const Sym fieldName() const {
			return field->name;
		}
	};

	struct StructFieldSet {
		const Expr* target;
		const StructInst* targetType;
		const StructField* field;
		const Expr* value;

		inline StructFieldSet(const Expr* _target, const StructInst* _targetType, const StructField* _field, const Expr* _value)
			: target{_target}, targetType{_targetType}, field{_field}, value{_value} {
			assert(field->isMutable);
		}
	};

	struct Cond {
		const Type type;
		const Expr* cond;
		const Expr* then;
		const Expr* elze;
	};

private:
	enum class Kind {
		bogus,
		call,
		closureFieldRef,
		cond,
		createArr,
		createRecord,
		funAsLambda,
		ifaceImplFieldRef,
		implicitConvertToUnion,
		lambda,
		let,
		localRef,
		match,
		messageSend,
		newIfaceImpl,
		paramRef,
		seq,
		stringLiteral,
		structFieldAccess,
		structFieldSet,
	};
	const SourceRange _range;
	const Kind kind;
	union {
		const Bogus bogus;
		const Call call;
		const ClosureFieldRef closureFieldRef;
		const Cond cond;
		const CreateArr createArr;
		const CreateRecord createRecord;
		const FunAsLambda funAsLambda;
		const IfaceImplFieldRef ifaceImplFieldRef;
		const ImplicitConvertToUnion implicitConvertToUnion;
		const Lambda lambda;
		const Let let;
		const LocalRef localRef;
		const Match _match;
		const MessageSend messageSend;
		const NewIfaceImpl newIfaceImpl;
		const ParamRef paramRef;
		const Seq seq;
		const StringLiteral stringLiteral;
		const StructFieldAccess structFieldAccess;
		const StructFieldSet structFieldSet;
	};
	inline Expr(const SourceRange range, const Kind _kind) : _range{range}, kind{_kind} {
		assert(_kind == Kind::bogus);
	}

public:
	inline Expr(const SourceRange range, const Bogus _bogus)
		: _range{range}, kind{Kind::bogus}, bogus{_bogus} {}
	inline Expr(const SourceRange range, const Call _call)
		: _range{range}, kind{Kind::call}, call{_call} {}
	inline Expr(const SourceRange range, const ClosureFieldRef _closureFieldRef)
		: _range{range}, kind{Kind::closureFieldRef}, closureFieldRef{_closureFieldRef} {}
	inline Expr(const SourceRange range, const Cond _cond)
		: _range{range}, kind{Kind::cond}, cond{_cond} {}
	inline Expr(const SourceRange range, const CreateArr _createArr)
		: _range{range}, kind{Kind::createArr}, createArr{_createArr} {}
	inline Expr(const SourceRange range, const CreateRecord _createRecord)
		: _range{range}, kind{Kind::createRecord}, createRecord{_createRecord} {}
	inline Expr(const SourceRange range, const FunAsLambda _funAsLambda)
		: _range{range}, kind{Kind::funAsLambda}, funAsLambda{_funAsLambda} {}
	inline Expr(const SourceRange range, const IfaceImplFieldRef _ifaceImplFieldRef)
		: _range{range}, kind{Kind::ifaceImplFieldRef}, ifaceImplFieldRef{_ifaceImplFieldRef} {}
	inline Expr(const SourceRange range, const ImplicitConvertToUnion _implicitConvertToUnion)
		: _range{range}, kind{Kind::implicitConvertToUnion}, implicitConvertToUnion{_implicitConvertToUnion} {}
	inline Expr(const SourceRange range, const Lambda _lambda)
		: _range{range}, kind{Kind::lambda}, lambda{_lambda} {}
	inline Expr(const SourceRange range, const Let _let)
		: _range{range}, kind{Kind::let}, let{_let} {}
	inline Expr(const SourceRange range, const LocalRef _localRef)
		: _range{range}, kind{Kind::localRef}, localRef{_localRef} {}
	inline Expr(const SourceRange range, const Match match)
		: _range{range}, kind{Kind::match}, _match{match} {}
	inline Expr(const SourceRange range, const MessageSend _messageSend)
		: _range{range}, kind{Kind::messageSend}, messageSend{_messageSend} {}
	inline Expr(const SourceRange range, const NewIfaceImpl _newIfaceImpl)
		: _range{range}, kind{Kind::newIfaceImpl}, newIfaceImpl{_newIfaceImpl} {}
	inline Expr(const SourceRange range, const ParamRef _paramRef)
		: _range{range}, kind{Kind::paramRef}, paramRef{_paramRef} {}
	inline Expr(const SourceRange range, const Seq _seq)
		: _range{range}, kind{Kind::seq}, seq{_seq} {}
	inline Expr(const SourceRange range, const StringLiteral _stringLiteral)
		: _range{range}, kind{Kind::stringLiteral}, stringLiteral{_stringLiteral} {}
	inline Expr(const SourceRange range, const StructFieldAccess _structFieldAccess)
		: _range{range}, kind{Kind::structFieldAccess}, structFieldAccess{_structFieldAccess} {}
	inline Expr(const SourceRange range, const StructFieldSet _structFieldSet)
		: _range{range}, kind{Kind::structFieldSet}, structFieldSet{_structFieldSet} {}

	inline SourceRange range() const {
		return _range;
	}

	template <
		typename CbBogus,
		typename CbCall,
		typename CbClosureFieldRef,
		typename CbCond,
		typename CbCreateArr,
		typename CbCreateRecord,
		typename CbFunAsLambda,
		typename CbIfaceImplFieldRef,
		typename CbImplicitConvertToUnion,
		typename CbLambda,
		typename CbLet,
		typename CbLocalRef,
		typename CbMatch,
		typename CbMessageSend,
		typename CbNewIfaceImpl,
		typename CbParamRef,
		typename CbSeq,
		typename CbStringLiteral,
		typename CbStructFieldAccess,
		typename CbStructFieldSet
	>
	inline auto match(
		CbBogus cbBogus,
		CbCall cbCall,
		CbClosureFieldRef cbClosureFieldRef,
		CbCond cbCond,
		CbCreateArr cbCreateArr,
		CbCreateRecord cbCreateRecord,
		CbFunAsLambda cbFunAsLambda,
		CbIfaceImplFieldRef cbIfaceImplFieldRef,
		CbImplicitConvertToUnion cbImplicitConvertToUnion,
		CbLambda cbLambda,
		CbLet cbLet,
		CbLocalRef cbLocalRef,
		CbMatch cbMatch,
		CbMessageSend cbMessageSend,
		CbNewIfaceImpl cbNewIfaceImpl,
		CbParamRef cbParamRef,
		CbSeq cbSeq,
		CbStringLiteral cbStringLiteral,
		CbStructFieldAccess cbStructFieldAccess,
		CbStructFieldSet cbStructFieldSet
	) const {
		switch (kind) {
			case Kind::bogus:
				return cbBogus(bogus);
			case Kind::call:
				return cbCall(call);
			case Kind::closureFieldRef:
				return cbClosureFieldRef(closureFieldRef);
			case Kind::cond:
				return cbCond(cond);
			case Kind::createArr:
				return cbCreateArr(createArr);
			case Kind::createRecord:
				return cbCreateRecord(createRecord);
			case Kind::funAsLambda:
				return cbFunAsLambda(funAsLambda);
			case Kind::ifaceImplFieldRef:
				return cbIfaceImplFieldRef(ifaceImplFieldRef);
			case Kind::implicitConvertToUnion:
				return cbImplicitConvertToUnion(implicitConvertToUnion);
			case Kind::lambda:
				return cbLambda(lambda);
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
			case Kind::stringLiteral:
				return cbStringLiteral(stringLiteral);
			case Kind::structFieldAccess:
				return cbStructFieldAccess(structFieldAccess);
			case Kind::structFieldSet:
				return cbStructFieldSet(structFieldSet);
			default:
				assert(0);
		}
	}

	const Bool typeIsBogus(Arena* arena) const;
	const Type getType(Arena* arena, const CommonTypes& commonTypes) const;
};

void writeStructInst(Writer& writer, const StructInst* s);
void writeType(Writer& writer, const Type type);
void writeExpr(Writer& writer, const Expr expr);

const Sexpr typeToSexpr(Arena* arena, const Type type);
const Sexpr exprToSexpr(Arena* arena, const Expr expr);
