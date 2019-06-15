#pragma once

#include "./Path.h"
#include "./util.h"
#include "./util/lineAndColumnGetter.h"
#include "./util/SourceRange.h"

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
	return res == Comparison::equal ? comparePath(a.path, b.path) : res;
}

using LineAndColumnGetters = Dict<const PathAndStorageKind, const LineAndColumnGetter, comparePathAndStorageKind>;

using Identifier = Str;

enum class Purity {
	data,
	sendable,
	nonSendable,
};

inline bool isPurityWorse(const Purity a, const Purity b) {
	switch (a) {
		case Purity::data:
			return false;
		case Purity::sendable:
			return b == Purity::data;
		case Purity::nonSendable:
			return true;
		default:
			assert(0);
	}
}

inline Purity worsePurity(const Purity a, const Purity b) {
	return isPurityWorse(a, b) ? a : b;
}

struct TypeParam {
	const SourceRange range;
	const Identifier name;
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
	inline Type(const Bogus _bogus) : kind{Kind::bogus}, bogus{_bogus} {}
	inline Type(const TypeParam* _typeParam) : kind{Kind::typeParam}, typeParam{_typeParam} {}
	inline Type(const StructInst* _structInst) : kind{Kind::structInst}, structInst{_structInst} {}

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

	inline bool isBogus() const {
		return kind == Kind::bogus;
	}
	inline bool isTypeParam() const {
		return kind == Kind::typeParam;
	}
	inline const TypeParam* asTypeParam() const {
		assert(isTypeParam());
		return typeParam;
	}
	inline bool isStructInst() const {
		return kind == Kind::structInst;
	}
	inline const StructInst* asStructInst() const {
		assert(isStructInst());
		return structInst;
	}

	bool containsUnresolvedTypeParams() const;
	bool typeEquals(const Type other) const;
	Purity purity() const;
};

//TODO:MOVE?
inline bool typeEquals(const Type a, const Type b) {
	return a.match(
		[&](const Type::Bogus) {
			return b.isBogus();
		},
		[&](const TypeParam* p) {
			return b.isTypeParam() && ptrEquals(p, b.asTypeParam());
		},
		[&](const StructInst* s) {
			return b.isStructInst() && ptrEquals(s, b.asStructInst());
		});
}

struct Param {
	const SourceRange range;
	const Identifier name;
	const Type type;
	const size_t index;
};

struct Sig {
	const SourceRange range;
	const Identifier name;
	const Type returnType;
	const Arr<const Param> params;

	inline size_t arity() const {
		return params.size;
	}
};

struct Message {
	Sig sig;
	const size_t index;
};

struct StructField {
	const Type type;
	const Identifier name;
	const size_t index;
};

struct StructBody {
	struct Builtin {};
	struct Fields {
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
	inline StructBody(const Builtin _builtin)
		: kind{Kind::builtin}, builtin{_builtin} {}
	inline StructBody(const Fields _fields)
		: kind{Kind::fields}, fields{_fields} {}
	inline StructBody(const Union __union)
		: kind{Kind::_union}, _union{__union} {}
	inline StructBody(const Iface _iface)
		: kind{Kind::iface}, iface{_iface} {}

	inline bool isBuiltin() const {
		return kind == Kind::builtin;
	}
	inline bool isFields() const {
		return kind == Kind::fields;
	}
	inline const Fields asFields() const {
		assert(isFields());
		return fields;
	}
	inline bool isUnion() const {
		return kind == Kind::_union;
	}
	inline const Union asUnion() const {
		assert(isUnion());
		return _union;
	}
	inline bool isIface() const {
		return kind == Kind::iface;
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

struct StructAlias {
	const SourceRange range;
	const bool isPublic;
	const Identifier name;
	const Arr<const TypeParam> typeParams;
private:
	Late<const StructInst*> _target;
public:
	StructAlias(const SourceRange _range, const bool _isPublic, const Identifier _name, const Arr<const TypeParam> _typeParams)
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
	const bool isPublic;
	const Identifier name;
	const Arr<const TypeParam> typeParams;
	// Note: purity on the decl does not take type args into account
	const Purity purity;
	mutable MutArr<const StructInst*> insts {};
private:
	Late<const StructBody> _body {};
public:
	inline bool bodyIsSet() const {
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

	inline StructDecl(const SourceRange r, const bool isp, const Identifier n, const Arr<const TypeParam> tps, const Purity p)
		: range{r}, isPublic{isp}, name{n}, typeParams{tps}, purity{p} {}
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

struct SpecDecl {
	const SourceRange range;
	const bool isPublic;
	const Identifier name;
	const Arr<const TypeParam> typeParams;
	const Arr<const Sig> sigs;
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
	inline FunBody(const Kind _kind) : kind{_kind} {
		assert(kind == Kind::builtin || kind == Kind::_extern);
	}
public:
	inline FunBody(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
	inline FunBody(const Extern __extern) : kind{Kind::_extern}, _extern{__extern} {}
	inline FunBody(const Expr* _expr) : kind{Kind::expr}, expr{_expr} {}

	inline bool isBuiltin() const {
		return kind == Kind::builtin;
	}
	inline bool isExtern() const {
		return kind == Kind::_extern;
	}
	inline bool isExpr() const {
		return kind == Kind::expr;
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
	const bool noCtx;
	const bool summon;
	const bool unsafe;
	const bool trusted;

	static inline FunFlags none() {
		return FunFlags{false, false, false, false};
	}
};

struct SpecUse {
	const SpecDecl* spec;
	const Arr<const Type> typeArgs;
	const size_t index;
};

struct FunDecl {
	const bool isPublic;
	const FunFlags flags;
	const Sig sig;
	const Arr<const TypeParam> typeParams;
	const Arr<const SpecUse> specs;
	Late<FunBody> _body {};

	inline const FunBody body() const {
		return _body.get();
	}
	inline void setBody(const FunBody body) {
		_body.set(body);
	}

	inline bool isBuiltin() const {
		return body().isBuiltin();
	}

	inline bool isExtern() const {
		return body().isExtern();
	}

	inline bool noCtx() const {
		return flags.noCtx;
	}

	inline const Identifier name() const {
		return sig.name;
	}

	inline const Type returnType() const {
		return sig.returnType;
	}

	inline const Arr<const Param> params() const {
		return sig.params;
	}

	inline bool arity() const {
		return sig.arity();
	}

	inline bool isGeneric() const {
		return !isEmpty(typeParams) || !isEmpty(specs);
	}

	inline bool isSummon() const {
		return flags.summon;
	}
};

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
	inline StructOrAlias(const StructAlias* _alias) : kind{Kind::alias}, alias{_alias} {}
	inline StructOrAlias(const StructDecl* _decl) : kind{Kind::decl}, decl{_decl} {}

	inline bool isAlias() const {
		return kind == Kind::alias;
	}
	inline bool isDecl() const {
		return kind == Kind::decl;
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

	inline bool isPublic() const {
		return match(
			[](const StructAlias* a) { return a->isPublic; },
			[](const StructDecl* d) { return d->isPublic; }
		);
	}

	inline const Str name() const {
		return match(
			[](const StructAlias* a) { return a->name; },
			[](const StructDecl* d) { return d->name; }
		);
	}
};

using StructsAndAliasesMap = Dict<const Str, const StructOrAlias, compareStr>;
using SpecsMap = Dict<const Str, const SpecDecl*, compareStr>;
using FunsMap = MultiDict<Str, const FunDecl*, compareStr>;

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
	const StructInst* int64; // 'main' returns this
	const StructInst* str; // Needed for str literals
	const StructInst* _void;
	const FixArr<3, const StructDecl*> optionSomeNone;
	const StructDecl* byVal;
	const StructDecl* arr;
	const StructDecl* mutArr;
	const StructDecl* fut;
	const FixArr<N_FUN_TYPES, const StructDecl*> funTypes;
	const FixArr<N_FUN_TYPES, const StructDecl*> remoteFunTypes;

	struct LambdaInfo {
		const bool isRemote;
		const StructDecl* nonRemote;
	};

	inline bool isNonRemoteFunType(const StructDecl* s) const {
		for (const StructDecl* p : funTypes)
			if (ptrEquals(p, s))
				return true;
		return false;
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
	const Identifier name;
	const Type type;
};

struct ClosureField {
	const Identifier name;
	const Type type;
	const Expr* expr;
	const size_t index;
};

struct CalledDecl {
	struct SpecUseSig {
		const SpecUse* specUse;
		// TODO: isn't it redundant to have both sig and sigIndex?
		const Sig* sig;
		const size_t sigIndexOverAllSpecUses;

		inline const Str name() const {
			return sig->name;
		}
	};

private:
	enum class Kind {
		funDecl,
		specUseSig,
	};
	const Kind kind;
	union {
		const FunDecl* funDecl;
		const SpecUseSig specUseSig;
	};

public:
	inline CalledDecl(const FunDecl* _funDecl) : kind{Kind::funDecl}, funDecl{_funDecl} {}
	inline CalledDecl(const SpecUseSig _specUseSig) : kind{Kind::specUseSig}, specUseSig{_specUseSig} {}

	inline bool isFunDecl() const {
		return kind == Kind::funDecl;
	}
	inline bool isSpecUseSig() const {
		return kind == Kind::specUseSig;
	}
	inline const FunDecl* asFunDecl() const {
		assert(isFunDecl());
		return funDecl;
	}
	inline const SpecUseSig asSpecUseSig() const {
		assert(isSpecUseSig());
		return specUseSig;
	}

	template <typename CbFunDecl, typename CbSpecUseSig>
	auto match(CbFunDecl cbFunDecl, CbSpecUseSig cbSpecUseSig) const {
		switch (kind) {
			case Kind::funDecl:
				return cbFunDecl(funDecl);
			case Kind::specUseSig:
				return cbSpecUseSig(specUseSig);
			default:
				assert(0);
		}
	}

	inline const Sig* sig() const {
		return match(
			[](const FunDecl* f) { return &f->sig; },
			[](const SpecUseSig s) { return s.sig; });
	}

	inline const Arr<const TypeParam> typeParams() const {
		return match(
			[](const FunDecl* f) {
				return f->typeParams;
			},
			[](const SpecUseSig) {
				return emptyArr<const TypeParam>();
			});
	}

	inline size_t arity() const {
		return sig()->arity();
	}

	inline const Str name() const {
		return match(
			[](const FunDecl* f) { return f->name(); },
			[](const SpecUseSig s) { return s.name(); });
	}

	inline const Type returnType() const {
		return sig()->returnType;
	}

	inline const Arr<const Param> params() const {
		return sig()->params;
	}
};

struct Expr {
	struct Bogus {};

	struct Call {
		struct Called {
			const CalledDecl calledDecl;
			const Arr<const Type> typeArgs;
			// These will be in order of the sigs from the specs of the function we're calling.
			// Note: if there is a generic spec sig, it should be matched by a generic function.
			// Currently a non-generic spec sig can't be matched by a generic function.
			// So, this uses CalledDecl and not Called.
			const Arr<const CalledDecl> specImpls;
		};

		const Type concreteReturnType;
		const Called called;
		const Arr<const Expr> args;

		inline const CalledDecl calledDecl() const {
			return called.calledDecl;
		}

		inline const Arr<const Type> typeArgs() const {
			return called.typeArgs;
		}

		inline const Arr<const CalledDecl> specImpls() const {
			return called.specImpls;
		}
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

	// Currently this only works on a non-generic fun.
	struct FunAsLambda {
		const FunDecl* fun;
		const StructInst* type;
		const bool isRemoteFun;

		inline FunAsLambda(
			const FunDecl* _fun, const StructInst* _type, const bool _isRemoteFun)
			: fun{_fun}, type{_type}, isRemoteFun{_isRemoteFun} {
			assert(!fun->isGeneric());
		}
	};

	struct ImplicitConvertToUnion {
		const StructInst* unionType;
		const StructInst* memberType;
		const Expr* inner;
	};

	// type is the lambda's type (not the body's return type), e.g. a Fun1 or RemoteFun1 instance.
	struct Lambda {
		const Arr<const Param> params;
		const Expr* body;
		const Arr<const ClosureField*> closure;
		const StructInst* type;
		const StructDecl* nonRemoteType;
		const bool isRemoteFun;
		// For RemoteFun this includes 'fut'
		const Type returnType;

		// For a RemoteFun this is missing the 'fut'
		inline const Type nonFutReturnType() const {
			return type->typeArgs[0];
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
	};

	struct NewIfaceImpl {
		struct Field {
			const Identifier name;
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

		inline const Str fieldName() const {
			return field->name;
		}

		inline size_t fieldIndex() const {
			return field->index;
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
		typename CbStructFieldAccess
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
		CbStructFieldAccess cbStructFieldAccess
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
			default:
				assert(0);
		}
	}

	bool typeIsBogus(Arena& arena) const;
	const Type getType(Arena& arena, const CommonTypes& commonTypes) const;
};
