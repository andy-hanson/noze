#pragma once

#include "../util/path.h"
#include "../util/sourceRange.h"
#include "../util/sym.h"

struct NameAndRange {
	const SourceRange range;
	const Sym name;
};

struct TypeAst {
	struct TypeParam {
		const SourceRange range;
		const Sym name;
	};

	struct InstStruct {
		const SourceRange range;
		const Sym name;
		const Arr<const TypeAst> typeArgs;
	};

private:
	enum class Kind {
		typeParam,
		instStruct
	};
	const Kind kind;
	union {
		const TypeParam typeParam;
		const InstStruct instStruct;
	};

public:
	explicit inline TypeAst(const TypeAst::TypeParam t) : kind{Kind::typeParam}, typeParam{t} {}
	explicit inline TypeAst(const TypeAst::InstStruct i) : kind{Kind::instStruct}, instStruct{i} {}

	template <typename CbTypeParam, typename CbInstStruct>
	inline auto match(CbTypeParam cbTypeParam, CbInstStruct cbInstStruct) const {
		switch (kind) {
			case Kind::typeParam:
				return cbTypeParam(typeParam);
			case Kind::instStruct:
				return cbInstStruct(instStruct);
			default:
				assert(0);
		}
	}
};

// Things that store ExprAst by value must go after it.
// Those that are stored by value must go before it.

struct ExprAst;

struct CallAst {
	const Sym funName;
	const Arr<const TypeAst> typeArgs;
	const Arr<const ExprAst> args;
};

struct CondAst {
	const ExprAst* cond;
	const ExprAst* then;
	const ExprAst* elze;
};

struct CreateArrAst {
	const Opt<const TypeAst> elementType;
	const Arr<const ExprAst> args;
};

struct CreateRecordAst {
	const Opt<const TypeAst> type;
	const Arr<const ExprAst> args;
};

struct FunAsLambdaAst {
	const Sym funName;
	const Arr<const TypeAst> typeArgs;
};

struct IdentifierAst {
	const Sym name;
};

struct LambdaAst {
	struct Param {
		const SourceRange range;
		const Sym name;
	};

	const Arr<const Param> params;
	const ExprAst* body;
};

struct LetAst {
	const NameAndRange name;
	const ExprAst* initializer;
	const ExprAst* then;
};

struct LiteralAst {
	enum class Kind {
		numeric,
		string
	};

	const Kind kind;
	const Str literal;
};

struct MatchAst {
	struct CaseAst {
		const SourceRange range;
		const Sym structName;
		const Opt<const NameAndRange> local;
		const ExprAst* then;
	};

	const ExprAst* matched;
	const Arr<const CaseAst> cases;
};

struct MessageSendAst {
	const ExprAst* target;
	const Sym messageName;
	const Arr<const ExprAst> args;
};

struct NewActorAst {
	struct Field {
		const Bool isMutable;
		const Sym name;
		const ExprAst* expr;
	};

	struct MessageImpl {
		const Sym name;
		const Arr<const NameAndRange> paramNames;
		const ExprAst* body;
	};

	const Arr<const Field> fields;
	const Arr<const MessageImpl> messages;
};

struct SeqAst {
	const ExprAst* first;
	const ExprAst* then;
};

struct RecordFieldSetAst {
	const ExprAst* target;
	const Sym fieldName;
	const ExprAst* value;
};

struct ThenAst {
	const LambdaAst::Param left;
	const ExprAst* futExpr;
	const ExprAst* then;
};

struct ExprAstKind {
private:
	enum class Kind {
		call,
		cond,
		createArr,
		createRecord,
		funAsLambda,
		identifier,
		lambda,
		let,
		literal,
		match,
		messageSend,
		newActor,
		seq,
		// no RecordFieldAccess, that's just 'call'
		recordFieldSet,
		then,
	};
	const Kind kind;
	union {
		const CallAst call;
		const CondAst cond;
		const CreateArrAst createArr;
		const CreateRecordAst createRecord;
		const FunAsLambdaAst funAsLambda;
		const IdentifierAst identifier;
		const LambdaAst lambda;
		const LetAst let;
		const LiteralAst literal;
		const MatchAst _match;
		const MessageSendAst messageSend;
		const NewActorAst newActor;
		const SeqAst seq;
		const RecordFieldSetAst recordFieldSet;
		const ThenAst then;
	};

public:
	explicit inline ExprAstKind(const CallAst a) : kind{Kind::call}, call{a} {}
	explicit inline ExprAstKind(const CondAst a) : kind{Kind::cond}, cond{a} {}
	explicit inline ExprAstKind(const CreateArrAst a) : kind{Kind::createArr}, createArr{a} {}
	explicit inline ExprAstKind(const CreateRecordAst a) : kind{Kind::createRecord}, createRecord{a} {}
	explicit inline ExprAstKind(const FunAsLambdaAst a) : kind{Kind::funAsLambda}, funAsLambda{a} {}
	explicit inline ExprAstKind(const IdentifierAst a) : kind{Kind::identifier}, identifier{a} {}
	explicit inline ExprAstKind(const LambdaAst a) : kind{Kind::lambda}, lambda{a} {}
	explicit inline ExprAstKind(const LetAst a) : kind{Kind::let}, let{a} {}
	explicit inline ExprAstKind(const LiteralAst a) : kind{Kind::literal}, literal{a} {}
	explicit inline ExprAstKind(const MatchAst a) : kind{Kind::match}, _match{a} {}
	explicit inline ExprAstKind(const MessageSendAst a) : kind{Kind::messageSend}, messageSend{a} {}
	explicit inline ExprAstKind(const NewActorAst a) : kind{Kind::newActor}, newActor{a} {}
	explicit inline ExprAstKind(const SeqAst a) : kind{Kind::seq}, seq{a} {}
	explicit inline ExprAstKind(const RecordFieldSetAst a) : kind{Kind::recordFieldSet}, recordFieldSet{a} {}
	explicit inline ExprAstKind(const ThenAst a) : kind{Kind::then}, then{a} {}

	inline const Bool isIdentifier() const {
		return enumEq(kind, Kind::identifier);
	}

	inline const IdentifierAst asIdentifier() const {
		assert(isIdentifier());
		return identifier;
	}

	inline bool isCall() const {
		return kind == Kind::call;
	}

	inline const CallAst asCall() const {
		assert(isCall());
		return call;
	}

	template <
		typename CbCall,
		typename CbCond,
		typename CbCreateArr,
		typename CbCreateRecord,
		typename CbFunAsLambda,
		typename CbIdentifier,
		typename CbLambda,
		typename CbLet,
		typename CbLiteral,
		typename CbMatch,
		typename CbMessageSend,
		typename CbNewActor,
		typename CbSeq,
		typename CbRecordFieldSet,
		typename CbThen
	>
	inline auto match(
		CbCall cbCall,
		CbCond cbCond,
		CbCreateArr cbCreateArr,
		CbCreateRecord cbCreateRecord,
		CbFunAsLambda cbFunAsLambda,
		CbIdentifier cbIdentifier,
		CbLambda cbLambda,
		CbLet cbLet,
		CbLiteral cbLiteral,
		CbMatch cbMatch,
		CbMessageSend cbMessageSend,
		CbNewActor cbNewActor,
		CbSeq cbSeq,
		CbRecordFieldSet cbRecordFieldSet,
		CbThen cbThen
	) const {
		switch (kind) {
			case Kind::call:
				return cbCall(call);
			case Kind::cond:
				return cbCond(cond);
			case Kind::createArr:
				return cbCreateArr(createArr);
			case Kind::createRecord:
				return cbCreateRecord(createRecord);
			case Kind::funAsLambda:
				return cbFunAsLambda(funAsLambda);
			case Kind::identifier:
				return cbIdentifier(identifier);
			case Kind::lambda:
				return cbLambda(lambda);
			case Kind::let:
				return cbLet(let);
			case Kind::literal:
				return cbLiteral(literal);
			case Kind::match:
				return cbMatch(_match);
			case Kind::messageSend:
				return cbMessageSend(messageSend);
			case Kind::newActor:
				return cbNewActor(newActor);
			case Kind::seq:
				return cbSeq(seq);
			case Kind::recordFieldSet:
				return cbRecordFieldSet(recordFieldSet);
			case Kind::then:
				return cbThen(then);
			default:
				assert(0);
		}

	}
};

struct ExprAst {
	const SourceRange range;
	const ExprAstKind kind;
};

// Unlike TypeAst.TypeParam, this is the *declaration*
struct TypeParamAst {
	const SourceRange range;
	const Sym name;
};

struct ParamAst {
	const SourceRange range;
	const Sym name;
	const TypeAst type;
};

struct SpecUseAst {
	const SourceRange range;
	const Sym spec;
	const Arr<const TypeAst> typeArgs;
};

struct SigAst {
	const SourceRange range;
	const Sym name;
	const TypeAst returnType;
	const Arr<const ParamAst> params;
};

enum class PuritySpecifier {
	sendable,
	forceSendable,
	mut,
};

struct StructAliasAst {
	const SourceRange range;
	const Bool isPublic;
	const Sym name;
	const Arr<const TypeParamAst> typeParams;
	const TypeAst::InstStruct target;
};

enum class ExplicitByValOrRef {
	byVal,
	byRef
};

struct StructDeclAst {
	struct Body {
		struct Builtin {};
		struct Record {
			struct Field {
				const SourceRange range;
				const Bool isMutable;
				const Sym name;
				const TypeAst type;
			};
			const Opt<const ExplicitByValOrRef> explicitByValOrRef;
			const Arr<const Field> fields;
		};
		struct Union {
			const Arr<const TypeAst::InstStruct> members;
		};
		struct Iface {
			const Arr<const SigAst> messages;
		};

		enum class Kind {
			builtin,
			record,
			_union,
			iface
		};
	private:
		const Kind kind;
		union {
			const Builtin builtin;
			const Record record;
			const Union _union;
			const Iface iface;
		};

	public:
		explicit inline Body(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
		explicit inline Body(const Record _record) : kind{Kind::record}, record{_record} {}
		explicit inline Body(const Union _union) : kind{Kind::_union}, _union{_union} {}
		explicit inline Body(const Iface _iface) : kind{Kind::iface}, iface{_iface} {}

		inline const Bool isRecord() const {
			return enumEq(kind, Kind::record);
		}
		inline const Bool isUnion() const {
			return enumEq(kind, Kind::_union);
		}
		inline const Bool isIface() const {
			return enumEq(kind, Kind::iface);
		}

		template <typename CbBuiltin, typename CbRecord, typename CbUnion, typename CbIface>
		inline auto match(CbBuiltin cbBuiltin, CbRecord cbRecord, CbUnion cbUnion, CbIface cbIface) const {
			switch (kind) {
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

	const SourceRange range;
	const Bool isPublic;
	const Sym name;
	const Arr<const TypeParamAst> typeParams;
	const Opt<const PuritySpecifier> purity;
	const Body body;
};

struct SpecDeclAst {
	const SourceRange range;
	const Bool isPublic;
	const Sym name;
	const Arr<const TypeParamAst> typeParams;
	const Arr<const SigAst> sigs;
};

struct FunBodyAst {
	struct Builtin {};
	struct Extern {};
	enum class Kind {
		builtin,
		_extern,
		exprAst,
	};
private:
	const Kind kind;
	union {
		const Builtin builtin;
		const Extern _extern;
		const ExprAst exprAst;
	};
public:
	explicit inline FunBodyAst(const Builtin _builtin) : kind{Kind::builtin}, builtin{_builtin} {}
	explicit inline FunBodyAst(const Extern __extern) : kind{Kind::_extern}, _extern{__extern} {}
	explicit inline FunBodyAst(const ExprAst _exprAst) : kind{Kind::exprAst}, exprAst{_exprAst} {}

	template <typename CbBuiltin, typename CbExtern, typename CbExprAst>
	inline auto match(CbBuiltin cbBuiltin, CbExtern cbExtern, CbExprAst cbExprAst) const {
		switch (kind) {
			case Kind::builtin:
				return cbBuiltin(builtin);
			case Kind::_extern:
				return cbExtern(_extern);
			case Kind::exprAst:
				return cbExprAst(exprAst);
			default:
				assert(0);
		}
	}
};

struct FunDeclAst {
	const Bool isPublic;
	// Note: if this is empty, the function may still have type params and they should be inferred
	const Arr<const TypeParamAst> typeParams;
	const SigAst sig;
	const Arr<const SpecUseAst> specUses;
	const Bool noCtx;
	const Bool summon;
	const Bool unsafe;
	const Bool trusted;
	const FunBodyAst body;
};

struct ImportAst {
	const SourceRange range;
	const uint nDots;
	const Path* path;
};

struct FileAst {
	const Arr<const ImportAst> imports;
	const Arr<const SpecDeclAst> specs;
	const Arr<const StructAliasAst> structAliases;
	const Arr<const StructDeclAst> structs;
	const Arr<const FunDeclAst> funs;
};

