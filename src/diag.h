#pragma once

#include "./model.h"

struct ParseDiag {
	enum class Kind {
		expectedCharacter,
		expectedIndent,
		expectedPurityAfterSpace,
		leadingSpace,
		matchWhenNewMayNotAppearInsideArg,
		mustEndInBlankLine,
		trailingSpace,
		typeParamCantHaveTypeArgs,
		unexpectedCharacter,
		unionCantBeEmpty,
		whenMustHaveElse,
	};

	const Kind kind;
	union {
		const char ch;
	};

	inline ParseDiag(const Kind _kind) : kind{_kind} {
		assert(kind != Kind::expectedCharacter && kind != Kind::unexpectedCharacter);
	}
	inline ParseDiag(const Kind _kind, const char _ch) : kind{_kind}, ch{_ch} {
		assert(kind == Kind::expectedCharacter || kind == Kind::unexpectedCharacter);
	}
};

struct Diag {
	struct CantCallNonNoCtx {};
	struct CantCallSummon {};
	struct CantCallUnsafe {};
	struct CantCreateNonRecordStruct {
		const StructDecl* strukt;
	};
	struct CantInferTypeArguments {};
	struct CircularImport {};
	struct CommonTypesMissing {};
	struct DuplicateDeclaration {
		enum class Kind {
			structOrAlias,
			spec,
			iface,
		};
		const Kind kind;
		const Str name;
	};
	struct ExpectedTypeIsNotALambda {
		const Opt<const Type> expectedType;
	};
	struct FileDoesNotExist {};
	struct MultipleFunctionCandidates {
		const Arr<const CalledDecl> candidates;
	};
	struct NameNotFound {
		enum class Kind {
			strukt,
			spec,
			iface,
			typeParam,
		};

		const Str name;
		const Kind kind;
	};
	struct NoSuchFunction {
		const Str name;
	};
	struct ParamShadowsPrevious {
		enum class Kind {
			param,
			typeParam,
		};
		const Kind kind;
	};
	struct ShouldNotHaveTypeParamsInIface {};
	struct TypeConflict {
		const Type expected;
		const Type actual;
	};
	struct TypeNotSendable {};
	struct WrongNumberNewStructArgs {
		const StructDecl* decl;
		const size_t nExpectedArgs;
		const size_t nActualArgs;
	};
	struct WrongNumberTypeArgsForSpec {
		const SpecDecl* decl;
		const size_t nExpectedTypeArgs;
		const size_t nActualTypeArgs;
	};
	struct WrongNumberTypeArgsForStruct {
		const StructOrAlias decl;
		const size_t nExpectedTypeArgs;
		const size_t nActualTypeArgs;
	};

private:
	enum class Kind {
		cantCallNonNoCtx,
		cantCallSummon,
		cantCallUnsafe,
		cantCreateNonRecordStruct,
		cantInferTypeArguments,
		circularImport,
		commonTypesMissing,
		duplicateDeclaration,
		expectedTypeIsNotALambda,
		fileDoesNotExist,
		multipleFunctionCandidates,
		nameNotFound,
		noSuchFunction,
		paramShadowsPrevious,
		parseDiag,
		shouldNotHaveTypeParamsInIface,
		typeConflict,
		typeNotSendable,
		wrongNumberNewStructArgs,
		wrongNumberTypeArgsForSpec,
		wrongNumberTypeArgsForStruct,
	};
	const Kind kind;
	union {
		const CantCallNonNoCtx cantCallNonNoCtx;
		const CantCallSummon cantCallSummon;
		const CantCallUnsafe cantCallUnsafe;
		const CantCreateNonRecordStruct cantCreateNonRecordStruct;
		const CantInferTypeArguments cantInferTypeArguments;
		const CircularImport circularImport;
		const CommonTypesMissing commonTypesMissing;
		const DuplicateDeclaration duplicateDeclaration;
		const ExpectedTypeIsNotALambda expectedTypeIsNotALambda;
		const FileDoesNotExist fileDoesNotExist;
		const MultipleFunctionCandidates multipleFunctionCandidates;
		const NameNotFound nameNotFound;
		const NoSuchFunction noSuchFunction;
		const ParamShadowsPrevious paramShadowsPrevious;
		const ParseDiag parseDiag;
		const ShouldNotHaveTypeParamsInIface shouldNotHaveTypeParamsInIface;
		const TypeConflict typeConflict;
		const TypeNotSendable typeNotSendable;
		const WrongNumberNewStructArgs wrongNumberNewStructArgs;
		const WrongNumberTypeArgsForSpec wrongNumberTypeArgsForSpec;
		const WrongNumberTypeArgsForStruct wrongNumberTypeArgsForStruct;
	};

public:
	inline Diag(const CantCallNonNoCtx d) : kind{Kind::cantCallNonNoCtx}, cantCallNonNoCtx{d} {}
	inline Diag(const CantCallSummon d) : kind{Kind::cantCallSummon}, cantCallSummon{d} {}
	inline Diag(const CantCallUnsafe d) : kind{Kind::cantCallUnsafe}, cantCallUnsafe{d} {}
	inline Diag(const CantCreateNonRecordStruct d) : kind{Kind::cantCreateNonRecordStruct}, cantCreateNonRecordStruct{d} {}
	inline Diag(const CantInferTypeArguments d) : kind{Kind::cantInferTypeArguments}, cantInferTypeArguments{d} {}
	inline Diag(const CircularImport d) : kind{Kind::circularImport}, circularImport{d} {}
	inline Diag(const CommonTypesMissing d) : kind{Kind::commonTypesMissing}, commonTypesMissing{d} {}
	inline Diag(const DuplicateDeclaration d) : kind{Kind::duplicateDeclaration}, duplicateDeclaration{d} {}
	inline Diag(const ExpectedTypeIsNotALambda d) : kind{Kind::expectedTypeIsNotALambda}, expectedTypeIsNotALambda{d} {}
	inline Diag(const FileDoesNotExist d) : kind{Kind::fileDoesNotExist}, fileDoesNotExist{d} {}
	inline Diag(const MultipleFunctionCandidates d) : kind{Kind::multipleFunctionCandidates}, multipleFunctionCandidates{d} {}
	inline Diag(const NameNotFound d) : kind{Kind::nameNotFound}, nameNotFound{d} {}
	inline Diag(const NoSuchFunction d) : kind{Kind::noSuchFunction}, noSuchFunction{d} {}
	inline Diag(const ParamShadowsPrevious d) : kind{Kind::paramShadowsPrevious}, paramShadowsPrevious{d} {}
	inline Diag(const ParseDiag d) : kind{Kind::parseDiag}, parseDiag{d} {}
	inline Diag(const ShouldNotHaveTypeParamsInIface d) : kind{Kind::shouldNotHaveTypeParamsInIface}, shouldNotHaveTypeParamsInIface{d} {}
	inline Diag(const TypeConflict d) : kind{Kind::typeConflict}, typeConflict{d} {}
	inline Diag(const TypeNotSendable d) : kind{Kind::typeNotSendable}, typeNotSendable{d} {}
	inline Diag(const WrongNumberNewStructArgs d) : kind{Kind::wrongNumberNewStructArgs}, wrongNumberNewStructArgs{d} {}
	inline Diag(const WrongNumberTypeArgsForSpec d) : kind{Kind::wrongNumberTypeArgsForSpec}, wrongNumberTypeArgsForSpec{d} {}
	inline Diag(const WrongNumberTypeArgsForStruct d) : kind{Kind::wrongNumberTypeArgsForStruct}, wrongNumberTypeArgsForStruct{d} {}
};

struct Diagnostic {
	const PathAndStorageKind where;
	const SourceRange range;
	const Diag diag;
};

using Diagnostics = const Arr<const Diagnostic>;
