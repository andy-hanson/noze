#pragma once

#include "./model.h"

struct ParseDiag {
	struct ExpectedCharacter {
		const char ch;
	};
	struct ExpectedIndent {};
	struct ExpectedPurityAfterSpace {};
	struct LeadingSpace {};
	struct MatchWhenNewMayNotAppearInsideArg {};
	struct MustEndInBlankLine {};
	struct TrailingSpace {};
	struct TypeParamCantHaveTypeArgs {};
	struct UnexpectedCharacter {
		const char ch;
	};
	struct UnionCantBeEmpty {};
	struct WhenMustHaveElse {};

private:
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
		const ExpectedCharacter expectedCharacter;
		const ExpectedIndent expectedIndent;
		const ExpectedPurityAfterSpace expectedPurityAfterSpace;
		const LeadingSpace leadingSpace;
		const MatchWhenNewMayNotAppearInsideArg matchWhenNewMayNotAppearInsideArg;
		const MustEndInBlankLine mustEndInBlankLine;
		const TrailingSpace trailingSpace;
		const TypeParamCantHaveTypeArgs typeParamCantHaveTypeArgs;
		const UnexpectedCharacter unexpectedCharacter;
		const UnionCantBeEmpty unionCantBeEmpty;
		const WhenMustHaveElse whenMustHaveElse;
	};

public:
	inline ParseDiag(const ExpectedCharacter d) : kind{Kind::expectedCharacter}, expectedCharacter{d} {}
	inline ParseDiag(const ExpectedIndent d) : kind{Kind::expectedIndent}, expectedIndent{d} {}
	inline ParseDiag(const ExpectedPurityAfterSpace d) : kind{Kind::expectedPurityAfterSpace}, expectedPurityAfterSpace{d} {}
	inline ParseDiag(const LeadingSpace d) : kind{Kind::leadingSpace}, leadingSpace{d} {}
	inline ParseDiag(const MatchWhenNewMayNotAppearInsideArg d) : kind{Kind::matchWhenNewMayNotAppearInsideArg}, matchWhenNewMayNotAppearInsideArg{d} {}
	inline ParseDiag(const MustEndInBlankLine d) : kind{Kind::mustEndInBlankLine}, mustEndInBlankLine{d} {}
	inline ParseDiag(const TrailingSpace d) : kind{Kind::trailingSpace}, trailingSpace{d} {}
	inline ParseDiag(const TypeParamCantHaveTypeArgs d) : kind{Kind::typeParamCantHaveTypeArgs}, typeParamCantHaveTypeArgs{d} {}
	inline ParseDiag(const UnexpectedCharacter d) : kind{Kind::unexpectedCharacter}, unexpectedCharacter{d} {}
	inline ParseDiag(const UnionCantBeEmpty d) : kind{Kind::unionCantBeEmpty}, unionCantBeEmpty{d} {}
	inline ParseDiag(const WhenMustHaveElse d) : kind{Kind::whenMustHaveElse}, whenMustHaveElse{d} {}

	template <
		typename CbExpectedCharacter,
		typename CbExpectedIndent,
		typename CbExpectedPurityAfterSpace,
		typename CbLeadingSpace,
		typename CbMatchWhenNewMayNotAppearInsideArg,
		typename CbMustEndInBlankLine,
		typename CbTrailingSpace,
		typename CbTypeParamCantHaveTypeArgs,
		typename CbUnexpectedCharacter,
		typename CbUnionCantBeEmpty,
		typename CbWhenMustHaveElse
	> inline auto match(
		CbExpectedCharacter cbExpectedCharacter,
		CbExpectedIndent cbExpectedIndent,
		CbExpectedPurityAfterSpace cbExpectedPurityAfterSpace,
		CbLeadingSpace cbLeadingSpace,
		CbMatchWhenNewMayNotAppearInsideArg cbMatchWhenNewMayNotAppearInsideArg,
		CbMustEndInBlankLine cbMustEndInBlankLine,
		CbTrailingSpace cbTrailingSpace,
		CbTypeParamCantHaveTypeArgs cbTypeParamCantHaveTypeArgs,
		CbUnexpectedCharacter cbUnexpectedCharacter,
		CbUnionCantBeEmpty cbUnionCantBeEmpty,
		CbWhenMustHaveElse cbWhenMustHaveElse
	) const {
		switch (kind) {
			case Kind::expectedCharacter:
				return cbExpectedCharacter(expectedCharacter);
			case Kind::expectedIndent:
				return cbExpectedIndent(expectedIndent);
			case Kind::expectedPurityAfterSpace:
				return cbExpectedPurityAfterSpace(expectedPurityAfterSpace);
			case Kind::leadingSpace:
				return cbLeadingSpace(leadingSpace);
			case Kind::matchWhenNewMayNotAppearInsideArg:
				return cbMatchWhenNewMayNotAppearInsideArg(matchWhenNewMayNotAppearInsideArg);
			case Kind::mustEndInBlankLine:
				return cbMustEndInBlankLine(mustEndInBlankLine);
			case Kind::trailingSpace:
				return cbTrailingSpace(trailingSpace);
			case Kind::typeParamCantHaveTypeArgs:
				return cbTypeParamCantHaveTypeArgs(typeParamCantHaveTypeArgs);
			case Kind::unexpectedCharacter:
				return cbUnexpectedCharacter(unexpectedCharacter);
			case Kind::unionCantBeEmpty:
				return cbUnionCantBeEmpty(unionCantBeEmpty);
			case Kind::whenMustHaveElse:
				return cbWhenMustHaveElse(whenMustHaveElse);
			default:
				assert(0);
		}
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

	inline bool isFileDoesNotExist() const {
		return kind == Kind::fileDoesNotExist;
	}

	template <
		typename CbCantCallNonNoCtx,
		typename CbCantCallSummon,
		typename CbCantCallUnsafe,
		typename CbCantCreateNonRecordStruct,
		typename CbCantInferTypeArguments,
		typename CbCircularImport,
		typename CbCommonTypesMissing,
		typename CbDuplicateDeclaration,
		typename CbExpectedTypeIsNotALambda,
		typename CbFileDoesNotExist,
		typename CbMultipleFunctionCandidates,
		typename CbNameNotFound,
		typename CbNoSuchFunction,
		typename CbParamShadowsPrevious,
		typename CbParseDiag,
		typename CbShouldNotHaveTypeParamsInIface,
		typename CbTypeConflict,
		typename CbTypeNotSendable,
		typename CbWrongNumberNewStructArgs,
		typename CbWrongNumberTypeArgsForSpec,
		typename CbWrongNumberTypeArgsForStruct
	> inline auto match(
		CbCantCallNonNoCtx cbCantCallNonNoCtx,
		CbCantCallSummon cbCantCallSummon,
		CbCantCallUnsafe cbCantCallUnsafe,
		CbCantCreateNonRecordStruct cbCantCreateNonRecordStruct,
		CbCantInferTypeArguments cbCantInferTypeArguments,
		CbCircularImport cbCircularImport,
		CbCommonTypesMissing cbCommonTypesMissing,
		CbDuplicateDeclaration cbDuplicateDeclaration,
		CbExpectedTypeIsNotALambda cbExpectedTypeIsNotALambda,
		CbFileDoesNotExist cbFileDoesNotExist,
		CbMultipleFunctionCandidates cbMultipleFunctionCandidates,
		CbNameNotFound cbNameNotFound,
		CbNoSuchFunction cbNoSuchFunction,
		CbParamShadowsPrevious cbParamShadowsPrevious,
		CbParseDiag cbParseDiag,
		CbShouldNotHaveTypeParamsInIface cbShouldNotHaveTypeParamsInIface,
		CbTypeConflict cbTypeConflict,
		CbTypeNotSendable cbTypeNotSendable,
		CbWrongNumberNewStructArgs cbWrongNumberNewStructArgs,
		CbWrongNumberTypeArgsForSpec cbWrongNumberTypeArgsForSpec,
		CbWrongNumberTypeArgsForStruct cbWrongNumberTypeArgsForStruct
	) const {
		switch (kind) {
			case Kind::cantCallNonNoCtx:
				return cbCantCallNonNoCtx(cantCallNonNoCtx);
			case Kind::cantCallSummon:
				return cbCantCallSummon(cantCallSummon);
			case Kind::cantCallUnsafe:
				return cbCantCallUnsafe(cantCallUnsafe);
			case Kind::cantCreateNonRecordStruct:
				return cbCantCreateNonRecordStruct(cantCreateNonRecordStruct);
			case Kind::cantInferTypeArguments:
				return cbCantInferTypeArguments(cantInferTypeArguments);
			case Kind::circularImport:
				return cbCircularImport(circularImport);
			case Kind::commonTypesMissing:
				return cbCommonTypesMissing(commonTypesMissing);
			case Kind::duplicateDeclaration:
				return cbDuplicateDeclaration(duplicateDeclaration);
			case Kind::expectedTypeIsNotALambda:
				return cbExpectedTypeIsNotALambda(expectedTypeIsNotALambda);
			case Kind::fileDoesNotExist:
				return cbFileDoesNotExist(fileDoesNotExist);
			case Kind::multipleFunctionCandidates:
				return cbMultipleFunctionCandidates(multipleFunctionCandidates);
			case Kind::nameNotFound:
				return cbNameNotFound(nameNotFound);
			case Kind::noSuchFunction:
				return cbNoSuchFunction(noSuchFunction);
			case Kind::paramShadowsPrevious:
				return cbParamShadowsPrevious(paramShadowsPrevious);
			case Kind::parseDiag:
				return cbParseDiag(parseDiag);
			case Kind::shouldNotHaveTypeParamsInIface:
				return cbShouldNotHaveTypeParamsInIface(shouldNotHaveTypeParamsInIface);
			case Kind::typeConflict:
				return cbTypeConflict(typeConflict);
			case Kind::typeNotSendable:
				return cbTypeNotSendable(typeNotSendable);
			case Kind::wrongNumberNewStructArgs:
				return cbWrongNumberNewStructArgs(wrongNumberNewStructArgs);
			case Kind::wrongNumberTypeArgsForSpec:
				return cbWrongNumberTypeArgsForSpec(wrongNumberTypeArgsForSpec);
			case Kind::wrongNumberTypeArgsForStruct:
				return cbWrongNumberTypeArgsForStruct(wrongNumberTypeArgsForStruct);
			default:
				assert(0);
		}
	}
};

struct Diagnostic {
	const PathAndStorageKind where;
	const SourceRange range;
	const Diag diag;
};

struct Diagnostics {
	const Arr<const Diagnostic> diagnostics;
	const LineAndColumnGetters lineAndColumnGetters;

	inline Diagnostics(const Arr<const Diagnostic> _diagnostics, const LineAndColumnGetters _lineAndColumnGetters)
		: diagnostics{_diagnostics}, lineAndColumnGetters{_lineAndColumnGetters} {
		assert(!isEmpty(diagnostics));
	}
};
