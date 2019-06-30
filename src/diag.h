#pragma once

#include "./model.h"

struct ParseDiag {
	struct ExpectedCharacter {
		const char ch;
	};
	struct ExpectedDedent {};
	struct ExpectedIndent {};
	struct ExpectedPurityAfterSpace {};
	struct LeadingSpace {};
	struct LetMustHaveThen {};
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
		expectedDedent,
		expectedIndent,
		expectedPurityAfterSpace,
		leadingSpace,
		letMustHaveThen,
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
		const ExpectedDedent expectedDedent;
		const ExpectedIndent expectedIndent;
		const ExpectedPurityAfterSpace expectedPurityAfterSpace;
		const LeadingSpace leadingSpace;
		const LetMustHaveThen letMustHaveThen;
		const MatchWhenNewMayNotAppearInsideArg matchWhenNewMayNotAppearInsideArg;
		const MustEndInBlankLine mustEndInBlankLine;
		const TrailingSpace trailingSpace;
		const TypeParamCantHaveTypeArgs typeParamCantHaveTypeArgs;
		const UnexpectedCharacter unexpectedCharacter;
		const UnionCantBeEmpty unionCantBeEmpty;
		const WhenMustHaveElse whenMustHaveElse;
	};

public:
	explicit inline ParseDiag(const ExpectedCharacter d) : kind{Kind::expectedCharacter}, expectedCharacter{d} {}
	explicit inline ParseDiag(const ExpectedDedent d) : kind{Kind::expectedDedent}, expectedDedent{d} {}
	explicit inline ParseDiag(const ExpectedIndent d) : kind{Kind::expectedIndent}, expectedIndent{d} {}
	explicit inline ParseDiag(const ExpectedPurityAfterSpace d) : kind{Kind::expectedPurityAfterSpace}, expectedPurityAfterSpace{d} {}
	explicit inline ParseDiag(const LeadingSpace d) : kind{Kind::leadingSpace}, leadingSpace{d} {}
	explicit inline ParseDiag(const LetMustHaveThen d) : kind{Kind::letMustHaveThen}, letMustHaveThen{d} {}
	explicit inline ParseDiag(const MatchWhenNewMayNotAppearInsideArg d) : kind{Kind::matchWhenNewMayNotAppearInsideArg}, matchWhenNewMayNotAppearInsideArg{d} {}
	explicit inline ParseDiag(const MustEndInBlankLine d) : kind{Kind::mustEndInBlankLine}, mustEndInBlankLine{d} {}
	explicit inline ParseDiag(const TrailingSpace d) : kind{Kind::trailingSpace}, trailingSpace{d} {}
	explicit inline ParseDiag(const TypeParamCantHaveTypeArgs d) : kind{Kind::typeParamCantHaveTypeArgs}, typeParamCantHaveTypeArgs{d} {}
	explicit inline ParseDiag(const UnexpectedCharacter d) : kind{Kind::unexpectedCharacter}, unexpectedCharacter{d} {}
	explicit inline ParseDiag(const UnionCantBeEmpty d) : kind{Kind::unionCantBeEmpty}, unionCantBeEmpty{d} {}
	explicit inline ParseDiag(const WhenMustHaveElse d) : kind{Kind::whenMustHaveElse}, whenMustHaveElse{d} {}

	template <
		typename CbExpectedCharacter,
		typename CbExpectedDedent,
		typename CbExpectedIndent,
		typename CbExpectedPurityAfterSpace,
		typename CbLeadingSpace,
		typename CbMatchWhenNewMayNotAppearInsideArg,
		typename CbMustEndInBlankLine,
		typename CbLetMustHaveThen,
		typename CbTrailingSpace,
		typename CbTypeParamCantHaveTypeArgs,
		typename CbUnexpectedCharacter,
		typename CbUnionCantBeEmpty,
		typename CbWhenMustHaveElse
	> inline auto match(
		CbExpectedCharacter cbExpectedCharacter,
		CbExpectedDedent cbExpectedDedent,
		CbExpectedIndent cbExpectedIndent,
		CbExpectedPurityAfterSpace cbExpectedPurityAfterSpace,
		CbLeadingSpace cbLeadingSpace,
		CbLetMustHaveThen cbLetMustHaveThen,
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
			case Kind::expectedDedent:
				return cbExpectedDedent(expectedDedent);
			case Kind::expectedIndent:
				return cbExpectedIndent(expectedIndent);
			case Kind::expectedPurityAfterSpace:
				return cbExpectedPurityAfterSpace(expectedPurityAfterSpace);
			case Kind::leadingSpace:
				return cbLeadingSpace(leadingSpace);
			case Kind::letMustHaveThen:
				return cbLetMustHaveThen(letMustHaveThen);
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
			field,
			unionMember,
		};
		const Kind kind;
		const Str name;
	};
	struct ExpectedTypeIsNotALambda {
		const Opt<const Type> expectedType;
	};
	struct FieldPurityWorseThanStructPurity {
		const StructDecl* strukt;
		const Type fieldType;
	};
	struct FileDoesNotExist {};
	struct MatchCaseStructNamesDoNotMatch {
		const Arr<const StructInst*> unionMembers;
	};
	struct MatchOnNonUnion {
		const Type type;
	};
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
	struct RemoteFunDoesNotReturnFut {
		const Type actualReturnType;
	};
	struct SpecImplNotFound {
		const Str sigName;
	};
	struct SpecImplHasSpecs {
		const Str funName;
	};
	struct TypeConflict {
		const Type expected;
		const Type actual;
	};
	struct TypeNotSendable {};
	struct WriteToNonMutableField {
		const StructField* field;
	};
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
		fieldPurityWorseThanStructPurity,
		fileDoesNotExist,
		matchCaseStructNamesDoNotMatch,
		matchOnNonUnion,
		multipleFunctionCandidates,
		nameNotFound,
		noSuchFunction,
		paramShadowsPrevious,
		parseDiag,
		remoteFunDoesNotReturnFut,
		specImplHasSpecs,
		specImplNotFound,
		typeConflict,
		typeNotSendable,
		writeToNonMutableField,
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
		const FieldPurityWorseThanStructPurity fieldPurityWorseThanStructPurity;
		const FileDoesNotExist fileDoesNotExist;
		const MatchCaseStructNamesDoNotMatch matchCaseStructNamesDoNotMatch;
		const MatchOnNonUnion matchOnNonUnion;
		const MultipleFunctionCandidates multipleFunctionCandidates;
		const NameNotFound nameNotFound;
		const NoSuchFunction noSuchFunction;
		const ParamShadowsPrevious paramShadowsPrevious;
		const ParseDiag parseDiag;
		const RemoteFunDoesNotReturnFut remoteFunDoesNotReturnFut;
		const SpecImplHasSpecs specImplHasSpecs;
		const SpecImplNotFound specImplNotFound;
		const TypeConflict typeConflict;
		const TypeNotSendable typeNotSendable;
		const WriteToNonMutableField writeToNonMutableField;
		const WrongNumberNewStructArgs wrongNumberNewStructArgs;
		const WrongNumberTypeArgsForSpec wrongNumberTypeArgsForSpec;
		const WrongNumberTypeArgsForStruct wrongNumberTypeArgsForStruct;
	};

public:
	explicit inline Diag(const CantCallNonNoCtx d) : kind{Kind::cantCallNonNoCtx}, cantCallNonNoCtx{d} {}
	explicit inline Diag(const CantCallSummon d) : kind{Kind::cantCallSummon}, cantCallSummon{d} {}
	explicit inline Diag(const CantCallUnsafe d) : kind{Kind::cantCallUnsafe}, cantCallUnsafe{d} {}
	explicit inline Diag(const CantCreateNonRecordStruct d) : kind{Kind::cantCreateNonRecordStruct}, cantCreateNonRecordStruct{d} {}
	explicit inline Diag(const CantInferTypeArguments d) : kind{Kind::cantInferTypeArguments}, cantInferTypeArguments{d} {}
	explicit inline Diag(const CircularImport d) : kind{Kind::circularImport}, circularImport{d} {}
	explicit inline Diag(const CommonTypesMissing d) : kind{Kind::commonTypesMissing}, commonTypesMissing{d} {}
	explicit inline Diag(const DuplicateDeclaration d) : kind{Kind::duplicateDeclaration}, duplicateDeclaration{d} {}
	explicit inline Diag(const ExpectedTypeIsNotALambda d) : kind{Kind::expectedTypeIsNotALambda}, expectedTypeIsNotALambda{d} {}
	explicit inline Diag(const FieldPurityWorseThanStructPurity d) : kind{Kind::fieldPurityWorseThanStructPurity}, fieldPurityWorseThanStructPurity{d} {}
	explicit inline Diag(const FileDoesNotExist d) : kind{Kind::fileDoesNotExist}, fileDoesNotExist{d} {}
	explicit inline Diag(const MatchCaseStructNamesDoNotMatch d) : kind{Kind::matchCaseStructNamesDoNotMatch}, matchCaseStructNamesDoNotMatch{d} {}
	explicit inline Diag(const MatchOnNonUnion d) : kind{Kind::matchOnNonUnion}, matchOnNonUnion{d} {}
	explicit inline Diag(const MultipleFunctionCandidates d) : kind{Kind::multipleFunctionCandidates}, multipleFunctionCandidates{d} {}
	explicit inline Diag(const NameNotFound d) : kind{Kind::nameNotFound}, nameNotFound{d} {}
	explicit inline Diag(const NoSuchFunction d) : kind{Kind::noSuchFunction}, noSuchFunction{d} {}
	explicit inline Diag(const ParamShadowsPrevious d) : kind{Kind::paramShadowsPrevious}, paramShadowsPrevious{d} {}
	explicit inline Diag(const ParseDiag d) : kind{Kind::parseDiag}, parseDiag{d} {}
	explicit inline Diag(const RemoteFunDoesNotReturnFut d) : kind{Kind::remoteFunDoesNotReturnFut}, remoteFunDoesNotReturnFut{d} {}
	explicit inline Diag(const SpecImplHasSpecs d) : kind{Kind::specImplHasSpecs}, specImplHasSpecs{d} {}
	explicit inline Diag(const SpecImplNotFound d) : kind{Kind::specImplNotFound}, specImplNotFound{d} {}
	explicit inline Diag(const TypeConflict d) : kind{Kind::typeConflict}, typeConflict{d} {}
	explicit inline Diag(const TypeNotSendable d) : kind{Kind::typeNotSendable}, typeNotSendable{d} {}
	explicit inline Diag(const WriteToNonMutableField d) : kind{Kind::writeToNonMutableField}, writeToNonMutableField{d} {}
	explicit inline Diag(const WrongNumberNewStructArgs d) : kind{Kind::wrongNumberNewStructArgs}, wrongNumberNewStructArgs{d} {}
	explicit inline Diag(const WrongNumberTypeArgsForSpec d) : kind{Kind::wrongNumberTypeArgsForSpec}, wrongNumberTypeArgsForSpec{d} {}
	explicit inline Diag(const WrongNumberTypeArgsForStruct d) : kind{Kind::wrongNumberTypeArgsForStruct}, wrongNumberTypeArgsForStruct{d} {}

	inline const Bool isFileDoesNotExist() const {
		return enumEq(kind, Kind::fileDoesNotExist);
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
		typename CbFieldPurityWorseThanStructPurity,
		typename CbFileDoesNotExist,
		typename CbMatchCaseStructNamesDoNotMatch,
		typename CbMatchOnNonUnion,
		typename CbMultipleFunctionCandidates,
		typename CbNameNotFound,
		typename CbNoSuchFunction,
		typename CbParamShadowsPrevious,
		typename CbParseDiag,
		typename CbRemoteFunDoesNotReturnFut,
		typename CbSpecImplHasSpecs,
		typename CbSpecImplNotFound,
		typename CbTypeConflict,
		typename CbTypeNotSendable,
		typename CbWriteToNonMutableField,
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
		CbFieldPurityWorseThanStructPurity cbFieldPurityWorseThanStructPurity,
		CbFileDoesNotExist cbFileDoesNotExist,
		CbMatchCaseStructNamesDoNotMatch cbMatchCaseStructNamesDoNotMatch,
		CbMatchOnNonUnion cbMatchOnNonUnion,
		CbMultipleFunctionCandidates cbMultipleFunctionCandidates,
		CbNameNotFound cbNameNotFound,
		CbNoSuchFunction cbNoSuchFunction,
		CbParamShadowsPrevious cbParamShadowsPrevious,
		CbParseDiag cbParseDiag,
		CbRemoteFunDoesNotReturnFut cbRemoteFunDoesNotReturnFut,
		CbSpecImplHasSpecs cbSpecImplHasSpecs,
		CbSpecImplNotFound cbSpecImplNotFound,
		CbTypeConflict cbTypeConflict,
		CbTypeNotSendable cbTypeNotSendable,
		CbWriteToNonMutableField cbWriteToNonMutableField,
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
			case Kind::fieldPurityWorseThanStructPurity:
				return cbFieldPurityWorseThanStructPurity(fieldPurityWorseThanStructPurity);
			case Kind::fileDoesNotExist:
				return cbFileDoesNotExist(fileDoesNotExist);
			case Kind::matchCaseStructNamesDoNotMatch:
				return cbMatchCaseStructNamesDoNotMatch(matchCaseStructNamesDoNotMatch);
			case Kind::matchOnNonUnion:
				return cbMatchOnNonUnion(matchOnNonUnion);
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
			case Kind::remoteFunDoesNotReturnFut:
				return cbRemoteFunDoesNotReturnFut(remoteFunDoesNotReturnFut);
			case Kind::specImplHasSpecs:
				return cbSpecImplHasSpecs(specImplHasSpecs);
			case Kind::specImplNotFound:
				return cbSpecImplNotFound(specImplNotFound);
			case Kind::typeConflict:
				return cbTypeConflict(typeConflict);
			case Kind::typeNotSendable:
				return cbTypeNotSendable(typeNotSendable);
			case Kind::writeToNonMutableField:
				return cbWriteToNonMutableField(writeToNonMutableField);
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
