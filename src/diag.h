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
	explicit inline ParseDiag(const MatchWhenNewMayNotAppearInsideArg d)
		: kind{Kind::matchWhenNewMayNotAppearInsideArg}, matchWhenNewMayNotAppearInsideArg{d} {}
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
	// Note: this error is issued *before* resolving specs.
	// We don't exclude a candidate based on not having specs.
	struct CallMultipleMatches {
		const Sym funName;
		// Unlike CallNoMatch, these are only the ones that match
		const Arr<const CalledDecl> matches;
	};
	struct CallNoMatch {
		const Sym funName;
		const Opt<const Type> expectedReturnType;
		const size_t actualArity;
		// NOTE: we may have given up early and this may not be as much as actualArity
		const Arr<const Type> actualArgTypes;
		// All candidates, including those with wrong arity
		const Arr<const CalledDecl> allCandidates;
	};

	struct CantCallNonNoCtx {};
	struct CantCallSummon {};
	struct CantCallUnsafe {};
	struct CantCreateNonRecordStruct {
		const StructDecl* strukt;
	};
	struct CantInferTypeArguments {};
	struct CircularImport {};
	struct CommonTypesMissing {};
	struct CreateRecordByRefNoCtx {
		const StructDecl* strukt;
	};
	struct DuplicateDeclaration {
		enum class Kind {
			structOrAlias,
			spec,
			field,
			unionMember,
		};
		const Kind kind;
		const Sym name;
	};
	struct ExpectedTypeIsNotALambda {
		const Opt<const Type> expectedType;
	};
	struct FileDoesNotExist {};
	struct MatchCaseStructNamesDoNotMatch {
		const Arr<const StructInst*> unionMembers;
	};
	struct MatchOnNonUnion {
		const Type type;
	};
	struct MutFieldInNonMutRecord {};
	struct NameNotFound {
		enum class Kind {
			strukt,
			spec,
			iface,
			typeParam,
		};

		const Sym name;
		const Kind kind;
	};
	struct ParamShadowsPrevious {
		enum class Kind {
			param,
			typeParam,
		};
		const Kind kind;
	};
	struct PurityOfFieldWorseThanRecord {
		const StructDecl* strukt;
		const Type fieldType;
	};
	struct PurityOfMemberWorseThanUnion {
		const StructDecl* strukt;
		const StructInst* member;
	};
	struct SendFunDoesNotReturnFut {
		const Type actualReturnType;
	};
	struct SpecImplNotFound {
		const Sym sigName;
	};
	struct SpecImplHasSpecs {
		const Sym funName;
	};
	struct TypeConflict {
		const Type expected;
		const Type actual;
	};
	struct TypeNotSendable {};
	struct WriteToNonExistentField {
		// Type of `x` in `x.y := z`
		const Type targetType;
		// `y` in `x.y := z`
		const Sym fieldName;
	};
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
		callMultipleMatches,
		callNoMatch,
		cantCallNonNoCtx,
		cantCallSummon,
		cantCallUnsafe,
		cantCreateNonRecordStruct,
		cantInferTypeArguments,
		circularImport,
		commonTypesMissing,
		createRecordByRefNoCtx,
		duplicateDeclaration,
		expectedTypeIsNotALambda,
		fileDoesNotExist,
		matchCaseStructNamesDoNotMatch,
		matchOnNonUnion,
		mutFieldInNonMutRecord,
		nameNotFound,
		paramShadowsPrevious,
		parseDiag,
		purityOfFieldWorseThanRecord,
		purityOfMemberWorseThanUnion,
		sendFunDoesNotReturnFut,
		specImplHasSpecs,
		specImplNotFound,
		typeConflict,
		typeNotSendable,
		writeToNonExistentField,
		writeToNonMutableField,
		wrongNumberNewStructArgs,
		wrongNumberTypeArgsForSpec,
		wrongNumberTypeArgsForStruct,
	};
	const Kind kind;
	union {
		const CallMultipleMatches callMultipleMatches;
		const CallNoMatch callNoMatch;
		const CantCallNonNoCtx cantCallNonNoCtx;
		const CantCallSummon cantCallSummon;
		const CantCallUnsafe cantCallUnsafe;
		const CantCreateNonRecordStruct cantCreateNonRecordStruct;
		const CantInferTypeArguments cantInferTypeArguments;
		const CircularImport circularImport;
		const CommonTypesMissing commonTypesMissing;
		const CreateRecordByRefNoCtx createRecordByRefNoCtx;
		const DuplicateDeclaration duplicateDeclaration;
		const ExpectedTypeIsNotALambda expectedTypeIsNotALambda;
		const FileDoesNotExist fileDoesNotExist;
		const MatchCaseStructNamesDoNotMatch matchCaseStructNamesDoNotMatch;
		const MatchOnNonUnion matchOnNonUnion;
		const MutFieldInNonMutRecord mutFieldInNonMutRecord;
		const NameNotFound nameNotFound;
		const ParamShadowsPrevious paramShadowsPrevious;
		const ParseDiag parseDiag;
		const PurityOfFieldWorseThanRecord purityOfFieldWorseThanRecord;
		const PurityOfMemberWorseThanUnion purityOfMemberWorseThanUnion;
		const SendFunDoesNotReturnFut sendFunDoesNotReturnFut;
		const SpecImplHasSpecs specImplHasSpecs;
		const SpecImplNotFound specImplNotFound;
		const TypeConflict typeConflict;
		const TypeNotSendable typeNotSendable;
		const WriteToNonExistentField writeToNonExistentField;
		const WriteToNonMutableField writeToNonMutableField;
		const WrongNumberNewStructArgs wrongNumberNewStructArgs;
		const WrongNumberTypeArgsForSpec wrongNumberTypeArgsForSpec;
		const WrongNumberTypeArgsForStruct wrongNumberTypeArgsForStruct;
	};

public:
	explicit inline Diag(const CallMultipleMatches d) : kind{Kind::callMultipleMatches}, callMultipleMatches{d} {}
	explicit inline Diag(const CallNoMatch d) : kind{Kind::callNoMatch}, callNoMatch{d} {}
	explicit inline Diag(const CantCallNonNoCtx d) : kind{Kind::cantCallNonNoCtx}, cantCallNonNoCtx{d} {}
	explicit inline Diag(const CantCallSummon d) : kind{Kind::cantCallSummon}, cantCallSummon{d} {}
	explicit inline Diag(const CantCallUnsafe d) : kind{Kind::cantCallUnsafe}, cantCallUnsafe{d} {}
	explicit inline Diag(const CantCreateNonRecordStruct d) : kind{Kind::cantCreateNonRecordStruct}, cantCreateNonRecordStruct{d} {}
	explicit inline Diag(const CantInferTypeArguments d) : kind{Kind::cantInferTypeArguments}, cantInferTypeArguments{d} {}
	explicit inline Diag(const CircularImport d) : kind{Kind::circularImport}, circularImport{d} {}
	explicit inline Diag(const CommonTypesMissing d) : kind{Kind::commonTypesMissing}, commonTypesMissing{d} {}
	explicit inline Diag(const CreateRecordByRefNoCtx d) : kind{Kind::createRecordByRefNoCtx}, createRecordByRefNoCtx{d} {}
	explicit inline Diag(const DuplicateDeclaration d) : kind{Kind::duplicateDeclaration}, duplicateDeclaration{d} {}
	explicit inline Diag(const ExpectedTypeIsNotALambda d) : kind{Kind::expectedTypeIsNotALambda}, expectedTypeIsNotALambda{d} {}
	explicit inline Diag(const FileDoesNotExist d) : kind{Kind::fileDoesNotExist}, fileDoesNotExist{d} {}
	explicit inline Diag(const MatchCaseStructNamesDoNotMatch d) : kind{Kind::matchCaseStructNamesDoNotMatch}, matchCaseStructNamesDoNotMatch{d} {}
	explicit inline Diag(const MatchOnNonUnion d) : kind{Kind::matchOnNonUnion}, matchOnNonUnion{d} {}
	explicit inline Diag(const MutFieldInNonMutRecord d) : kind{Kind::mutFieldInNonMutRecord}, mutFieldInNonMutRecord{d} {}
	explicit inline Diag(const NameNotFound d) : kind{Kind::nameNotFound}, nameNotFound{d} {}
	explicit inline Diag(const ParamShadowsPrevious d) : kind{Kind::paramShadowsPrevious}, paramShadowsPrevious{d} {}
	explicit inline Diag(const ParseDiag d) : kind{Kind::parseDiag}, parseDiag{d} {}
	explicit inline Diag(const PurityOfFieldWorseThanRecord d) : kind{Kind::purityOfFieldWorseThanRecord}, purityOfFieldWorseThanRecord{d} {}
	explicit inline Diag(const PurityOfMemberWorseThanUnion d) : kind{Kind::purityOfMemberWorseThanUnion}, purityOfMemberWorseThanUnion{d} {}
	explicit inline Diag(const SendFunDoesNotReturnFut d) : kind{Kind::sendFunDoesNotReturnFut}, sendFunDoesNotReturnFut{d} {}
	explicit inline Diag(const SpecImplHasSpecs d) : kind{Kind::specImplHasSpecs}, specImplHasSpecs{d} {}
	explicit inline Diag(const SpecImplNotFound d) : kind{Kind::specImplNotFound}, specImplNotFound{d} {}
	explicit inline Diag(const TypeConflict d) : kind{Kind::typeConflict}, typeConflict{d} {}
	explicit inline Diag(const TypeNotSendable d) : kind{Kind::typeNotSendable}, typeNotSendable{d} {}
	explicit inline Diag(const WriteToNonExistentField d) : kind{Kind::writeToNonExistentField}, writeToNonExistentField{d} {}
	explicit inline Diag(const WriteToNonMutableField d) : kind{Kind::writeToNonMutableField}, writeToNonMutableField{d} {}
	explicit inline Diag(const WrongNumberNewStructArgs d) : kind{Kind::wrongNumberNewStructArgs}, wrongNumberNewStructArgs{d} {}
	explicit inline Diag(const WrongNumberTypeArgsForSpec d) : kind{Kind::wrongNumberTypeArgsForSpec}, wrongNumberTypeArgsForSpec{d} {}
	explicit inline Diag(const WrongNumberTypeArgsForStruct d) : kind{Kind::wrongNumberTypeArgsForStruct}, wrongNumberTypeArgsForStruct{d} {}

	inline const Bool isFileDoesNotExist() const {
		return enumEq(kind, Kind::fileDoesNotExist);
	}

	template <
		typename CbCallMultipleMatches,
		typename CbCallNoMatch,
		typename CbCantCallNonNoCtx,
		typename CbCantCallSummon,
		typename CbCantCallUnsafe,
		typename CbCantCreateNonRecordStruct,
		typename CbCantInferTypeArguments,
		typename CbCircularImport,
		typename CbCommonTypesMissing,
		typename CbCreateRecordByRefNoCtx,
		typename CbDuplicateDeclaration,
		typename CbExpectedTypeIsNotALambda,
		typename CbFileDoesNotExist,
		typename CbMatchCaseStructNamesDoNotMatch,
		typename CbMatchOnNonUnion,
		typename CbMutFieldInNonMutRecord,
		typename CbNameNotFound,
		typename CbParamShadowsPrevious,
		typename CbParseDiag,
		typename CbPurityOfFieldWorseThanRecord,
		typename CbPurityOfMemberWorseThanUnion,
		typename CbSendFunDoesNotReturnFut,
		typename CbSpecImplHasSpecs,
		typename CbSpecImplNotFound,
		typename CbTypeConflict,
		typename CbTypeNotSendable,
		typename CbWriteToNonExistentField,
		typename CbWriteToNonMutableField,
		typename CbWrongNumberNewStructArgs,
		typename CbWrongNumberTypeArgsForSpec,
		typename CbWrongNumberTypeArgsForStruct
	> inline auto match(
		CbCallMultipleMatches cbCallMultipleMatches,
		CbCallNoMatch cbCallNoMatch,
		CbCantCallNonNoCtx cbCantCallNonNoCtx,
		CbCantCallSummon cbCantCallSummon,
		CbCantCallUnsafe cbCantCallUnsafe,
		CbCantCreateNonRecordStruct cbCantCreateNonRecordStruct,
		CbCantInferTypeArguments cbCantInferTypeArguments,
		CbCircularImport cbCircularImport,
		CbCommonTypesMissing cbCommonTypesMissing,
		CbCreateRecordByRefNoCtx cbCreateRecordByRefNoCtx,
		CbDuplicateDeclaration cbDuplicateDeclaration,
		CbExpectedTypeIsNotALambda cbExpectedTypeIsNotALambda,
		CbFileDoesNotExist cbFileDoesNotExist,
		CbMatchCaseStructNamesDoNotMatch cbMatchCaseStructNamesDoNotMatch,
		CbMatchOnNonUnion cbMatchOnNonUnion,
		CbMutFieldInNonMutRecord cbMutFieldInNonMutRecord,
		CbNameNotFound cbNameNotFound,
		CbParamShadowsPrevious cbParamShadowsPrevious,
		CbParseDiag cbParseDiag,
		CbPurityOfFieldWorseThanRecord cbPurityOfFieldWorseThanRecord,
		CbPurityOfMemberWorseThanUnion cbPurityOfMemberWorseThanUnion,
		CbSendFunDoesNotReturnFut cbSendFunDoesNotReturnFut,
		CbSpecImplHasSpecs cbSpecImplHasSpecs,
		CbSpecImplNotFound cbSpecImplNotFound,
		CbTypeConflict cbTypeConflict,
		CbTypeNotSendable cbTypeNotSendable,
		CbWriteToNonExistentField cbWriteToNonExistentField,
		CbWriteToNonMutableField cbWriteToNonMutableField,
		CbWrongNumberNewStructArgs cbWrongNumberNewStructArgs,
		CbWrongNumberTypeArgsForSpec cbWrongNumberTypeArgsForSpec,
		CbWrongNumberTypeArgsForStruct cbWrongNumberTypeArgsForStruct
	) const {
		switch (kind) {
			case Kind::callMultipleMatches:
				return cbCallMultipleMatches(callMultipleMatches);
			case Kind::callNoMatch:
				return cbCallNoMatch(callNoMatch);
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
			case Kind::createRecordByRefNoCtx:
				return cbCreateRecordByRefNoCtx(createRecordByRefNoCtx);
			case Kind::duplicateDeclaration:
				return cbDuplicateDeclaration(duplicateDeclaration);
			case Kind::expectedTypeIsNotALambda:
				return cbExpectedTypeIsNotALambda(expectedTypeIsNotALambda);
			case Kind::fileDoesNotExist:
				return cbFileDoesNotExist(fileDoesNotExist);
			case Kind::matchCaseStructNamesDoNotMatch:
				return cbMatchCaseStructNamesDoNotMatch(matchCaseStructNamesDoNotMatch);
			case Kind::matchOnNonUnion:
				return cbMatchOnNonUnion(matchOnNonUnion);
			case Kind::mutFieldInNonMutRecord:
				return cbMutFieldInNonMutRecord(mutFieldInNonMutRecord);
			case Kind::nameNotFound:
				return cbNameNotFound(nameNotFound);
			case Kind::paramShadowsPrevious:
				return cbParamShadowsPrevious(paramShadowsPrevious);
			case Kind::parseDiag:
				return cbParseDiag(parseDiag);
			case Kind::purityOfFieldWorseThanRecord:
				return cbPurityOfFieldWorseThanRecord(purityOfFieldWorseThanRecord);
			case Kind::purityOfMemberWorseThanUnion:
				return cbPurityOfMemberWorseThanUnion(purityOfMemberWorseThanUnion);
			case Kind::sendFunDoesNotReturnFut:
				return cbSendFunDoesNotReturnFut(sendFunDoesNotReturnFut);
			case Kind::specImplHasSpecs:
				return cbSpecImplHasSpecs(specImplHasSpecs);
			case Kind::specImplNotFound:
				return cbSpecImplNotFound(specImplNotFound);
			case Kind::typeConflict:
				return cbTypeConflict(typeConflict);
			case Kind::typeNotSendable:
				return cbTypeNotSendable(typeNotSendable);
			case Kind::writeToNonExistentField:
				return cbWriteToNonExistentField(writeToNonExistentField);
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

struct FilesInfo {
	const AbsolutePathsGetter absolutePathsGetter;
	const LineAndColumnGetters lineAndColumnGetters;
};

struct Diagnostics {
	const Arr<const Diagnostic> diagnostics;
	const FilesInfo filesInfo;

	inline Diagnostics(const Arr<const Diagnostic> _diagnostics, const FilesInfo _filesInfo)
		: diagnostics{_diagnostics}, filesInfo{_filesInfo} {
		assert(!isEmpty(diagnostics));
	}
};
