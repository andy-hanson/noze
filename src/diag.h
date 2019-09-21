#pragma once

#include "./model.h"
#include "./parseDiag.h"

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
		// 0 for inferred type args
		const size_t actualNTypeArgs;
		const size_t actualArity;
		// NOTE: we may have given up early and this may not be as much as actualArity
		const Arr<const Type> actualArgTypes;
		// All candidates, including those with wrong arity
		const Arr<const CalledDecl> allCandidates;
	};

	struct CantCall {
		enum class Reason {
			nonNoCtx,
			summon,
			unsafe
		};

		const Reason reason;
		const FunDecl* callee;
		const FunDecl* caller;
	};

	struct CantCreateNonRecordType {
		const Type type;
	};
	struct CantCreateRecordWithoutExpectedType {};
	struct CantInferTypeArguments {};
	struct CircularImport {
		const PathAndStorageKind from;
		const PathAndStorageKind to;
	};
	struct CommonTypesMissing {};
	struct CreateArrNoExpectedType {};
	struct CreateRecordByRefNoCtx {
		const StructDecl* strukt;
	};
	struct CreateRecordMultiLineWrongFields {
		const StructDecl* decl;
		const Arr<const RecordField> fields;
		const Arr<const Sym> providedFieldNames;
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
	struct DuplicateImports {
		enum class Kind {
			structOrAlias,
			spec,
		};
		const Kind kind;
		const Sym name;
	};
	struct ExpectedTypeIsNotALambda {
		const Opt<const Type> expectedType;
	};
	struct FileDoesNotExist {
		enum class Kind {
			root,
			import
		};
		const Kind kind;
		const PathAndStorageKind path;
	};
	struct FileShouldEndInNz {};
	struct FunAsLambdaCantOverload {
		const Sym funName;
	};
	struct FunAsLambdaWrongNumberTypeArgs {
		const FunDecl* fun;
		const size_t nProvidedTypeArgs;
	};
	struct FunAsLambdaWrongReturnType {
		const Type actual;
		const Type expected;
	};
	struct LambdaCantInferParamTypes {};
	struct LambdaClosesOverMut {
		const ClosureField* field;
	};
	struct LambdaForFunPtrHasClosure {
		const ClosureField* field;
	};
	struct LocalShadowsPrevious {
		const Sym name;
	};
	struct MatchCaseStructNamesDoNotMatch {
		const Arr<const StructInst*> unionMembers;
	};
	struct MatchOnNonUnion {
		const Type type;
	};
	struct MutFieldNotAllowed {
		enum Reason {
			recordIsNotMut,
			recordIsForcedByVal
		};

		const Reason reason;
	};
	struct NameNotFound {
		enum class Kind {
			strukt,
			spec,
			typeParam,
		};
		const Kind kind;
		const Sym name;
	};
	struct ParamShadowsPrevious {
		enum class Kind {
			param,
			typeParam,
		};
		const Kind kind;
		const Sym name;
	};
	struct PurityOfFieldWorseThanRecord {
		const StructDecl* strukt;
		const Type fieldType;
	};
	struct PurityOfMemberWorseThanUnion {
		const StructDecl* strukt;
		const StructInst* member;
	};
	struct RelativeImportReachesPastRoot {
		const RelPath imported;
	};
	struct SendFunDoesNotReturnFut {
		const Type actualReturnType;
	};
	struct SpecBuiltinNotSatisfied {
		const SpecBody::Builtin::Kind kind;
		const Type type;
		const FunDecl* called;
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
		const RecordField* field;
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
		cantCall,
		cantCreateNonRecordType,
		cantCreateRecordWithoutExpectedType,
		cantInferTypeArguments,
		circularImport,
		commonTypesMissing,
		createArrNoExpectedType,
		createRecordByRefNoCtx,
		createRecordMultiLineWrongFields,
		duplicateDeclaration,
		duplicateImports,
		expectedTypeIsNotALambda,
		fileDoesNotExist,
		fileShouldEndInNz,
		funAsLambdaCantOverload,
		funAsLambdaWrongNumberTypeArgs,
		funAsLambdaWrongReturnType,
		lambdaCantInferParamTypes,
		lambdaClosesOverMut,
		lambdaForFunPtrHasClosure,
		localShadowsPrevious,
		matchCaseStructNamesDoNotMatch,
		matchOnNonUnion,
		mutFieldNotAllowed,
		nameNotFound,
		paramShadowsPrevious,
		parseDiag,
		purityOfFieldWorseThanRecord,
		purityOfMemberWorseThanUnion,
		relativeImportReachesPastRoot,
		sendFunDoesNotReturnFut,
		specBuiltinNotSatisfied,
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
		const CantCall cantCall;
		const CantCreateNonRecordType cantCreateNonRecordType;
		const CantCreateRecordWithoutExpectedType cantCreateRecordWithoutExpectedType;
		const CantInferTypeArguments cantInferTypeArguments;
		const CircularImport circularImport;
		const CommonTypesMissing commonTypesMissing;
		const CreateArrNoExpectedType createArrNoExpectedType;
		const CreateRecordByRefNoCtx createRecordByRefNoCtx;
		const CreateRecordMultiLineWrongFields createRecordMultiLineWrongFields;
		const DuplicateDeclaration duplicateDeclaration;
		const DuplicateImports duplicateImports;
		const ExpectedTypeIsNotALambda expectedTypeIsNotALambda;
		const FileDoesNotExist fileDoesNotExist;
		const FileShouldEndInNz fileShouldEndInNz;
		const FunAsLambdaCantOverload funAsLambdaCantOverload;
		const FunAsLambdaWrongNumberTypeArgs funAsLambdaWrongNumberTypeArgs;
		const FunAsLambdaWrongReturnType funAsLambdaWrongReturnType;
		const LambdaCantInferParamTypes lambdaCantInferParamTypes;
		const LambdaClosesOverMut lambdaClosesOverMut;
		const LambdaForFunPtrHasClosure lambdaForFunPtrHasClosure;
		const LocalShadowsPrevious localShadowsPrevious;
		const MatchCaseStructNamesDoNotMatch matchCaseStructNamesDoNotMatch;
		const MatchOnNonUnion matchOnNonUnion;
		const MutFieldNotAllowed mutFieldNotAllowed;
		const NameNotFound nameNotFound;
		const ParamShadowsPrevious paramShadowsPrevious;
		const ParseDiag parseDiag;
		const PurityOfFieldWorseThanRecord purityOfFieldWorseThanRecord;
		const PurityOfMemberWorseThanUnion purityOfMemberWorseThanUnion;
		const RelativeImportReachesPastRoot relativeImportReachesPastRoot;
		const SendFunDoesNotReturnFut sendFunDoesNotReturnFut;
		const SpecBuiltinNotSatisfied specBuiltinNotSatisfied;
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
	explicit inline Diag(const CallMultipleMatches d)
		: kind{Kind::callMultipleMatches}, callMultipleMatches{d} {}
	explicit inline Diag(const CallNoMatch d)
		: kind{Kind::callNoMatch}, callNoMatch{d} {}
	explicit inline Diag(const CantCall d)
		: kind{Kind::cantCall}, cantCall{d} {}
	explicit inline Diag(const CantCreateNonRecordType d)
		: kind{Kind::cantCreateNonRecordType}, cantCreateNonRecordType{d} {}
	explicit inline Diag(const CantCreateRecordWithoutExpectedType d)
		: kind{Kind::cantCreateRecordWithoutExpectedType}, cantCreateRecordWithoutExpectedType{d} {}
	explicit inline Diag(const CantInferTypeArguments d)
		: kind{Kind::cantInferTypeArguments}, cantInferTypeArguments{d} {}
	explicit inline Diag(const CircularImport d)
		: kind{Kind::circularImport}, circularImport{d} {}
	explicit inline Diag(const CommonTypesMissing d)
		: kind{Kind::commonTypesMissing}, commonTypesMissing{d} {}
	explicit inline Diag(const CreateArrNoExpectedType d)
		: kind{Kind::createArrNoExpectedType}, createArrNoExpectedType{d} {}
	explicit inline Diag(const CreateRecordByRefNoCtx d)
		: kind{Kind::createRecordByRefNoCtx}, createRecordByRefNoCtx{d} {}
	explicit inline Diag(const CreateRecordMultiLineWrongFields d)
		: kind{Kind::createRecordMultiLineWrongFields}, createRecordMultiLineWrongFields{d} {}
	explicit inline Diag(const DuplicateDeclaration d)
		: kind{Kind::duplicateDeclaration}, duplicateDeclaration{d} {}
	explicit inline Diag(const DuplicateImports d)
		: kind{Kind::duplicateImports}, duplicateImports{d} {}
	explicit inline Diag(const ExpectedTypeIsNotALambda d)
		: kind{Kind::expectedTypeIsNotALambda}, expectedTypeIsNotALambda{d} {}
	explicit inline Diag(const FileDoesNotExist d)
		: kind{Kind::fileDoesNotExist}, fileDoesNotExist{d} {}
	explicit inline Diag(const FileShouldEndInNz d)
		: kind{Kind::fileShouldEndInNz}, fileShouldEndInNz{d} {}
	explicit inline Diag(const FunAsLambdaCantOverload d)
		: kind{Kind::funAsLambdaCantOverload}, funAsLambdaCantOverload{d} {}
	explicit inline Diag(const FunAsLambdaWrongNumberTypeArgs d)
		: kind{Kind::funAsLambdaWrongNumberTypeArgs}, funAsLambdaWrongNumberTypeArgs{d} {}
	explicit inline Diag(const FunAsLambdaWrongReturnType d)
		: kind{Kind::funAsLambdaWrongReturnType}, funAsLambdaWrongReturnType{d} {}
	explicit inline Diag(const LambdaCantInferParamTypes d)
		: kind{Kind::lambdaCantInferParamTypes}, lambdaCantInferParamTypes{d} {}
	explicit inline Diag(const LambdaClosesOverMut d)
		: kind{Kind::lambdaClosesOverMut}, lambdaClosesOverMut{d} {}
	explicit inline Diag(const LambdaForFunPtrHasClosure d)
		: kind{Kind::lambdaForFunPtrHasClosure}, lambdaForFunPtrHasClosure{d} {}
	explicit inline Diag(const LocalShadowsPrevious d)
		: kind{Kind::localShadowsPrevious}, localShadowsPrevious{d} {}
	explicit inline Diag(const MatchCaseStructNamesDoNotMatch d)
		: kind{Kind::matchCaseStructNamesDoNotMatch}, matchCaseStructNamesDoNotMatch{d} {}
	explicit inline Diag(const MatchOnNonUnion d)
		: kind{Kind::matchOnNonUnion}, matchOnNonUnion{d} {}
	explicit inline Diag(const MutFieldNotAllowed d)
		: kind{Kind::mutFieldNotAllowed}, mutFieldNotAllowed{d} {}
	explicit inline Diag(const NameNotFound d)
		: kind{Kind::nameNotFound}, nameNotFound{d} {}
	explicit inline Diag(const ParamShadowsPrevious d)
		: kind{Kind::paramShadowsPrevious}, paramShadowsPrevious{d} {}
	explicit inline Diag(const ParseDiag d)
		: kind{Kind::parseDiag}, parseDiag{d} {}
	explicit inline Diag(const PurityOfFieldWorseThanRecord d)
		: kind{Kind::purityOfFieldWorseThanRecord}, purityOfFieldWorseThanRecord{d} {}
	explicit inline Diag(const PurityOfMemberWorseThanUnion d)
		: kind{Kind::purityOfMemberWorseThanUnion}, purityOfMemberWorseThanUnion{d} {}
	explicit inline Diag(const RelativeImportReachesPastRoot d)
		: kind{Kind::relativeImportReachesPastRoot}, relativeImportReachesPastRoot{d} {}
	explicit inline Diag(const SendFunDoesNotReturnFut d)
		: kind{Kind::sendFunDoesNotReturnFut}, sendFunDoesNotReturnFut{d} {}
	explicit inline Diag(const SpecBuiltinNotSatisfied d)
		: kind{Kind::specBuiltinNotSatisfied}, specBuiltinNotSatisfied{d} {}
	explicit inline Diag(const SpecImplHasSpecs d)
		: kind{Kind::specImplHasSpecs}, specImplHasSpecs{d} {}
	explicit inline Diag(const SpecImplNotFound d)
		: kind{Kind::specImplNotFound}, specImplNotFound{d} {}
	explicit inline Diag(const TypeConflict d)
		: kind{Kind::typeConflict}, typeConflict{d} {}
	explicit inline Diag(const TypeNotSendable d)
		: kind{Kind::typeNotSendable}, typeNotSendable{d} {}
	explicit inline Diag(const WriteToNonExistentField d)
		: kind{Kind::writeToNonExistentField}, writeToNonExistentField{d} {}
	explicit inline Diag(const WriteToNonMutableField d)
		: kind{Kind::writeToNonMutableField}, writeToNonMutableField{d} {}
	explicit inline Diag(const WrongNumberNewStructArgs d)
		: kind{Kind::wrongNumberNewStructArgs}, wrongNumberNewStructArgs{d} {}
	explicit inline Diag(const WrongNumberTypeArgsForSpec d)
		: kind{Kind::wrongNumberTypeArgsForSpec}, wrongNumberTypeArgsForSpec{d} {}
	explicit inline Diag(const WrongNumberTypeArgsForStruct d)
		: kind{Kind::wrongNumberTypeArgsForStruct}, wrongNumberTypeArgsForStruct{d} {}

	template <
		typename CbCallMultipleMatches,
		typename CbCallNoMatch,
		typename CbCantCall,
		typename CbCantCreateNonRecordType,
		typename CbCantCreateRecordWithoutExpectedType,
		typename CbCantInferTypeArguments,
		typename CbCircularImport,
		typename CbCommonTypesMissing,
		typename CbCreateArrNoExpectedType,
		typename CbCreateRecordByRefNoCtx,
		typename CbCreateRecordMultiLineWrongFields,
		typename CbDuplicateDeclaration,
		typename CbDuplicateImports,
		typename CbExpectedTypeIsNotALambda,
		typename CbFileDoesNotExist,
		typename CbFileShouldEndInNz,
		typename CbFunAsLambdaCantOverload,
		typename CbFunAsLambdaWrongNumberTypeArgs,
		typename CbFunAsLambdaWrongReturnType,
		typename CbLambdaCantInferParamTypes,
		typename CbLambdaClosesOverMut,
		typename CbLambdaForFunPtrHasClosure,
		typename CbLocalShadowsPrevious,
		typename CbMatchCaseStructNamesDoNotMatch,
		typename CbMatchOnNonUnion,
		typename CbMutFieldNotAllowed,
		typename CbNameNotFound,
		typename CbParamShadowsPrevious,
		typename CbParseDiag,
		typename CbPurityOfFieldWorseThanRecord,
		typename CbPurityOfMemberWorseThanUnion,
		typename CbRelativeImportReachesPastRoot,
		typename CbSendFunDoesNotReturnFut,
		typename CbSpecBuiltinNotSatisfied,
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
		CbCantCall cbCantCall,
		CbCantCreateNonRecordType cbCantCreateNonRecordType,
		CbCantCreateRecordWithoutExpectedType cbCantCreateRecordWithoutExpectedType,
		CbCantInferTypeArguments cbCantInferTypeArguments,
		CbCircularImport cbCircularImport,
		CbCommonTypesMissing cbCommonTypesMissing,
		CbCreateArrNoExpectedType cbCreateArrNoExpectedType,
		CbCreateRecordByRefNoCtx cbCreateRecordByRefNoCtx,
		CbCreateRecordMultiLineWrongFields cbCreateRecordMultiLineWrongFields,
		CbDuplicateDeclaration cbDuplicateDeclaration,
		CbDuplicateImports cbDuplicateImports,
		CbExpectedTypeIsNotALambda cbExpectedTypeIsNotALambda,
		CbFileDoesNotExist cbFileDoesNotExist,
		CbFileShouldEndInNz cbFileShouldEndInNz,
		CbFunAsLambdaCantOverload cbFunAsLambdaCantOverload,
		CbFunAsLambdaWrongNumberTypeArgs cbFunAsLambdaWrongNumberTypeArgs,
		CbFunAsLambdaWrongReturnType cbFunAsLambdaWrongReturnType,
		CbLambdaCantInferParamTypes cbLambdaCantInferParamTypes,
		CbLambdaClosesOverMut cbLambdaClosesOverMut,
		CbLambdaForFunPtrHasClosure cbLambdaForFunPtrHasClosure,
		CbLocalShadowsPrevious cbLocalShadowsPrevious,
		CbMatchCaseStructNamesDoNotMatch cbMatchCaseStructNamesDoNotMatch,
		CbMatchOnNonUnion cbMatchOnNonUnion,
		CbMutFieldNotAllowed cbMutFieldNotAllowed,
		CbNameNotFound cbNameNotFound,
		CbParamShadowsPrevious cbParamShadowsPrevious,
		CbParseDiag cbParseDiag,
		CbPurityOfFieldWorseThanRecord cbPurityOfFieldWorseThanRecord,
		CbPurityOfMemberWorseThanUnion cbPurityOfMemberWorseThanUnion,
		CbRelativeImportReachesPastRoot cbRelativeImportReachesPastRoot,
		CbSendFunDoesNotReturnFut cbSendFunDoesNotReturnFut,
		CbSpecBuiltinNotSatisfied cbSpecBuiltinNotSatisfied,
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
			case Kind::cantCall:
				return cbCantCall(cantCall);
			case Kind::cantCreateNonRecordType:
				return cbCantCreateNonRecordType(cantCreateNonRecordType);
			case Kind::cantCreateRecordWithoutExpectedType:
				return cbCantCreateRecordWithoutExpectedType(cantCreateRecordWithoutExpectedType);
			case Kind::cantInferTypeArguments:
				return cbCantInferTypeArguments(cantInferTypeArguments);
			case Kind::circularImport:
				return cbCircularImport(circularImport);
			case Kind::commonTypesMissing:
				return cbCommonTypesMissing(commonTypesMissing);
			case Kind::createArrNoExpectedType:
				return cbCreateArrNoExpectedType(createArrNoExpectedType);
			case Kind::createRecordByRefNoCtx:
				return cbCreateRecordByRefNoCtx(createRecordByRefNoCtx);
			case Kind::createRecordMultiLineWrongFields:
				return cbCreateRecordMultiLineWrongFields(createRecordMultiLineWrongFields);
			case Kind::duplicateDeclaration:
				return cbDuplicateDeclaration(duplicateDeclaration);
			case Kind::duplicateImports:
				return cbDuplicateImports(duplicateImports);
			case Kind::expectedTypeIsNotALambda:
				return cbExpectedTypeIsNotALambda(expectedTypeIsNotALambda);
			case Kind::fileDoesNotExist:
				return cbFileDoesNotExist(fileDoesNotExist);
			case Kind::fileShouldEndInNz:
				return cbFileShouldEndInNz(fileShouldEndInNz);
			case Kind::funAsLambdaCantOverload:
				return cbFunAsLambdaCantOverload(funAsLambdaCantOverload);
			case Kind::funAsLambdaWrongNumberTypeArgs:
				return cbFunAsLambdaWrongNumberTypeArgs(funAsLambdaWrongNumberTypeArgs);
			case Kind::funAsLambdaWrongReturnType:
				return cbFunAsLambdaWrongReturnType(funAsLambdaWrongReturnType);
			case Kind::lambdaCantInferParamTypes:
				return cbLambdaCantInferParamTypes(lambdaCantInferParamTypes);
			case Kind::lambdaClosesOverMut:
				return cbLambdaClosesOverMut(lambdaClosesOverMut);
			case Kind::lambdaForFunPtrHasClosure:
				return cbLambdaForFunPtrHasClosure(lambdaForFunPtrHasClosure);
			case Kind::localShadowsPrevious:
				return cbLocalShadowsPrevious(localShadowsPrevious);
			case Kind::matchCaseStructNamesDoNotMatch:
				return cbMatchCaseStructNamesDoNotMatch(matchCaseStructNamesDoNotMatch);
			case Kind::matchOnNonUnion:
				return cbMatchOnNonUnion(matchOnNonUnion);
			case Kind::mutFieldNotAllowed:
				return cbMutFieldNotAllowed(mutFieldNotAllowed);
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
			case Kind::relativeImportReachesPastRoot:
				return cbRelativeImportReachesPastRoot(relativeImportReachesPastRoot);
			case Kind::sendFunDoesNotReturnFut:
				return cbSendFunDoesNotReturnFut(sendFunDoesNotReturnFut);
			case Kind::specBuiltinNotSatisfied:
				return cbSpecBuiltinNotSatisfied(specBuiltinNotSatisfied);
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

struct PathAndStorageKindAndRange {
	const PathAndStorageKind pathAndStorageKind;
	const SourceRange range;
};

struct Diagnostic {
	const PathAndStorageKindAndRange where;
	const Diag diag;
};

struct FilesInfo {
	const AbsolutePathsGetter absolutePathsGetter;
	const LineAndColumnGetters lineAndColumnGetters;
};

using Diags = const Arr<const Diagnostic>;

struct Diagnostics {
	const Diags diagnostics;
	const FilesInfo filesInfo;

	inline Diagnostics(Diags _diagnostics, const FilesInfo _filesInfo)
		: diagnostics{_diagnostics}, filesInfo{_filesInfo} {
		assert(!isEmpty(diagnostics));
	}
};
