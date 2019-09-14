#pragma once

#include "./util/sourceRange.h"
#include "./util/sym.h"

struct ParseDiag {
	struct ExpectedCharacter {
		const char ch;
	};
	struct ExpectedDedent {};
	struct ExpectedIndent {};
	struct ExpectedPurityAfterSpace {};
	struct IndentNotDivisible {
		const uint nSpaces;
		const uint nSpacesPerIndent;
	};
	struct IndentWrongCharacter {
		const Bool expectedTabs;
	};
	struct LetMustHaveThen {};
	struct MatchWhenNewMayNotAppearInsideArg {};
	struct MustEndInBlankLine {};
	struct ReservedName {
		const Sym name;
	};
	struct TypeParamCantHaveTypeArgs {};
	struct UnexpectedCharacter {
		const char ch;
	};
	struct UnexpectedDedent {};
	struct UnexpectedIndent {};
	struct UnionCantBeEmpty {};
	struct WhenMustHaveElse {};

private:
	enum class Kind {
		expectedCharacter,
		expectedDedent,
		expectedIndent,
		expectedPurityAfterSpace,
		indentNotDivisible,
		indentWrongCharacter,
		letMustHaveThen,
		matchWhenNewMayNotAppearInsideArg,
		mustEndInBlankLine,
		reservedName,
		typeParamCantHaveTypeArgs,
		unexpectedCharacter,
		unexpectedDedent,
		unexpectedIndent,
		unionCantBeEmpty,
		whenMustHaveElse,
	};
	const Kind kind;
	union {
		const ExpectedCharacter expectedCharacter;
		const ExpectedDedent expectedDedent;
		const ExpectedIndent expectedIndent;
		const ExpectedPurityAfterSpace expectedPurityAfterSpace;
		const IndentNotDivisible indentNotDivisible;
		const IndentWrongCharacter indentWrongCharacter;
		const LetMustHaveThen letMustHaveThen;
		const MatchWhenNewMayNotAppearInsideArg matchWhenNewMayNotAppearInsideArg;
		const MustEndInBlankLine mustEndInBlankLine;
		const ReservedName reservedName;
		const TypeParamCantHaveTypeArgs typeParamCantHaveTypeArgs;
		const UnexpectedCharacter unexpectedCharacter;
		const UnexpectedDedent unexpectedDedent;
		const UnexpectedIndent unexpectedIndent;
		const UnionCantBeEmpty unionCantBeEmpty;
		const WhenMustHaveElse whenMustHaveElse;
	};

public:
	explicit inline ParseDiag(const ExpectedCharacter d)
		: kind{Kind::expectedCharacter}, expectedCharacter{d} {}
	explicit inline ParseDiag(const ExpectedDedent d)
		: kind{Kind::expectedDedent}, expectedDedent{d} {}
	explicit inline ParseDiag(const ExpectedIndent d)
		: kind{Kind::expectedIndent}, expectedIndent{d} {}
	explicit inline ParseDiag(const ExpectedPurityAfterSpace d)
		: kind{Kind::expectedPurityAfterSpace}, expectedPurityAfterSpace{d} {}
	explicit inline ParseDiag(const IndentNotDivisible d)
		: kind{Kind::indentNotDivisible}, indentNotDivisible{d} {}
	explicit inline ParseDiag(const IndentWrongCharacter d)
		: kind{Kind::indentWrongCharacter}, indentWrongCharacter{d} {}
	explicit inline ParseDiag(const LetMustHaveThen d)
		: kind{Kind::letMustHaveThen}, letMustHaveThen{d} {}
	explicit inline ParseDiag(const MatchWhenNewMayNotAppearInsideArg d)
		: kind{Kind::matchWhenNewMayNotAppearInsideArg}, matchWhenNewMayNotAppearInsideArg{d} {}
	explicit inline ParseDiag(const MustEndInBlankLine d)
		: kind{Kind::mustEndInBlankLine}, mustEndInBlankLine{d} {}
	explicit inline ParseDiag(const ReservedName d)
		: kind{Kind::reservedName}, reservedName{d} {}
	explicit inline ParseDiag(const TypeParamCantHaveTypeArgs d)
		: kind{Kind::typeParamCantHaveTypeArgs}, typeParamCantHaveTypeArgs{d} {}
	explicit inline ParseDiag(const UnexpectedCharacter d)
		: kind{Kind::unexpectedCharacter}, unexpectedCharacter{d} {}
	explicit inline ParseDiag(const UnexpectedDedent d)
		: kind{Kind::unexpectedDedent}, unexpectedDedent{d} {}
	explicit inline ParseDiag(const UnexpectedIndent d)
		: kind{Kind::unexpectedIndent}, unexpectedIndent{d} {}
	explicit inline ParseDiag(const UnionCantBeEmpty d)
		: kind{Kind::unionCantBeEmpty}, unionCantBeEmpty{d} {}
	explicit inline ParseDiag(const WhenMustHaveElse d)
		: kind{Kind::whenMustHaveElse}, whenMustHaveElse{d} {}

	template <
		typename CbExpectedCharacter,
		typename CbExpectedDedent,
		typename CbExpectedIndent,
		typename CbExpectedPurityAfterSpace,
		typename CbIndentNotDivisible,
		typename CbIndentWrongCharacter,
		typename CbMatchWhenNewMayNotAppearInsideArg,
		typename CbMustEndInBlankLine,
		typename CbLetMustHaveThen,
		typename CbReservedName,
		typename CbTypeParamCantHaveTypeArgs,
		typename CbUnexpectedCharacter,
		typename CbUnexpectedDedent,
		typename CbUnexpectedIndent,
		typename CbUnionCantBeEmpty,
		typename CbWhenMustHaveElse
	> inline auto match(
		CbExpectedCharacter cbExpectedCharacter,
		CbExpectedDedent cbExpectedDedent,
		CbExpectedIndent cbExpectedIndent,
		CbExpectedPurityAfterSpace cbExpectedPurityAfterSpace,
		CbIndentNotDivisible cbIndentNotDivisible,
		CbIndentWrongCharacter cbIndentWrongCharacter,
		CbLetMustHaveThen cbLetMustHaveThen,
		CbMatchWhenNewMayNotAppearInsideArg cbMatchWhenNewMayNotAppearInsideArg,
		CbMustEndInBlankLine cbMustEndInBlankLine,
		CbReservedName cbReservedName,
		CbTypeParamCantHaveTypeArgs cbTypeParamCantHaveTypeArgs,
		CbUnexpectedCharacter cbUnexpectedCharacter,
		CbUnexpectedDedent cbUnexpectedDedent,
		CbUnexpectedIndent cbUnexpectedIndent,
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
			case Kind::indentNotDivisible:
				return cbIndentNotDivisible(indentNotDivisible);
			case Kind::indentWrongCharacter:
				return cbIndentWrongCharacter(indentWrongCharacter);
			case Kind::letMustHaveThen:
				return cbLetMustHaveThen(letMustHaveThen);
			case Kind::matchWhenNewMayNotAppearInsideArg:
				return cbMatchWhenNewMayNotAppearInsideArg(matchWhenNewMayNotAppearInsideArg);
			case Kind::mustEndInBlankLine:
				return cbMustEndInBlankLine(mustEndInBlankLine);
			case Kind::reservedName:
				return cbReservedName(reservedName);
			case Kind::typeParamCantHaveTypeArgs:
				return cbTypeParamCantHaveTypeArgs(typeParamCantHaveTypeArgs);
			case Kind::unexpectedCharacter:
				return cbUnexpectedCharacter(unexpectedCharacter);
			case Kind::unexpectedDedent:
				return cbUnexpectedDedent(unexpectedDedent);
			case Kind::unexpectedIndent:
				return cbUnexpectedIndent(unexpectedIndent);
			case Kind::unionCantBeEmpty:
				return cbUnionCantBeEmpty(unionCantBeEmpty);
			case Kind::whenMustHaveElse:
				return cbWhenMustHaveElse(whenMustHaveElse);
			default:
				assert(0);
		}
	}
};

// Only used temporarily while parsing
struct ParseDiagnostic {
	const SourceRange range;
	const ParseDiag diag;
};
