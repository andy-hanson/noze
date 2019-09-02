#pragma once

#include "../diag.h" // ParseDiag
#include "./parse.h" // ParseDiagnostic

#include "../util/arrUtil.h" // copyStr

struct Lexer {
	Arena* arena;
	AllSymbols* allSymbols;
	// Null-terminated.
	const CStr sourceBegin;
	// Starts at sourceBegin and proceeds until it hits NUL
	CStr ptr;
	size_t indent = 0;
};

inline char curChar(Lexer* lexer) {
	return *lexer->ptr;
}

inline Pos curPos(const Lexer* lexer) {
	return safeSizeTToUint(lexer->ptr - lexer->sourceBegin);
}

template <typename T>
T throwDiag(const ParseDiagnostic pd) {
	throw pd;
}

template <typename T>
T throwDiag(const SourceRange range, const ParseDiag diag) {
	return throwDiag<T>(ParseDiagnostic{range, diag});
}

template <typename T>
T throwAtChar(Lexer* lexer, const ParseDiag diag) {
	const Pos a = curPos(lexer);
	const SourceRange range = SourceRange{a, curChar(lexer) == '\0' ? a : a + 1};
	return throwDiag<T>(range, diag);
}

const Bool tryTake(Lexer* lexer, const char c);
const Bool tryTake(Lexer* lexer, const CStr c);
void take(Lexer* lexer, const char c);
void take(Lexer* lexer, const CStr c) ;

void skipBlankLines(Lexer* lexer);

enum class NewlineOrIndent { newline, indent };
NewlineOrIndent takeNewlineOrIndent(Lexer* lexer);

void takeIndent(Lexer* lexer);
void takeDedent(Lexer* lexer);
const Bool tryTakeIndent(Lexer* lexer);
NewlineOrIndent tryTakeIndentAfterNewline(Lexer* lexer);

// Returns # of dedents. (TODO:RENAME)
size_t takeNewlineOrDedentAmount(Lexer* lexer);

enum class NewlineOrDedent { newline, dedent };
NewlineOrDedent takeNewlineOrSingleDedent(Lexer* lexer);

inline const SourceRange range(Lexer* lexer, const Pos begin) {
	assert(begin < curPos(lexer));
	return SourceRange{begin, curPos(lexer)};
}

template <typename T>
T throwOnReservedName(const SourceRange range, const Sym name) {
	return throwDiag<T>(range, ParseDiag{ParseDiag::ReservedName{name}});
}

struct SymAndIsReserved {
	const Sym sym;
	const SourceRange range;
	const Bool isReserved;
};

const SymAndIsReserved takeNameAllowReserved(Lexer* lexer);
const Sym takeName(Lexer* lexer);
const Str takeNameAsStr(Lexer* lexer);
const NameAndRange takeNameAndRange(Lexer* lexer);

struct ExpressionToken {
	enum class Kind {
		ampersand,
		lambda,
		lbrace,
		literal,
		lparen,
		match,
		nameAndRange,
		_new,
		newActor,
		newArr,
		when
	};
	const Kind kind;
	union {
		const LiteralAst literal;
		const NameAndRange nameAndRange;
	};
	explicit inline ExpressionToken(Kind _kind) : kind{_kind} {
		assert(kind != Kind::literal && kind != Kind::nameAndRange);
	}
	explicit inline ExpressionToken(const LiteralAst _literal)
		: kind{Kind::literal}, literal{_literal} {}
	explicit inline ExpressionToken(const NameAndRange _nameAndRange)
		: kind{Kind::nameAndRange}, nameAndRange{_nameAndRange} {}

	inline LiteralAst asLiteral() const {
		assert(kind == Kind::literal);
		return literal;
	}
	inline const Bool isNameAndRange() const {
		return enumEq(kind, Kind::nameAndRange);
	}
	inline NameAndRange asNameAndRange() const {
		assert(isNameAndRange());
		return nameAndRange;
	}
};
const ExpressionToken takeExpressionToken(Lexer* lexer);

const Bool tryTakeElseIndent(Lexer* lexer);

Lexer createLexer(Arena* astArena, AllSymbols* allSymbols, const NulTerminatedStr source);
