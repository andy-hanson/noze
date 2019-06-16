#pragma once

#include "../diag.h" // ParseDiag
#include "./parse.h" // ParseDiagnostic

#include "../util/arrUtil.h" // copyStr

struct Lexer {
	Arena& arena;
	Arena& pathArena;

	// Need to remember the beginning to get index
	const char* sourceBegin;
	const char* ptr;
	size_t indent = 0;

public:
	inline Lexer(Arena& astArena, Arena& _pathArena, const NulTerminatedStr p)
		: arena{astArena}, pathArena{_pathArena}, sourceBegin{p._begin}, ptr{p._begin} {}

	inline char cur() const {
		return *ptr;
	}

private:
	inline char next() {
		const char res = *ptr;
		ptr++;
		return res;
	}

public:
	inline Pos at() const {
		return safeSizeTToUint(ptr - sourceBegin);
	}

	template <typename T>
	inline T throwDiag(const ParseDiagnostic pd) const {
		throw pd;
	}

	template <typename T>
	inline T throwDiag(const SourceRange range, const ParseDiag diag) const {
		return throwDiag<T>(ParseDiagnostic{range, diag});
	}

	template <typename T>
	inline T throwAtChar(const ParseDiag diag) const {
		const Pos a = at();
		const SourceRange range = SourceRange{a, cur() == '\0' ? a : a + 1};
		return throwDiag<T>(range, diag);
	}

	template <typename T>
	inline T throwUnexpected() const {
		return throwAtChar<T>(ParseDiag{ParseDiag::UnexpectedCharacter{cur()}});
	}

	inline const Bool tryTake(const char c) {
		if (*ptr == c) {
			ptr++;
			return True;
		} else
			return False;
	}

	const Bool tryTake(const char* c);

	inline void take(const char c) {
		if (!tryTake(c))
			throwUnexpected<void>();
	}

	inline void take(const char* c) {
		if (!tryTake(c))
			throwUnexpected<void>();
	}

	inline const SourceRange range(const Pos begin) const {
		assert(begin < at());
		return SourceRange{begin, at()};
	}

private:
	inline const SourceRange range(const char* begin) {
		assert(begin >= sourceBegin);
		return range(safeSizeTToUint(begin - sourceBegin));
	}

	// Returns the change in indent (and updates the indent)
	int skipLinesAndGetIndentDelta();

	void takeNewline() {
		take("\n");
		takeExtraNewlines();
	}

	void takeExtraNewlines();

public:
	void skipBlankLines();

	enum class NewlineOrIndent { newline, indent };
	NewlineOrIndent takeNewlineOrIndent();
	NewlineOrIndent takeNewlineOrIndentAfterNl();

	void takeIndent();

	void takeDedent();

	const Bool tryTakeIndent();

	const Bool tryTakeIndentAfterNewline();

public:
	// Returns # of dedents. (TODO:RENAME)
	size_t takeNewlineOrDedentAmount();

	enum class NewlineOrDedent { newline, dedent };
	NewlineOrDedent takeNewlineOrSingleDedent();

private:
	const Str copyStr(const char* begin, const char* end) const {
		return ::copyStr(arena, arrOfRange(begin, end));
	}

	const Str takeNameRest(const char* begin);

public:
	const Str takeName();

	const NameAndRange takeNameAndRange();

	struct ExpressionToken {
		enum class Kind {
			actor,
			ampersand,
			lambda,
			lbrace,
			literal,
			lparen,
			match,
			nameAndRange,
			_new,
			newArr,
			when
		};
		const Kind kind;
		union {
			const Str literal;
			const NameAndRange nameAndRange;
		};
		inline ExpressionToken(Kind _kind) : kind{_kind} {
			assert(kind != Kind::literal && kind != Kind::nameAndRange);
		}
		inline ExpressionToken(const Str _literal)
			: kind{Kind::literal}, literal{_literal} {}
		inline ExpressionToken(const NameAndRange _nameAndRange)
			: kind{Kind::nameAndRange}, nameAndRange{_nameAndRange} {}

		inline Str asLiteral() const {
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

	const ExpressionToken takeExpressionToken();

private:
	const Str takeOperatorRest(const char* begin);
	const ExpressionToken takeOperator(const char* begin);
	const ExpressionToken takeNumber(const char* begin);
	const Str takeStringLiteral();
};

const Bool tryTakeElseIndent(Lexer& lexer);

Lexer createLexer(Arena& astArena, Arena& pathArena, const NulTerminatedStr source);
