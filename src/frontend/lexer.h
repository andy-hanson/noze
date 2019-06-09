#pragma once

#include "../diag.h" // ParseDiag
#include "./parse.h" // ParseDiagnostic

struct Lexer {
	Arena& arena;
	Arena& pathArena;

private:
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

	inline char next() {
		const char res = *ptr;
		ptr++;
		return res;
	}

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
		return throwAtChar<T>(ParseDiag{ParseDiag::Kind::unexpectedCharacter, cur()});
	}

	inline bool tryTake(const char c) {
		if (*ptr == c) {
			ptr++;
			return true;
		} else
			return false;
	}

	inline bool tryTake(const char* c) {
		const char* ptr2 = ptr;
		for (const char* cptr = c; *cptr != 0; cptr++) {
			if (*ptr2 != *cptr)
				return false;
			ptr2++;
		}
		ptr = ptr2;
		return true;
	}

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

	inline const SourceRange range(const char* begin) {
		assert(begin >= sourceBegin);
		return range(safeSizeTToUint(begin - sourceBegin));
	}

private:
	uint takeTabs() {
		const char* begin = ptr;
		while (*ptr == '\t')
			ptr++;
		return safeSizeTToUint(ptr - begin);
	}

	// Returns the change in indent (and updates the indent)
	int skipLinesAndGetIndentDelta() {
		while (true) {
			if (*ptr == '|') {
				ptr++;
				while (*ptr != '\n')
					ptr++;
			}
			else if (tryTake("region ")) {
				while (*ptr != '\n') ptr++;
			}
			// If either of the above happened we will enter here
			if (*ptr == '\n') {
				takeNewline(); // skip all empty lines
				const uint newIndent = takeTabs();
				if (newIndent != indent) {
					int res = ((int) newIndent) - ((int) indent);
					indent = newIndent;
					return res;
				}
			} else
				return 0;
		}
	}

	void takeNewline() {
		take("\n");
		takeExtraNewlines();
	}

	void takeExtraNewlines() {
		while (*ptr == '\n')
			ptr++;
	}

	bool tryTakeNewline() {
		const bool res = *ptr == '\n';
		if (res)
			takeNewline();
		return res;
	}

public:
	void skipBlankLines() {
		const int i = skipLinesAndGetIndentDelta();
		if (i != 0)
			throwUnexpected<void>();
	}

	enum class NewlineOrIndent { newline, indent };
	NewlineOrIndent takeNewlineOrIndent() {
		takeNewline();
		return takeNewlineOrIndentAfterNl();
	}

	NewlineOrIndent takeNewlineOrIndentAfterNl() {
		const size_t newIndent = takeTabs();
		const NewlineOrIndent res =
			newIndent == indent ? NewlineOrIndent::newline
			: newIndent != indent + 1 ? throwUnexpected<NewlineOrIndent>()
			: [&]() {
				indent = newIndent;
				return NewlineOrIndent::indent;
			}();
		skipBlankLines();
		return res;
	}

	void takeIndent() {
		takeNewline();
		takeIndentAfterNewline();
	}

	void takeDedent() {
		assert(indent == 1);
		takeNewline();
		const size_t newIndent = takeTabs();
		if (newIndent != 0)
			todo<void>("takeDedent");
		indent = 0;
	}

	bool tryTakeIndent() {
		const bool res = tryTakeNewline();
		if (res) takeIndentAfterNewline();
		return res;
	}

	bool tryTakeIndentAfterNewline() {
		takeExtraNewlines();
		const size_t newIndent = takeTabs();
		if (newIndent != indent && newIndent != indent + 1)
			todo<void>("expected 0 or 1 indent");
		const bool res = newIndent == indent + 1;
		indent = newIndent;
		return res;
	}

private:
	void takeIndentAfterNewline() {
		const size_t newIndent = takeTabs();
		if (newIndent != indent + 1)
			throwAtChar<void>(ParseDiag{ParseDiag::Kind::expectedIndent});
		indent = newIndent;
		skipBlankLines();
	}

public:
	// Returns # of dedents. (TODO:RENAME)
	size_t takeNewlineOrDedentAmount() {
		const int i = skipLinesAndGetIndentDelta();
		if (i > 0)
			todo<void>("takeNewlineOrDedentAmoutn -- actually it indented");
		return -i;
	}

	enum class NewlineOrDedent { newline, dedent };
	NewlineOrDedent takeNewlineOrSingleDedent() {
		assert(indent == 1);
		const size_t amnt = takeNewlineOrDedentAmount();
		switch (amnt) {
			case 0: return NewlineOrDedent::newline;
			case 1: return NewlineOrDedent::dedent;
			default: return unreachable<NewlineOrDedent>();
		}
	}

private:
	const Str copyStr(const char* begin, const char* end) const {
		return ::copyStr(arena, Str::ofRange(begin, end));
	}

	const Str takeNameRest(const char* begin);

public:
	const Str takeName();

	const NameAndRange takeNameAndRange() {
		const char* begin = ptr;
		const Str name = takeName();
		return NameAndRange{range(begin), name};
	}

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
		inline bool isNameAndRange() const {
			return kind == Kind::nameAndRange;
		}
		inline NameAndRange asNameAndRange() const {
			assert(isNameAndRange());
			return nameAndRange;
		}
	};

	const ExpressionToken takeExpressionToken();

private:
	const Str takeOperatorRest(const char* begin);

	const ExpressionToken takeOperator(const char* begin) {
		const Str name = takeOperatorRest(begin);
		return ExpressionToken{NameAndRange{range(begin), name}};
	}

	const ExpressionToken takeNumber(const char* begin);

	const Str takeStringLiteral();

public:
	bool tryTakeElseIndent() {
		const bool res = tryTake("else\n");
		if (res)
			takeIndentAfterNewline();
		return res;
	}
};

Lexer createLexer(Arena& astArena, Arena& pathArena, const NulTerminatedStr source);
