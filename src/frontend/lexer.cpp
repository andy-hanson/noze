#include "./lexer.h"

namespace {
	template <typename T>
	inline T throwUnexpected(Lexer& lexer) {
		return throwAtChar<T>(lexer, ParseDiag{ParseDiag::UnexpectedCharacter{curChar(lexer)}});
	}

	char next(Lexer& lexer) {
		const char res = *lexer.ptr;
		lexer.ptr++;
		return res;
	}

	const Bool isLowerCaseLetter(const char c) {
		return _and('a' <= c, c <= 'z');
	}

	const Bool isDigit(const char c) {
		return _and('0' <= c, c <= '9');
	}

	const Bool isOperatorChar(const char c) {
		switch (c) {
			case '+':
			case '-':
			case '*':
			case '/':
			case '<':
			case '>':
			case '=':
				return True;
			default:
				return False;
		}
	}

	const Bool isNameContinue(const char c) {
		return _or3(isLowerCaseLetter(c), c == '-', isDigit(c));
	}

	uint takeTabs(Lexer& lexer) {
		const CStr begin = lexer.ptr;
		while (*lexer.ptr == '\t')
			lexer.ptr++;
		return safeSizeTToUint(lexer.ptr - begin);
	}

	const SourceRange range(Lexer& lexer, const CStr begin) {
		assert(begin >= lexer.sourceBegin);
		return range(lexer, safeSizeTToUint(begin - lexer.sourceBegin));
	}

	const Str copyStr(Lexer& lexer, const CStr begin, const CStr end) {
		return ::copyStr(lexer.arena, arrOfRange(begin, end));
	}

	const ExpressionToken takeNumber(Lexer& lexer, const CStr begin)  {
		while (isDigit(*lexer.ptr) || *lexer.ptr == '.') lexer.ptr++;
		return ExpressionToken{copyStr(lexer, begin, lexer.ptr)};
	}

	const Str takeOperatorRest(Lexer& lexer, const CStr begin)  {
		while (isOperatorChar(*lexer.ptr))
			lexer.ptr++;
		return copyStr(lexer, begin, lexer.ptr);
	}

	const ExpressionToken takeOperator(Lexer& lexer, const CStr begin)  {
		const Str name = takeOperatorRest(lexer, begin);
		return ExpressionToken{NameAndRange{range(lexer, begin), name}};
	}

	const Str takeStringLiteral(Lexer& lexer) {
		const CStr begin = lexer.ptr;
		size_t nEscapes = 0;
		lexer.ptr++;
		// First get the max size
		while (*lexer.ptr != '"') {
			if (*lexer.ptr == '\\') {
				nEscapes++;
				// Not ending at an escaped quote
				lexer.ptr++;
			}
			lexer.ptr++;
		}

		const size_t size = (lexer.ptr - begin) - nEscapes;
		MutStr res = newUninitializedMutArr<const char>(lexer.arena, size);

		size_t outI = 0;
		lexer.ptr = begin;
		while (*lexer.ptr != '"') {
			if (*lexer.ptr == '\\') {
				lexer.ptr++;
				const char c = [&]() {
					switch (*lexer.ptr) {
						case '"':
							return '"';
						case 'n':
							return '\n';
						case 't':
							return '\t';
						case '0':
							return '\0';
						default:
							return throwUnexpected<char>(lexer);
					}
				}();
				res.set(outI, c);
				outI++;
			} else {
				res.set(outI, *lexer.ptr);
				outI++;
			}
			lexer.ptr++;
		}

		lexer.ptr++; // Skip past the closing '"'

		assert(outI == res.size());
		return res.freeze();
	}

	void takeIndentAfterNewline(Lexer& lexer) {
		const size_t newIndent = takeTabs(lexer);
		if (newIndent != lexer.indent + 1)
			throwAtChar<void>(lexer, ParseDiag{ParseDiag::ExpectedIndent{}});
		lexer.indent = newIndent;
		skipBlankLines(lexer);
	}

	const Str takeNameRest(Lexer& lexer, const CStr begin) {
		while (isNameContinue(*lexer.ptr))
			lexer.ptr++;
		if (*lexer.ptr == '?')
			lexer.ptr++;
		return copyStr(lexer, begin, lexer.ptr);
	}

	void takeExtraNewlines(Lexer& lexer) {
		while (*lexer.ptr == '\n')
			lexer.ptr++;
	}

	void takeNewline(Lexer& lexer) {
		take(lexer, "\n");
		takeExtraNewlines(lexer);
	}

	// Returns the change in indent (and updates the indent)
	int skipLinesAndGetIndentDelta(Lexer& lexer) {
		for (;;) {
			if (*lexer.ptr == '|') {
				lexer.ptr++;
				while (*lexer.ptr != '\n')
					lexer.ptr++;
			}
			else if (tryTake(lexer, "region ")) {
				while (*lexer.ptr != '\n')
					lexer.ptr++;
			}
			// If either of the above happened we will enter here
			if (*lexer.ptr == '\n') {
				takeNewline(lexer); // skip all empty lines
				const uint newIndent = takeTabs(lexer);
				if (newIndent != lexer.indent) {
					int res = static_cast<int>(newIndent) - static_cast<int>(lexer.indent);
					lexer.indent = newIndent;
					return res;
				}
			} else
				return 0;
		}
	}
}

const Bool tryTake(Lexer& lexer, const char c) {
	if (*lexer.ptr == c) {
		lexer.ptr++;
		return True;
	} else
		return False;
}

const Bool tryTake(Lexer& lexer, const CStr c)  {
	CStr ptr2 = lexer.ptr;
	for (CStr cptr = c; *cptr != 0; cptr++) {
		if (*ptr2 != *cptr)
			return False;
		ptr2++;
	}
	lexer.ptr = ptr2;
	return True;
}

void take(Lexer& lexer, const char c) {
	if (!tryTake(lexer, c))
		throwUnexpected<void>(lexer);
}

void take(Lexer& lexer, const CStr c) {
	if (!tryTake(lexer, c))
		throwUnexpected<void>(lexer);
}

const Str takeName(Lexer& lexer) {
	const CStr begin = lexer.ptr;
	if (isOperatorChar(*lexer.ptr)) {
		lexer.ptr++;
		return takeOperatorRest(lexer, begin);
	} else if (isLowerCaseLetter(*lexer.ptr)) {
		lexer.ptr++;
		return takeNameRest(lexer, begin);
	} else
		return throwUnexpected<const Str>(lexer);
}

const NameAndRange takeNameAndRange(Lexer& lexer)  {
	const CStr begin = lexer.ptr;
	const Str name = takeName(lexer);
	return NameAndRange{range(lexer, begin), name};
}

const ExpressionToken takeExpressionToken(Lexer& lexer)  {
	const CStr begin = lexer.ptr;
	const char c = next(lexer);
	switch (c) {
		case '(':
			return ExpressionToken{ExpressionToken::Kind::lparen};
		case '{':
			return ExpressionToken{ExpressionToken::Kind::lbrace};
		case '&':
			return ExpressionToken{ExpressionToken::Kind::ampersand};
		case '\\':
			return ExpressionToken{ExpressionToken::Kind::lambda};
		case '"':
			return ExpressionToken{takeStringLiteral(lexer)};
		case '+':
		case '-':
			return isDigit(*lexer.ptr)
				? takeNumber(lexer, begin)
				: takeOperator(lexer, begin);
		default:
			if (isOperatorChar(c))
				return takeOperator(lexer, begin);
			else if (isLowerCaseLetter(c)) {
				const Str name = takeNameRest(lexer, begin);
				return strEqLiteral(name, "actor")
					? ExpressionToken{ExpressionToken::Kind::actor}
					: strEqLiteral(name, "match")
					? ExpressionToken{ExpressionToken::Kind::match}
					: strEqLiteral(name, "new")
					? ExpressionToken{ExpressionToken::Kind::_new}
					: strEqLiteral(name, "new-arr")
					 ? ExpressionToken{ExpressionToken::Kind::newArr}
					: strEqLiteral(name, "when")
					? ExpressionToken{ExpressionToken::Kind::when}
					: ExpressionToken{NameAndRange{range(lexer, begin), name}};
			} else if (isDigit(c))
				return takeNumber(lexer, begin);
			else
				return throwUnexpected<const ExpressionToken>(lexer);
	}
}

void skipBlankLines(Lexer& lexer)  {
	const int i = skipLinesAndGetIndentDelta(lexer);
	if (i != 0)
		throwUnexpected<void>(lexer);
}

NewlineOrIndent takeNewlineOrIndent(Lexer& lexer) {
	takeNewline(lexer);
	return takeNewlineOrIndentAfterNl(lexer);
}

NewlineOrIndent takeNewlineOrIndentAfterNl(Lexer& lexer) {
	const size_t newIndent = takeTabs(lexer);
	const NewlineOrIndent res =
		newIndent == lexer.indent ? NewlineOrIndent::newline
		: newIndent != lexer.indent + 1 ? throwUnexpected<const NewlineOrIndent>(lexer)
		: [&]() {
			lexer.indent = newIndent;
			return NewlineOrIndent::indent;
		}();
	skipBlankLines(lexer);
	return res;
}

void takeIndent(Lexer& lexer) {
	takeNewline(lexer);
	takeIndentAfterNewline(lexer);
}

void takeDedent(Lexer& lexer)  {
	assert(lexer.indent == 1);
	takeNewline(lexer);
	const size_t newIndent = takeTabs(lexer);
	if (newIndent != 0)
		todo<void>("takeDedent");
	lexer.indent = 0;
}

const Bool tryTakeIndent(Lexer& lexer)  {
	const Bool res = eq(*lexer.ptr, '\n');
	if (res)
		takeIndent(lexer);
	return res;
}

const Bool tryTakeIndentAfterNewline(Lexer& lexer) {
	takeExtraNewlines(lexer);
	const size_t newIndent = takeTabs(lexer);
	if (newIndent != lexer.indent && newIndent != lexer.indent + 1)
		todo<void>("expected 0 or 1 indent");
	const Bool res = eq(newIndent, lexer.indent + 1);
	lexer.indent = newIndent;
	return res;
}

size_t takeNewlineOrDedentAmount(Lexer& lexer) {
	const int i = skipLinesAndGetIndentDelta(lexer);
	if (i > 0)
		todo<void>("takeNewlineOrDedentAmoutn -- actually it indented");
	return -i;
}

NewlineOrDedent takeNewlineOrSingleDedent(Lexer& lexer) {
	assert(lexer.indent == 1);
	const size_t amnt = takeNewlineOrDedentAmount(lexer);
	switch (amnt) {
		case 0: return NewlineOrDedent::newline;
		case 1: return NewlineOrDedent::dedent;
		default: return unreachable<NewlineOrDedent>();
	}
}

const Bool tryTakeElseIndent(Lexer& lexer)  {
	const Bool res = tryTake(lexer, "else\n");
	if (res)
		takeIndentAfterNewline(lexer);
	return res;
}

Lexer createLexer(Arena& arena, const NulTerminatedStr source) {
	// Note: We *are* relying on the nul terminator to stop the lexer.
	const Str str = stripNulTerminator(source);
	const uint len = safeSizeTToUint(str.size);
	assert(len < 99999);

	if (len == 0)
		todo<void>("empty file"); // TODO: allow this, but check that that's safe
	else if (str[len - 1] != '\n')
		throw ParseDiagnostic{
			SourceRange{len - 1, len},
			ParseDiag{ParseDiag::MustEndInBlankLine{}}};

	for (CStr ptr = str.begin(); ptr != str.end(); ptr++)
		if (*ptr == '\n') {
			const uint i = safeSizeTToUint(ptr - str.begin());
			if (*(ptr + 1) == ' ')
				throw ParseDiagnostic{SourceRange{i + 1, i + 2}, ParseDiag{ParseDiag::LeadingSpace{}}};
			else if (ptr != str.begin()) {
				const char prev = *(ptr - 1);
				if (prev == ' ' || prev == '\t')
					throw ParseDiagnostic{SourceRange{i - 1, i}, ParseDiag{ParseDiag::TrailingSpace{}}};
			}
		}

	return Lexer{arena, /*sourceBegin*/ str.begin(), /*ptr*/ str.begin(), /*indent*/ 0};
}
