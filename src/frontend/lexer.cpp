#include "./lexer.h"

namespace {
	template <typename T>
	inline T throwUnexpected(Lexer* lexer) {
		return throwAtChar<T>(lexer, ParseDiag{ParseDiag::UnexpectedCharacter{curChar(lexer)}});
	}

	char next(Lexer* lexer) {
		const char res = *lexer->ptr;
		lexer->ptr++;
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
		return _or3(
			isLowerCaseLetter(c),
			c == '-',
			isDigit(c));
	}

	uint takeTabs(Lexer* lexer) {
		const CStr begin = lexer->ptr;
		while (*lexer->ptr == '\t')
			lexer->ptr++;
		return safeSizeTToUint(lexer->ptr - begin);
	}

	const SourceRange range(Lexer* lexer, const CStr begin) {
		assert(begin >= lexer->sourceBegin);
		return range(lexer, safeSizeTToUint(begin - lexer->sourceBegin));
	}

	const ExpressionToken takeNumber(Lexer* lexer, const CStr begin)  {
		while (isDigit(*lexer->ptr) || *lexer->ptr == '.') lexer->ptr++;
		const Str text = copyStr(lexer->arena, arrOfRange(begin, lexer->ptr));
		return ExpressionToken{LiteralAst{LiteralAst::Kind::numeric, text}};
	}

	const Str takeOperatorRest(Lexer* lexer, const CStr begin)  {
		while (isOperatorChar(*lexer->ptr))
			lexer->ptr++;
		return arrOfRange(begin, lexer->ptr);
	}

	const ExpressionToken takeOperator(Lexer* lexer, const CStr begin)  {
		const Str name = takeOperatorRest(lexer, begin);
		return ExpressionToken{NameAndRange{range(lexer, begin), getSymFromOperator(lexer->allSymbols, name)}};
	}

	const Str takeStringLiteralAfterQuote(Lexer* lexer) {
		const CStr begin = lexer->ptr;
		size_t nEscapes = 0;
		// First get the max size
		while (*lexer->ptr != '"') {
			if (*lexer->ptr == '\\') {
				nEscapes++;
				// Not ending at an escaped quote
				lexer->ptr++;
			}
			lexer->ptr++;
		}

		const size_t size = (lexer->ptr - begin) - nEscapes;
		MutStr res = newUninitializedMutArr<const char>(lexer->arena, size);

		size_t outI = 0;
		lexer->ptr = begin;
		while (*lexer->ptr != '"') {
			if (*lexer->ptr == '\\') {
				lexer->ptr++;
				const char c = [&]() {
					switch (*lexer->ptr) {
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
				setAt<const char>(&res, outI, c);
				outI++;
			} else {
				setAt<const char>(&res, outI, *lexer->ptr);
				outI++;
			}
			lexer->ptr++;
		}

		lexer->ptr++; // Skip past the closing '"'

		assert(outI == mutArrSize(&res));
		return freeze(&res);
	}

	const Str takeNameRest(Lexer* lexer, const CStr begin) {
		while (isNameContinue(*lexer->ptr))
			lexer->ptr++;
		if (*lexer->ptr == '?')
			lexer->ptr++;
		return arrOfRange(begin, lexer->ptr);
	}

	void skipRestOfLine(Lexer* lexer) {
		while (*lexer->ptr != '\n')
			lexer->ptr++;
		lexer->ptr++;
	}

	// Returns the change in indent (and updates the indent)
	// Note: does nothing if not looking at a newline!
	int skipLinesAndGetIndentDelta(Lexer* lexer) {
		// comment / region counts as a blank line no matter its indent level.
		const uint newIndent = takeTabs(lexer);
		if (tryTake(lexer, '\n'))
			return skipLinesAndGetIndentDelta(lexer);
		if (tryTake(lexer, '|')) {
			skipRestOfLine(lexer);
			return skipLinesAndGetIndentDelta(lexer);
		} else if (tryTake(lexer, "region ")) {
			skipRestOfLine(lexer);
			return skipLinesAndGetIndentDelta(lexer);
		} else {
			// If we got here, we're looking at a non-empty line (or EOF)
			int res = static_cast<int>(newIndent) - static_cast<int>(lexer->indent);
			lexer->indent = newIndent;
			return res;
		}
	}

	void takeIndentAfterNewline(Lexer* lexer) {
		const int delta = skipLinesAndGetIndentDelta(lexer);
		if (delta != 1)
			throwAtChar<void>(lexer, ParseDiag{ParseDiag::ExpectedIndent{}});
	}

	struct StrAndIsOperator {
		const Str str;
		const SourceRange range;
		const Bool isOperator;
	};

	const StrAndIsOperator takeNameAsTempStr(Lexer* lexer) {
		const CStr begin = lexer->ptr;
		if (isOperatorChar(*lexer->ptr)) {
			lexer->ptr++;
			const Str op = takeOperatorRest(lexer, begin);
			return StrAndIsOperator{op, range(lexer, begin), True};
		} else if (isLowerCaseLetter(*lexer->ptr)) {
			lexer->ptr++;
			const Str name = takeNameRest(lexer, begin);
			return StrAndIsOperator{name, range(lexer, begin), False};
		} else
			return throwUnexpected<const StrAndIsOperator>(lexer);
	}
}

const Bool tryTake(Lexer* lexer, const char c) {
	if (*lexer->ptr == c) {
		lexer->ptr++;
		return True;
	} else
		return False;
}

const Bool tryTake(Lexer* lexer, const CStr c)  {
	CStr ptr2 = lexer->ptr;
	for (CStr cptr = c; *cptr != 0; cptr++) {
		if (*ptr2 != *cptr)
			return False;
		ptr2++;
	}
	lexer->ptr = ptr2;
	return True;
}

void take(Lexer* lexer, const char c) {
	if (!tryTake(lexer, c))
		throwUnexpected<void>(lexer);
}

void take(Lexer* lexer, const CStr c) {
	if (!tryTake(lexer, c))
		throwUnexpected<void>(lexer);
}

namespace {
	const Bool isReservedName(const Sym name) {
		switch (name.value) {
			case shortSymAlphaLiteralValue("alias"):
			case shortSymAlphaLiteralValue("builtin"):
			case shortSymAlphaLiteralValue("else"):
			case shortSymAlphaLiteralValue("extern"):
			case shortSymAlphaLiteralValue("iface"):
			case shortSymAlphaLiteralValue("import"):
			case shortSymAlphaLiteralValue("match"):
			case shortSymAlphaLiteralValue("mut"):
			case shortSymAlphaLiteralValue("new"):
			case shortSymAlphaLiteralValue("new-actor"):
			case shortSymAlphaLiteralValue("new-arr"):
			case shortSymAlphaLiteralValue("noctx"):
			case shortSymAlphaLiteralValue("record"):
			case shortSymAlphaLiteralValue("sendable"):
			case shortSymAlphaLiteralValue("spec"):
			case shortSymAlphaLiteralValue("summon"):
			case shortSymAlphaLiteralValue("trusted"):
			case shortSymAlphaLiteralValue("union"):
			case shortSymAlphaLiteralValue("unsafe"):
			case shortSymAlphaLiteralValue("when"):
				return True;
			default:
				return False;
		}
	}
}

const SymAndIsReserved takeNameAllowReserved(Lexer* lexer) {
	const StrAndIsOperator s = takeNameAsTempStr(lexer);
	if (s.isOperator) {
		const Sym op = getSymFromOperator(lexer->allSymbols, s.str);
		return SymAndIsReserved{op, s.range, symEq(op, shortSymOperatorLiteral("="))};
	}
	else {
		const Sym name = getSymFromAlphaIdentifier(lexer->allSymbols, s.str);
		return SymAndIsReserved{name, s.range, isReservedName(name)};
	}
}

const Sym takeName(Lexer* lexer) {
	const SymAndIsReserved s = takeNameAllowReserved(lexer);
	return s.isReserved
		? throwOnReservedName<const Sym>(s.range, s.sym)
		: s.sym;
}

const Str takeNameAsStr(Lexer* lexer) {
	return copyStr(lexer->arena, takeNameAsTempStr(lexer).str);
}

const NameAndRange takeNameAndRange(Lexer* lexer)  {
	const CStr begin = lexer->ptr;
	const Sym name = takeName(lexer);
	return NameAndRange{range(lexer, begin), name};
}

const ExpressionToken takeExpressionToken(Lexer* lexer)  {
	const CStr begin = lexer->ptr;
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
			return ExpressionToken{LiteralAst{LiteralAst::Kind::string, takeStringLiteralAfterQuote(lexer)}};
		case '+':
		case '-':
			return isDigit(*lexer->ptr)
				? takeNumber(lexer, begin)
				: takeOperator(lexer, begin);
		default:
			if (isOperatorChar(c))
				return takeOperator(lexer, begin);
			else if (isLowerCaseLetter(c)) {
				const Str nameStr = takeNameRest(lexer, begin);
				const Sym name = getSymFromAlphaIdentifier(lexer->allSymbols, nameStr);
				const SourceRange nameRange = range(lexer, begin);
				if (isReservedName(name))
					switch (name.value) {
						case shortSymAlphaLiteralValue("match"):
							return ExpressionToken{ExpressionToken::Kind::match};
						case shortSymAlphaLiteralValue("new"):
							return ExpressionToken{ExpressionToken::Kind::_new};
						case shortSymAlphaLiteralValue("new-actor"):
							return ExpressionToken{ExpressionToken::Kind::newActor};
						case shortSymAlphaLiteralValue("new-arr"):
							return ExpressionToken{ExpressionToken::Kind::newArr};
						case shortSymAlphaLiteralValue("when"):
							return ExpressionToken{ExpressionToken::Kind::when};
						default:
							return throwOnReservedName<const ExpressionToken>(nameRange, name);
					}
				else
					return ExpressionToken{NameAndRange{nameRange, name}};
			} else if (isDigit(c))
				return takeNumber(lexer, begin);
			else
				return throwUnexpected<const ExpressionToken>(lexer);
	}
}

void skipBlankLines(Lexer* lexer)  {
	const int i = skipLinesAndGetIndentDelta(lexer);
	if (i != 0)
		throwUnexpected<void>(lexer);
}

NewlineOrIndent takeNewlineOrIndent(Lexer* lexer) {
	take(lexer, '\n');
	const int delta = skipLinesAndGetIndentDelta(lexer);
	switch (delta) {
		case 0:
			return NewlineOrIndent::newline;
		case 1:
			return NewlineOrIndent::indent;
		default:
			return todo<const NewlineOrIndent>("diagnostic: took unexpected dedent, or too many indents");
	}
}

void takeIndent(Lexer* lexer) {
	take(lexer, '\n');
	takeIndentAfterNewline(lexer);
}

void takeDedent(Lexer* lexer)  {
	const int delta = skipLinesAndGetIndentDelta(lexer);
	if (delta != -1)
		throwAtChar<void>(lexer, ParseDiag{ParseDiag::ExpectedDedent{}});
}

const Bool tryTakeIndent(Lexer* lexer)  {
	const Bool res = eq<const char>(curChar(lexer), '\n');
	if (res)
		takeIndent(lexer);
	return res;
}

NewlineOrIndent tryTakeIndentAfterNewline(Lexer* lexer) {
	const int delta = skipLinesAndGetIndentDelta(lexer);
	switch (delta) {
		case 0:
			return NewlineOrIndent::newline;
		case 1:
			return NewlineOrIndent::indent;
		default:
			return todo<const NewlineOrIndent>("diagnostic");
	}
}

size_t takeNewlineOrDedentAmount(Lexer* lexer) {
	// Must be at the end of a line
	take(lexer, '\n');
	const int i = skipLinesAndGetIndentDelta(lexer);
	return i > 0 ? throwAtChar<size_t>(lexer, ParseDiag{ParseDiag::UnexpectedIndent{}}) : -i;
}

NewlineOrDedent takeNewlineOrSingleDedent(Lexer* lexer) {
	assert(lexer->indent == 1);
	const size_t amnt = takeNewlineOrDedentAmount(lexer);
	switch (amnt) {
		case 0: return NewlineOrDedent::newline;
		case 1: return NewlineOrDedent::dedent;
		default: return unreachable<NewlineOrDedent>();
	}
}

const Bool tryTakeElseIndent(Lexer* lexer)  {
	const Bool res = tryTake(lexer, "else\n");
	if (res)
		takeIndentAfterNewline(lexer);
	return res;
}

Lexer createLexer(Arena* arena, AllSymbols* allSymbols, const NulTerminatedStr source) {
	// Note: We *are* relying on the nul terminator to stop the lexer->
	const Str str = stripNulTerminator(source);
	const uint len = safeSizeTToUint(size(str));
	assert(len < 99999);

	if (len == 0)
		todo<void>("empty file"); // TODO: allow this, but check that that's safe
	else if (last(str) != '\n')
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

	return Lexer{arena, allSymbols, /*sourceBegin*/ str.begin(), /*ptr*/ str.begin(), /*indent*/ 0};
}
