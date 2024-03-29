#include "./lexer.h"

#include "../util/arrUtil.h"

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
			case '!':
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

	size_t toHexDigit(const char c) {
		if ('0' <= c && c <= '9')
			return c - '0';
		else if ('a' <= c && c <= 'f')
			return 10 + c - 'a';
		else
			return todo<const size_t>("parse diagnostic -- bad hex digit");
	}

	const Str takeStringLiteralAfterQuote(Lexer* lexer) {
		const CStr begin = lexer->ptr;
		size_t nEscapedCharacters = 0;
		// First get the max size
		while (*lexer->ptr != '"') {
			if (*lexer->ptr == '\\') {
				lexer->ptr++;
				nEscapedCharacters += (*lexer->ptr == 'x' ? 3 : 1);
			}
			lexer->ptr++;
		}

		const size_t size = (lexer->ptr - begin) - nEscapedCharacters;
		MutStr res = newUninitializedMutArr<const char>(lexer->arena, size);

		size_t outI = 0;
		lexer->ptr = begin;
		while (*lexer->ptr != '"') {
			if (*lexer->ptr == '\\') {
				lexer->ptr++;
				const char c = [&]() {
					switch (*lexer->ptr) {
						case 'x': {
							// Take two more
							lexer->ptr++;
							const char a = *lexer->ptr;
							lexer->ptr++;
							const char b = *lexer->ptr;
							const size_t na = toHexDigit(a);
							const size_t nb = toHexDigit(b);
							return (char) (na * 16 + nb);
						}
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

	uint takeIndentAmount(Lexer* lexer) {
		const CStr begin = lexer->ptr;
		if (lexer->indentKind == IndentKind::tabs) {
			while (*lexer->ptr == '\t')
				lexer->ptr++;
			if (*lexer->ptr == ' ')
				throwAtChar<void>(lexer, ParseDiag{ParseDiag::IndentWrongCharacter{.expectedTabs = True}});
			return safeSizeTToUint(lexer->ptr - begin);
		} else {
			const Pos start = curPos(lexer);
			while (*lexer->ptr == ' ')
				lexer->ptr++;
			if (*lexer->ptr == '\t')
				throwAtChar<void>(lexer, ParseDiag{ParseDiag::IndentWrongCharacter{.expectedTabs = False}});
			const uint nSpaces = safeSizeTToUint(lexer->ptr - begin);
			const uint nSpacesPerIndent = lexer->indentKind == IndentKind::spaces2 ? 2 : 4;
			const uint res = nSpaces / nSpacesPerIndent;
			if (res * nSpacesPerIndent != nSpaces)
				throwDiag<void>(
					range(lexer, start),
					ParseDiag{ParseDiag::IndentNotDivisible{nSpaces, nSpacesPerIndent}});
			return res;
		}
	}

	// Returns the change in indent (and updates the indent)
	// Note: does nothing if not looking at a newline!
	// NOTE: never returns a value > 1 as double-indent is always illegal.
	int skipLinesAndGetIndentDelta(Lexer* lexer) {
		// comment / region counts as a blank line no matter its indent level.
		const uint newIndent = takeIndentAmount(lexer);

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
			if (res > 1)
				todo<void>("too much indent");
			lexer->indent = newIndent;
			return res;
		}
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
			case shortSymAlphaLiteralValue("export"):
			case shortSymAlphaLiteralValue("extern"):
			case shortSymAlphaLiteralValue("global"):
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

const Str takeQuotedStr(Lexer* lexer) {
	take(lexer, '"');
	return takeStringLiteralAfterQuote(lexer);
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

void skipShebang(Lexer* lexer) {
	if (tryTake(lexer, "#!"))
		skipRestOfLine(lexer);
}

void skipBlankLines(Lexer* lexer)  {
	const int i = skipLinesAndGetIndentDelta(lexer);
	if (i != 0)
		throwUnexpected<void>(lexer);
}

const Opt<const NewlineOrIndent> tryTakeNewlineOrIndent(Lexer* lexer) {
	const Pos start = curPos(lexer);
	if (tryTake(lexer, '\n')) {
		const int delta = skipLinesAndGetIndentDelta(lexer);
		switch (delta) {
			case 0:
				return some<const NewlineOrIndent>(NewlineOrIndent::newline);
			case 1:
				return some<const NewlineOrIndent>(NewlineOrIndent::indent);
			default:
				return throwDiag<const Opt<const NewlineOrIndent>>(
					range(lexer, start),
					ParseDiag{ParseDiag::UnexpectedDedent{}});
		}
	} else
		return none<const NewlineOrIndent>();
}

NewlineOrIndent takeNewlineOrIndent(Lexer* lexer) {
	const Opt<const NewlineOrIndent> op = tryTakeNewlineOrIndent(lexer);
	return has(op) ? force(op) : throwUnexpected<const NewlineOrIndent>(lexer);
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

const Opt<const int> tryTakeIndentOrDedent(Lexer* lexer) {
	return eq<const char>(curChar(lexer), '\n')
		? some<const int>(skipLinesAndGetIndentDelta(lexer))
		: none<const int>();
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

void takeIndentAfterNewline(Lexer* lexer) {
	const int delta = skipLinesAndGetIndentDelta(lexer);
	if (delta != 1)
		throwAtChar<void>(lexer, ParseDiag{ParseDiag::ExpectedIndent{}});
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

namespace {
	// Note: Not issuing any diagnostics here. We'll fail later if we detect the wrong indent kind.
	IndentKind detectIndentKind(const Str str) {
		if (isEmpty(str))
			// No indented lines, so it's irrelevant
			return IndentKind::tabs;
		else {
			const char c0 = first(str);
			if (c0 == '\t')
				return IndentKind::tabs;
			else if (c0 == ' ') {
				// Count spaces
				size_t i = 0;
				for (; i < size(str); i++)
					if (at(str, i) != ' ')
						break;
				// Only allowed amounts are 2 and 4.
				return i == 2 ? IndentKind::spaces2 : IndentKind::spaces4;
			} else {
				for (size_t i = 0; i < size(str); i++)
					if (at(str, i) == '\n')
						return detectIndentKind(slice(str, i + 1));
				return IndentKind::tabs;
			}
		}
	}
}

Lexer createLexer(Arena* arena, AllSymbols* allSymbols, const NulTerminatedStr source) {
	// Note: We *are* relying on the nul terminator to stop the lexer.
	const Str str = stripNulTerminator(source);
	const uint len = safeSizeTToUint(size(str));
	if (!isEmpty(str) && last(str) != '\n')
		throw ParseDiagnostic{
			SourceRange{len - 1, len},
			ParseDiag{ParseDiag::MustEndInBlankLine{}}};

	return Lexer{
		.arena = arena,
		.allSymbols = allSymbols,
		.sourceBegin = str.begin(),
		.ptr = str.begin(),
		.indentKind = detectIndentKind(str),
		.indent = 0};
}
