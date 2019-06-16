#include "./lexer.h"

namespace {
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
		const char* begin = lexer.ptr;
		while (*lexer.ptr == '\t')
			lexer.ptr++;
		return safeSizeTToUint(lexer.ptr - begin);
	}

	void takeIndentAfterNewline(Lexer& lexer) {
		const size_t newIndent = takeTabs(lexer);
		if (newIndent != lexer.indent + 1)
			lexer.throwAtChar<void>(ParseDiag{ParseDiag::ExpectedIndent{}});
		lexer.indent = newIndent;
		lexer.skipBlankLines();
	}
}

const Bool Lexer::tryTake(const char* c)  {
	const char* ptr2 = ptr;
	for (const char* cptr = c; *cptr != 0; cptr++) {
		if (*ptr2 != *cptr)
			return False;
		ptr2++;
	}
	ptr = ptr2;
	return True;
}

int Lexer::skipLinesAndGetIndentDelta() {
	for (;;) {
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
			const uint newIndent = takeTabs(*this);
			if (newIndent != indent) {
				int res = ((int) newIndent) - ((int) indent);
				indent = newIndent;
				return res;
			}
		} else
			return 0;
	}
}

void Lexer::takeExtraNewlines() {
	while (*ptr == '\n')
		ptr++;
}

const Str Lexer::takeNameRest(const char* begin)  {
	while (isNameContinue(*ptr))
		ptr++;
	if (*ptr == '?')
		ptr++;
	return copyStr(begin, ptr);
}

const Str Lexer::takeOperatorRest(const char* begin)  {
	while (isOperatorChar(*ptr))
		ptr++;
	return copyStr(begin, ptr);
}

const Lexer::ExpressionToken Lexer::takeOperator(const char* begin)  {
	const Str name = takeOperatorRest(begin);
	return ExpressionToken{NameAndRange{range(begin), name}};
}

const Str Lexer::takeName() {
	const char* begin = ptr;
	if (isOperatorChar(*ptr)) {
		ptr++;
		return takeOperatorRest(begin);
	} else if (isLowerCaseLetter(*ptr)) {
		ptr++;
		return takeNameRest(begin);
	} else
		return throwUnexpected<Str>();
}

const NameAndRange Lexer::takeNameAndRange()  {
	const char* begin = ptr;
	const Str name = takeName();
	return NameAndRange{range(begin), name};
}

const Lexer::ExpressionToken Lexer::takeNumber(const char* begin)  {
	while (isDigit(*ptr) || *ptr == '.') ptr++;
	return ExpressionToken{copyStr(begin, ptr)};
}

const Lexer::ExpressionToken Lexer::takeExpressionToken()  {
	const char* begin = ptr;
	const char c = next();
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
			return ExpressionToken{takeStringLiteral()};
		case '+':
		case '-':
			return isDigit(*ptr) ? takeNumber(begin) : takeOperator(begin);
		default:
			if (isOperatorChar(c))
				return takeOperator(begin);
			else if (isLowerCaseLetter(c)) {
				const Str name = takeNameRest(begin);
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
					: ExpressionToken{NameAndRange{range(begin), name}};
			} else if (isDigit(c))
				return takeNumber(begin);
			else
				return throwUnexpected<const ExpressionToken>();
	}
}

const Str Lexer::takeStringLiteral() {
	const char* begin = ptr;
	size_t nEscapes = 0;
	ptr++;
	// First get the max size
	while (*ptr != '"') {
		if (*ptr == '\\') {
			nEscapes++;
			// Not ending at an escaped quote
			ptr++;
		}
		ptr++;
	}

	const size_t size = (ptr - begin) - nEscapes;
	MutStr res = newUninitializedMutArr<const char>(arena, size);

	size_t outI = 0;
	ptr = begin;
	while (*ptr != '"') {
		if (*ptr == '\\') {
			ptr++;
			const char c = [&]() {
				switch (*ptr) {
					case '"':
						return '"';
					case 'n':
						return '\n';
					case 't':
						return '\t';
					case '0':
						return '\0';
					default:
						return throwUnexpected<char>();
				}
			}();
			res.set(outI, c);
			outI++;
		} else {
			res.set(outI, *ptr);
			outI++;
		}
		ptr++;
	}

	ptr++; // Skip past the closing '"'

	assert(outI == res.size());
	return res.freeze();
}

void Lexer::skipBlankLines()  {
	const int i = skipLinesAndGetIndentDelta();
	if (i != 0)
		throwUnexpected<void>();
}

Lexer::NewlineOrIndent Lexer::takeNewlineOrIndent() {
	takeNewline();
	return takeNewlineOrIndentAfterNl();
}

Lexer::NewlineOrIndent Lexer::takeNewlineOrIndentAfterNl() {
	const size_t newIndent = takeTabs(*this);
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

void Lexer::takeIndent() {
	takeNewline();
	takeIndentAfterNewline(*this);
}

void Lexer::takeDedent()  {
	assert(indent == 1);
	takeNewline();
	const size_t newIndent = takeTabs(*this);
	if (newIndent != 0)
		todo<void>("takeDedent");
	indent = 0;
}

const Bool Lexer::tryTakeIndent()  {
	const Bool res = eq(*ptr, '\n');
	if (res)
		takeIndent();
	return res;
}

const Bool Lexer::tryTakeIndentAfterNewline() {
	takeExtraNewlines();
	const size_t newIndent = takeTabs(*this);
	if (newIndent != indent && newIndent != indent + 1)
		todo<void>("expected 0 or 1 indent");
	const Bool res = eq(newIndent, indent + 1);
	indent = newIndent;
	return res;
}

size_t Lexer::takeNewlineOrDedentAmount() {
	const int i = skipLinesAndGetIndentDelta();
	if (i > 0)
		todo<void>("takeNewlineOrDedentAmoutn -- actually it indented");
	return -i;
}

Lexer::NewlineOrDedent Lexer::takeNewlineOrSingleDedent() {
	assert(indent == 1);
	const size_t amnt = takeNewlineOrDedentAmount();
	switch (amnt) {
		case 0: return NewlineOrDedent::newline;
		case 1: return NewlineOrDedent::dedent;
		default: return unreachable<NewlineOrDedent>();
	}
}

const Bool tryTakeElseIndent(Lexer& lexer)  {
	const Bool res = lexer.tryTake("else\n");
	if (res)
		takeIndentAfterNewline(lexer);
	return res;
}

Lexer createLexer(Arena& astArena, Arena& pathArena, const NulTerminatedStr source) {
	const uint len = safeSizeTToUint(source.size);
	assert(len != 0);
	assert(len < 9999);
	if (len == 1)
		todo<void>("empty file"); // TODO: allow this, but check that that's safe
	else if (source[len - 2] != '\n')
		throw ParseDiagnostic{
			SourceRange{len - 2, len - 1},
			ParseDiag{ParseDiag::MustEndInBlankLine{}}};

	for (const char* ptr = source.begin(); ptr != source.end(); ptr++) {
		if (*ptr == '\n') {
			const uint i = safeSizeTToUint(ptr - source.begin());
			if (*(ptr + 1) == ' ')
				throw ParseDiagnostic{SourceRange{i + 1, i + 2}, ParseDiag{ParseDiag::LeadingSpace{}}};
			else if (ptr != source.begin()) {
				const char prev = *(ptr - 1);
				if (prev == ' ' || prev == '\t')
					throw ParseDiagnostic{SourceRange{i - 1, i}, ParseDiag{ParseDiag::TrailingSpace{}}};
			}
		}
	}

	return Lexer{astArena, pathArena, source};
}
