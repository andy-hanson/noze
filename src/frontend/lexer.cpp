#include "./lexer.h"

namespace {
	bool isLowerCaseLetter(char c) {
		return 'a' <= c && c <= 'z';
	}

	bool isDigit(char c) {
		return '0' <= c && c <= '9';
	}

	bool isOperatorChar(char c) {
		switch (c) {
			case '+':
			case '-':
			case '*':
			case '/':
			case '<':
			case '>':
			case '=':
				return true;
			default:
				return false;
		}
	}

	bool isNameContinue(char c) {
		return isLowerCaseLetter(c) || c == '-' || isDigit(c);
	}
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
	MutStr res = newUninitializedMutSlice<const char>(arena, size);

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

	assert(outI == res.size);
	return res.freeze();
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
