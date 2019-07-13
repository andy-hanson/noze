#include "./sym.h"

namespace {
	const Sym getSymFromLongStr(Symbols* symbols, const Str str, const Bool isOperator) {
		const CStr cstr = getOrAddAndCopyKey<const Str, const CStr, compareStr>{}(
			&symbols->arena,
			&symbols->largeStrings,
			str,
			[&]() {
				return copyStr(&symbols->arena, str);
			},
			[&]() {
				return strToCStr(&symbols->arena, str);
			});
		const Nat64 res = reinterpret_cast<const Nat64>(cstr) | (isOperator ? symImpl::shortOrLongOperatorMarker : symImpl::shortOrLongAlphaMarker);
		assert((res & (symImpl::shortAlphaOrOperatorMarker)) == 0);
		return Sym{res};
	}

	void assertSym(const Sym sym, const Str str) {
		//TODO:KILL
		size_t idx = 0;
		eachCharInSym(sym, [&](const char c) {
			const char expected = at(str, idx++);
			assert(c == expected);
		});
		assert(idx == size(str));
	}
}

const Sym getSymFromAlphaIdentifier(Symbols* symbols, const Str str) {
	const Sym res = size(str) <= symImpl::maxShortAlphaIdentifierSize
		? Sym{symImpl::packAlphaIdentifier(str)}
		: getSymFromLongStr(symbols, str, False);
	assertSym(res, str);
	return res;
}

const Sym getSymFromOperator(Symbols* symbols, const Str str) {
	const Sym res = size(str) <= symImpl::maxShortOperatorSize
		? Sym{symImpl::packOperator(str)}
		: getSymFromLongStr(symbols, str, True);
	assertSym(res, str);
	return res;
}

const Bool symEqLongAlphaLiteral(const Sym a, const char* lit) {
	const Str str = strLiteral(lit);
	assert(size(str) > symImpl::maxShortAlphaIdentifierSize);
	return _and(
		symImpl::isLongSym(a),
		strEqLiteral(symImpl::asLong(a), lit));
}

const Bool symEqLongOperatorLiteral(const Sym a, const char* lit) {
	const Str str = strLiteral(lit);
	assert(size(str) > symImpl::maxShortOperatorSize);
	return _and(
		symImpl::isLongSym(a),
		strEqLiteral(symImpl::asLong(a), lit));
}

const Str strOfSym(Arena* arena, const Sym a) {
	Writer writer { arena };
	writeSym(&writer, a);
	return finishWriter(&writer);
}

void writeSym(Writer* writer, const Sym a) {
	eachCharInSym(a, [&](const char c) {
		writeChar(writer, c);
	});
}
