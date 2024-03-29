#pragma once

#include "./bitUtils.h"
#include "./mutDict.h"
#include "./mutSet.h"
#include "./str.h"
#include "./writer.h"

struct Sym {
	// One of 4 possibilities.
	// Short alpha identifier: packed representation, marked with shortAlphaIdentifierMarker
	// Short operator: even more packed representation (there are fewer operator chars), marked with shortOperatorMarker
	// Long alpha identifier: a CStr
	// Long operator: a CStr tagged with longOperatorMarker
	//
	// It's perfectly safe to *compare* syms with `a.value == b.value` without first testing if they're short or long.
	u64 value;
};

struct AllSymbols {
	Arena arena;
	MutDict<const Str, const CStr, compareStr> largeStrings;
};

// Ideally this would go in the implementation file, but need this for templates and constexpr functions
namespace symImpl {
	const u64 bitsPerAlphaChar = 6;
	const u64 bitsPerOperatorChar = 4;

	// Never returns 0
	// (the first '0' tells us that the string is over. Note there may be no '0' if we use all space.)
	// TODO: huffman compression could make this even better
	inline constexpr u64 packAlphaCharWorker(const char c) {
		if ('a' <= c && c <= 'z')
			return 1 + c - 'a';
		else if ('0' <= c && c <= '9')
			return 1 + 26 + c - '0';
		else if (c == '-')
			return 1 + 26 + 10;
		else if (c == '?')
			return 1 + 26 + 10 + 1;
		else
			return unreachable<const u64>();
	}

	inline constexpr u64 packAlphaChar(const char c) {
		const u64 res = packAlphaCharWorker(c);
		assert(res < (1 << bitsPerAlphaChar));
		return res;
	}

	inline constexpr char unpackAlphaChar(const u64 n) {
		assert(n != 0);
		if (n < 1 + 26)
			return 'a' + (n - 1);
		else if (n < 1 + 26 + 10)
			return '0' + (n - 1 - 26);
		else if (n == 1 + 26 + 10)
			return '-';
		else if (n == 1 + 26 + 10 + 1)
			return '?';
		else
			return unreachable<const char>();
	}

	inline constexpr u64 packOperatorCharWorker(const char c) {
		// We need 0 to tell us that the string is over, so never return that.
		switch (c) {
			case '+':
				return 1;
			case '-':
				return 2;
			case '*':
				return 3;
			case '/':
				return 4;
			case '<':
				return 5;
			case '>':
				return 6;
			case '=':
				return 7;
			case '!':
				return 8;
			default:
				return unreachable<const u64>();
		}
	}

	inline constexpr u64 packOperatorChar(const char c) {
		const u64 res = packOperatorCharWorker(c);
		assert(res < (1 << bitsPerOperatorChar));
		return res;
	}

	inline constexpr char unpackOperatorChar(const u64 n) {
		switch (n) {
			case 1:
				return '+';
			case 2:
				return '-';
			case 3:
				return '*';
			case 4:
				return '/';
			case 5:
				return '<';
			case 6:
				return '>';
			case 7:
				return '=';
			case 8:
				return '!';
			default:
				return unreachable<const char>();
		}
	}

	// We represent a short string by converting each char to 6 bits,
	// and shifting left by 6 * the char's index.
	// Note this puts the encoded characters "backwards" as char 0 is rightmost.

	// Short strings leave the top 3 bits untouched.
	const u64 shortStringAvailableBits = 64 - 3;
	const u64 maxShortAlphaIdentifierSize = shortStringAvailableBits / bitsPerAlphaChar;
	// Bit to be set when the sym is short
	const u64 shortAlphaOrOperatorMarker = 0x8000000000000000;
	// Bit to be set when the sym is alpha
	// (NOTE: this is redundant as alpha == not operator
	const u64 shortOrLongAlphaMarker     = 0x4000000000000000;
	// Bit to be set when the sym is an operator
	const u64 shortOrLongOperatorMarker  = 0x2000000000000000;
	// 8 = b1000
	const u64 shortAlphaIdentifierMarker = shortAlphaOrOperatorMarker | shortOrLongAlphaMarker;

	const u64 maxShortOperatorSize = shortStringAvailableBits / bitsPerOperatorChar;
	const u64 shortOperatorMarker = shortAlphaOrOperatorMarker | shortOrLongOperatorMarker;

	const u64 highestPossibleAlphaBit = singleBit(bitsPerAlphaChar * (maxShortAlphaIdentifierSize - 1));
	static_assert((shortAlphaIdentifierMarker & highestPossibleAlphaBit) == 0, "!");

	const u64 highestPossibleOperatorBit = singleBit(bitsPerOperatorChar * (maxShortOperatorSize - 1));
	static_assert((shortOperatorMarker & highestPossibleOperatorBit) == 0, "1");

	inline constexpr u64 packAlphaIdentifier(const Str str) {
		assert(size(str) <= maxShortAlphaIdentifierSize);
		u64 res = 0;
		for (const u64 i : Range{size(str)}) // TODO: zipwithindex
			res |= packAlphaChar(at(str, i)) << (bitsPerAlphaChar * i);
		assert((res & shortAlphaIdentifierMarker) == 0);
		return res | shortAlphaIdentifierMarker;
	}

	inline constexpr u64 packOperator(const Str str) {
		assert(size(str) <= maxShortOperatorSize);
		u64 res = 0;
		for (const u64 i : Range{size(str)}) // TODO: zipwithindex
			res |= packOperatorChar(at(str, i)) << (bitsPerOperatorChar * i);
		assert((res & shortOperatorMarker) == 0);
		return res | shortOperatorMarker;
	}

	template <typename Cb>
	inline void unpackShortAlphaIdentifier(const u64 packedStr, Cb cb) {
		assert((packedStr & shortAlphaIdentifierMarker) == shortAlphaIdentifierMarker);
		for (const u64 i : Range{maxShortAlphaIdentifierSize}) {
			const u64 packedChar = getBitsShifted(packedStr, bitsPerAlphaChar * i, bitsPerAlphaChar);
			if (packedChar == 0)
				break;
			cb(unpackAlphaChar(packedChar));
		}
	}

	template <typename Cb>
	inline void unpackShortOperator(const u64 packedStr, Cb cb) {
		assert((packedStr & shortOperatorMarker) == shortOperatorMarker);
		for (const u64 i : Range{maxShortOperatorSize}) {
			const u64 packedChar = getBitsShifted(packedStr, bitsPerOperatorChar * i, bitsPerOperatorChar);
			if (packedChar == 0)
				break;
			cb(unpackOperatorChar(packedChar));
		}
	}

	inline const Bool isLongSym(const Sym a) {
		return _not(bitsOverlap(a.value, shortAlphaOrOperatorMarker));
	}

	inline const Str asLong(const Sym a) {
		assert(isLongSym(a));
		const u64 value = a.value & ~(shortOrLongAlphaMarker | shortOrLongOperatorMarker);
		return strLiteral(reinterpret_cast<const char*>(value));
	}
}

inline constexpr const Bool isShortAlpha(const Sym a) {
	return allBitsSet(a.value, symImpl::shortAlphaIdentifierMarker);
}

inline constexpr const Bool isShortOperator(const Sym a) {
	return allBitsSet(a.value, symImpl::shortOperatorMarker);
}

// Get symbol from characters 'a'-'z' '0'-'9' '-' '?'
// str is temporary, we'll make a copy
const Sym getSymFromAlphaIdentifier(AllSymbols* allSymbols, const Str str);
// str is temporary, we'll make a copy
const Sym getSymFromOperator(AllSymbols* allSymbols, const Str str);

inline constexpr Comparison compareSym(const Sym a, const Sym b) {
	return comparePrimitive(a.value, b.value);
}

inline constexpr const Bool symEq(const Sym a, const Sym b) {
	return eq(a.value, b.value);
}

inline constexpr const Sym shortSymAlphaLiteral(const char* name) {
	assert(size(strLiteral(name)) <= symImpl::maxShortAlphaIdentifierSize);
	return Sym{symImpl::packAlphaIdentifier(strLiteral(name))};
}

inline constexpr u64 shortSymAlphaLiteralValue(const char* name) {
	return shortSymAlphaLiteral(name).value;
}

inline constexpr const Sym shortSymOperatorLiteral(const char* name) {
	assert(size(strLiteral(name)) <= symImpl::maxShortOperatorSize);
	return Sym{symImpl::packOperator(strLiteral(name))};
}

inline constexpr u64 shortSymOperatorLiteralValue(const char* name) {
	return shortSymOperatorLiteral(name).value;
}

const Bool symEqLongAlphaLiteral(const Sym a, const char* lit);

const Bool symEqLongOperatorLiteral(const Sym a, const char* lit);

const Str strOfSym(Arena* arena, const Sym a);
size_t writeSymAndGetSize(Writer* writer, const Sym s);
inline void writeSym(Writer* writer, const Sym s) {
	writeSymAndGetSize(writer, s);
}
size_t symSize(const Sym s);

inline CStr symToCStr(Arena* arena, const Sym a) {
	return strToCStr(arena, strOfSym(arena, a));
}

inline const Bool isSymOperator(const Sym a) {
	return bitsOverlap(a.value, symImpl::shortOrLongOperatorMarker);
}

template <typename Cb>
inline void eachCharInSym(const Sym a, Cb cb) {
	if (isShortAlpha(a))
		symImpl::unpackShortAlphaIdentifier(a.value, cb);
	else if (isShortOperator(a))
		symImpl::unpackShortOperator(a.value, cb);
	else
		for (const char c : symImpl::asLong(a))
			cb(c);
}

using MutSymSet = MutSet<const Sym, compareSym>;

inline bool mutSymSetHas(const MutSymSet* set, const Sym sym) {
	return mutSetHas<const Sym, compareSym>(set, sym);
}

inline void addToMutSymSetOkIfPresent(Arena* arena, MutSymSet* set, const Sym sym) {
	addToMutSetOkIfPresent<const Sym, compareSym>(arena, set, sym);
}
