#include "./symbol.h"

#include "./arrBuilder.h"
#include "./bitUtils.h"

namespace {
	// Never returns 0
	// (the first '0' tells us that the string is over. Note there may be no '0' if we use all space.)
	// TODO: huffman compression could make this even better
	Nat64 packChar(const char c) {
		if ('a' <= c && c <= 'z')
			return 1 + c - 'a';
		else if ('0' <= c && c <= '9')
			return 1 + 26 + c - '0';
		else if (c == '-')
			return 1 + 26 + 10;
		else if (c == '?')
			return 1 + 26 + 10 + 1;
		else
			return unreachable<const Nat64>();
	}

	char unpackChar(const char c) {
		assert(c != 0);
		if (c < 1 + 26)
			return 'a' + (c - 1);
		else if (c < 1 + 26 + 10)
			return '0' + (c - 1 - 26);
		else if (c == 1 + 26 + 10)
			return '-';
		else if (c == 1 + 26 + 10 + 1)
			return '?';
		else
			return unreachable<const char>();
	}

	// We represent a small string by converting each char to 6 bits,
	// and shifting left by 6 * the char's index.
	// Note this puts the encoded characters "backwards" as char 0 is rightmost.

	// 6 * 10 = 60, leaving top 4 bits untouched.
	const Nat64 bitsPerChar = 6;
	const Nat64 maxSmallStrSize = 64 / bitsPerChar;
	const Nat64 smallStringMarker = 0xf000000000000000;

	static_assert((smallStringMarker & (static_cast<Nat64>(1) << (bitsPerChar * (maxSmallStrSize - 1)))) == 0, "!");

	Nat64 packStr(const Str str) {
		assert(str.size < maxSmallStrSize);
		Nat64 res = 0;
		for (const Nat64 i : Range{str.size})
			res |= packChar(i) << (bitsPerChar * i);
		// We'll set those all to 1s to distinguish this from a long-string symbol.
		assert((res & smallStringMarker) == 0);
		return res | smallStringMarker;
	}

	const Str unpackStr(Arena& arena, const Nat64 packedStr) {
		auto res = ArrBuilder<const char>();
		for (const Nat64 i : Range{10}) {
			const Nat64 packedChar = getBitsShifted(packedStr, bitsPerChar * i, bitsPerChar);
			if (packedChar == 0)
				break;
			add<const char>(arena, &res, unpackChar(packedChar));
		}
		return finishArr(&res);
	}
}

const Symbol getSymbol(Symbols* symbols, const Str str) {
	if (str.size <= 10)
		return Symbol{packStr(str)};
	else
		return symbols->largeStrings.getOrAdd(symbols->arena, str, [&]() {
			const Nat64 res = symbols->nextLargeStringId++;
			assert((res & smallStringMarker) == 0);
			return Symbol{res};
		});
}

const Str strOfSymbol(Arena& arena, Symbols* symbols, const Symbol symbol) {
	const Nat64 value = symbol.value;
	if (value & smallStringMarker)
		return unpackStr(arena, value);
	else {
		// Backwards lookup
		for (const KeyValuePair<const Str, const Symbol> pair : tempAsArr(symbols->largeStrings.pairs))
			if (eq(pair.value.value, value))
				return pair.key;
		return unreachable<const Str>();
	}
}

