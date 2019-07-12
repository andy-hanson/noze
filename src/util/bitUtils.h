#pragma once

#include "./bool.h"
#include "./range.h"

inline constexpr Nat64 singleBit(const Nat64 bitIndex) {
	return (static_cast<Nat64>(1) << bitIndex);
}

inline constexpr const Bool bitsOverlap(const Nat64 a, const Nat64 b) {
	return _not(eq<const Nat64>(a & b, 0));
}

// Tests if 'a' has *all* bits from 'b' set
inline constexpr const Bool allBitsSet(const Nat64 a, const Nat64 b) {
	return eq<const Nat64>(a & b, b);
}

inline constexpr const Bool getBit(const Nat64 n, const Nat64 bitIndex) {
	return bitsOverlap(n, singleBit(bitIndex));
}

inline constexpr Nat64 setBit(const Nat64 n, const Nat64 bitIndex) {
	return n | singleBit(bitIndex);
}

// Bits are counted such that the rightmost bit is bit 0. (Since it is the 2^0th place)
inline Nat64 getBitsUnshifted(const Nat64 n, const Nat64 firstBitIndex, const Nat64 nBits) {
	Nat64 mask = 0;
	for (const size_t i : Range{firstBitIndex, firstBitIndex + nBits})
		mask = setBit(mask, i);
	const Nat64 res = n & mask;
	for (const size_t i : Range{0, firstBitIndex})
		assert(!getBit(res, i));
	for (const size_t i : Range{firstBitIndex + nBits, 64})
		assert(!getBit(res, i));
	return res;
}

inline Nat64 getBitsShifted(const Nat64 n, const Nat64 firstBitIndex, const Nat64 nBits) {
	Nat64 res = getBitsUnshifted(n, firstBitIndex, nBits) >> firstBitIndex;
	for (const size_t i : Range{nBits, 64})
		assert(!getBit(res, i));
	return res;
}
