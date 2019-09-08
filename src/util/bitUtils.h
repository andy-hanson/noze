#pragma once

#include "./bool.h"
#include "./range.h"

using u64 = uint64_t;

inline constexpr u64 singleBit(const u64 bitIndex) {
	return static_cast<u64>(1) << bitIndex;
}

inline constexpr const Bool bitsOverlap(const u64 a, const u64 b) {
	return neq<u64>(a & b, 0);
}

// Tests if 'a' has *all* bits from 'b' set
inline constexpr const Bool allBitsSet(const u64 a, const u64 b) {
	return eq<u64>(a & b, b);
}

inline constexpr const Bool getBit(const u64 n, const u64 bitIndex) {
	return bitsOverlap(n, singleBit(bitIndex));
}

inline constexpr u64 setBit(const u64 n, const u64 bitIndex) {
	return n | singleBit(bitIndex);
}

// Bits are counted such that the rightmost bit is bit 0. (Since it is the 2^0th place)
inline u64 getBitsUnshifted(const u64 n, const u64 firstBitIndex, const u64 nBits) {
	u64 mask = 0;
	for (const size_t i : Range{firstBitIndex, firstBitIndex + nBits})
		mask = setBit(mask, i);
	const u64 res = n & mask;
	for (const size_t i : Range{0, firstBitIndex})
		assert(!getBit(res, i));
	for (const size_t i : Range{firstBitIndex + nBits, 64})
		assert(!getBit(res, i));
	return res;
}

inline u64 getBitsShifted(const u64 n, const u64 firstBitIndex, const u64 nBits) {
	u64 res = getBitsUnshifted(n, firstBitIndex, nBits) >> firstBitIndex;
	for (const size_t i : Range{nBits, 64})
		assert(!getBit(res, i));
	return res;
}
