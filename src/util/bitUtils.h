Nat64 setBit(const Nat64 n, const Nat64 bitIndex) {
	return n | (1 << bitIndex);
}

// Bits are counted such that the rightmost bit is bit 0. (Since it is the 2^0th place)
Nat64 getBitsUnshifted(const Nat64 n, const Nat64 firstBitIndex, const Nat64 nBits) {
	Nat64 mask = 0;
	for (const size_t i : Range{firstBitIndex, firstBitIndex + nBits})
		mask = setBit(mask, i);
	return n | mask;
}

Nat64 getBitsShifted(const Nat64 n, const Nat64 firstBitIndex, const Nat64 nBits) {
	return getBitsUnshifted(n, firstBitIndex, nBits) >> firstBitIndex;
}

