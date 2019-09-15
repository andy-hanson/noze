#pragma once

#include <cassert>
#include <cstdint>
#include <stdio.h>

#include "./bool.h"
#include "./comparison.h"

template <typename T>
inline T todo(const char* message) {
	printf("TODO: %s\n", message);
	throw message;
}

using CStr = const char*;
struct Byte {
	const uint8_t value;
};
struct _void {
	const uint8_t _;
};
// Don't want these implicitly converting to each other, so wrapping in a struct
struct Int16 {
	const int16_t value;
};
struct Int32 {
	const int32_t value;
};
struct Int64 {
	const int64_t value;
};
struct Nat16 {
	const uint16_t value;
};
struct Nat32 {
	const uint32_t value;
};
struct Nat64 {
	const uint64_t value;
};
inline const Nat64 nat64FromNat16(const Nat16 n) {
	return Nat64{n.value};
}
inline const Nat64 nat64FromNat32(const Nat32 n) {
	return Nat64{n.value};
}

inline const Int64 int64FromInt16(const Int16 i) {
	return Int64{i.value};
}
inline const Int64 int64FromInt32(const Int32 i) {
	return Int64{i.value};
}

inline const Nat64 toNat64(const Nat32 a) {
	return Nat64{a.value};
}

inline const Nat16 operator+(const Nat16 a, const Nat16 b) {
	//TODO: safety
	return Nat16{static_cast<uint16_t>(a.value + b.value)};
}
inline const Nat32 operator+(const Nat32 a, const Nat32 b) {
	//TODO: safety
	return Nat32{a.value + b.value};
}
inline const Nat64 operator+(const Nat64 a, const Nat64 b) {
	//TODO: safety
	return Nat64{a.value + b.value};
}

inline const Nat32 operator-(const Nat32 a, const Nat32 b) {
	//TODO: safety
	return Nat32{a.value - b.value};
}
inline const Nat64 operator-(const Nat64 a, const Nat64 b) {
	//TODO: safety
	return Nat64{a.value - b.value};
}

inline const Nat32 operator*(const Nat32 a, const Nat32 b) {
	//TODO: safety
	return Nat32{a.value * b.value};
}
inline constexpr Nat64 operator*(const Nat64 a, const Nat64 b) {
	//TODO: safety
	return Nat64{a.value * b.value};
}
inline constexpr Nat64 operator/(const Nat64 a, const Nat64 b) {
	assert(b.value != 0);
	return Nat64{a.value / b.value};
}
inline constexpr Nat64 operator%(const Nat64 a, const Nat64 b) {
	assert(b.value != 0);
	return Nat64{a.value % b.value};
}


inline const Int64 operator+(const Int64 a, const Int64 b) {
	//TODO: safety
	return Int64{a.value + b.value};
}
inline const Int64 operator*(const Int64 a, const Int64 b) {
	//TODO: safety
	return Int64{a.value * b.value};
}

inline const Int16 wrapAdd(const Int16 a, const Int16 b) {
	if (a.value < -999 || a.value > 999 || b.value < -999 || b.value > 999)
		todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
	return Int16{static_cast<int16_t>(a.value + b.value)};
}
inline const Int32 wrapAdd(const Int32 a, const Int32 b) {
	if (a.value < -999 || a.value > 999 || b.value < -999 || b.value > 999)
		todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
	return Int32{a.value + b.value};
}
inline const Int64 wrapAdd(const Int64 a, const Int64 b) {
	if (a.value < -9999999 || a.value > 9999999 || b.value < -9999999 || b.value > 9999999)
		todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
	return Int64{a.value + b.value};
}
inline const Int16 wrapSub(const Int16 a, const Int16 b) {
	if (a.value < -999 || a.value > 999 || b.value < -999 || b.value > 999)
		todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
	return Int16{static_cast<int16_t>(a.value - b.value)};
}
inline const Int32 wrapSub(const Int32 a, const Int32 b) {
	if (a.value < -999 || a.value > 999 || b.value < -999 || b.value > 999)
		todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
	return Int32{a.value - b.value};
}
inline const Int64 wrapSub(const Int64 a, const Int64 b) {
	if (a.value < -9999999 || a.value > 9999999 || b.value < -9999999 || b.value > 9999999)
		todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
	return Int64{a.value - b.value};
}

inline Comparison compareNat16(const Nat16 a, const Nat16 b) {
	return comparePrimitive(a.value, b.value);
}
inline Comparison compareNat32(const Nat32 a, const Nat32 b) {
	return comparePrimitive(a.value, b.value);
}
inline Comparison compareNat64(const Nat64 a, const Nat64 b) {
	return comparePrimitive(a.value, b.value);
}
inline Comparison compareInt16(const Int16 a, const Int16 b) {
	return comparePrimitive(a.value, b.value);
}
inline Comparison compareInt32(const Int32 a, const Int32 b) {
	return comparePrimitive(a.value, b.value);
}
inline Comparison compareInt64(const Int64 a, const Int64 b) {
	return comparePrimitive(a.value, b.value);
}

inline const Int64 int64FromNat64(const Nat64 n) {
	const uint64_t highestBit = (static_cast<uint64_t>(1) << 63);
	assert(!(n.value & highestBit));
	return Int64{static_cast<int64_t>(n.value)};
}

inline const Nat32 nat32FromNat64(const Nat64 n) {
	const uint64_t highestNat32 = (static_cast<uint64_t>(1) << 32) - 1;
	assert(n.value <= highestNat32);
	return Nat32{static_cast<uint32_t>(n.value)};
}

struct Float64 {
	double value;
};
//TODO: what is this used for?
using uint = uint32_t;

#define CHAR_BIT 8
static_assert(sizeof(Byte) * CHAR_BIT == 8, "byte");
static_assert(sizeof(Int64) * CHAR_BIT == 64, "int64");
static_assert(sizeof(Nat64) * CHAR_BIT == 64, "nat64");
static_assert(sizeof(Nat32) * CHAR_BIT == 32, "nat32");
static_assert(sizeof(Nat16) * CHAR_BIT == 16, "nat16");
static_assert(sizeof(Float64) * CHAR_BIT == 64, "float64");

const uint8_t MAX_UINT8 = 255;

inline size_t safeIntToSizeT(const int i) {
	assert(i >= 0);
	return static_cast<size_t>(i);
}

inline int safeSizeTToInt(const size_t s) {
	assert(s <= 99999);
	return s;
}

inline uint safeSizeTToUint(const size_t s) {
	assert(s <= 99999);
	return s;
}

inline size_t roundUp(const size_t a, const size_t b) {
	assert(b != 0);
	return a % b == 0 ? a : roundUp(a + 1, b);
}

inline void debugger() {}

template <typename T>
inline void unused(const T) {}

template <typename T>
inline constexpr T unreachable() {
	assert(0);
}

inline void debugPrint(const char* c) {
	puts(c);
}
