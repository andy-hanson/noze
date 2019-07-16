#pragma once

#include <cassert>
#include <cstdint>
#include <stdio.h>

using CStr = const char*;
using byte = uint8_t;
using _void = uint8_t;
using Int64 = int64_t;
using Nat64 = uint64_t;
using Float64 = double;
using uint = uint32_t;

#define CHAR_BIT 8
static_assert(sizeof(byte) * CHAR_BIT == 8, "byte");
static_assert(sizeof(Int64) * CHAR_BIT == 64, "int64");
static_assert(sizeof(Nat64) * CHAR_BIT == 64, "nat64");
static_assert(sizeof(Float64) * CHAR_BIT == 64, "float64");

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

template <typename T>
inline T todo(const char* message) {
	// printf("TODO: %s\n", message);
	throw message;
}

inline void debugger() {}

template <typename T>
inline void unused(const T) {}

template <typename T>
inline T unreachable() {
	assert(0);
}

inline void debugPrint(const char* c) {
	puts(c);
}
