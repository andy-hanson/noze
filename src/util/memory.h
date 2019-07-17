#pragma once

#include <cstdint>

#include "./range.h"

template <typename T>
void initMemoryFromPtr(const T* to, const T* from) {
	uint8_t* toByte = const_cast<uint8_t*>(reinterpret_cast<const uint8_t*>(to));
	const uint8_t* fromByte = reinterpret_cast<const uint8_t*>(from);
	for (const size_t i : Range{sizeof(T)})
		#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
		toByte[i] = fromByte[i];
}

template <typename T>
void initMemory(const T* to, const T from) {
	// T may contain const members, so do initialization by blitting
	initMemoryFromPtr(to, &from);
}

template <typename T>
void overwriteConst(const T* to, const T from) {
	initMemory(to, from);
}
