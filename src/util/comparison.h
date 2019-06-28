#pragma once

#include "./bool.h"

enum class Comparison {
	less,
	equal,
	greater,
};

template <typename T>
using Eq = const Bool(*)(const T, const T);

template <typename T>
using Cmp = Comparison(*)(const T, const T);

template <typename T>
inline Comparison comparePrimitive(const T a, const T b) {
	static_assert(std::is_fundamental<T>::value || std::is_enum<T>::value, "must be primitive");
	return a < b ? Comparison::less : a > b ? Comparison::greater : Comparison::equal;
}

inline Comparison compareBool(const Bool a, const Bool b) {
	return comparePrimitive(a.b, b.b);
}

template <typename T>
inline const Bool ptrEquals(const T* a, const T* b) {
	return Bool{a == b};
}

template <typename T>
inline Comparison comparePointer(const T* a, const T* b) {
	return a < b ? Comparison::less : a >  b ? Comparison::greater : Comparison::equal;
}

template <typename T>
inline const T max(const T a, const T b) {
	static_assert(std::is_fundamental<T>::value, "must be primitive");
	return a > b ? a : b;
}
