#pragma once

#include <cstddef> // size_t

#include "./bool.h"
#include "./comparison.h"
#include "./range.h"

template <typename T>
struct Arr {
	T* const _begin;
	const size_t size;

	inline constexpr Arr(T* const begin, const size_t _size) : _begin{begin}, size{_size} {
		assert(size < 1000000); // sanity check
	}

	inline const T* begin() const {
		return _begin;
	}
	inline T* begin() {
		return _begin;
	}

	inline const T* end() const {
		return _begin + size;
	}
	inline T* end() {
		return _begin + size;
	}
};

template <typename T>
inline constexpr T at(const Arr<T> a, const size_t index) {
	assert(index < a.size);
	return a._begin[index];
}

template <typename T>
inline T* getPtr(const Arr<T> a, const size_t index) {
	assert(index < a.size);
	return a._begin + index;
}

template <typename T>
inline Arr<T> emptyArr() {
	return Arr<T> { nullptr, 0 };
}

template <typename T>
inline const Bool isEmpty(const Arr<T> a) {
	return eq(a.size, static_cast<size_t>(0));
}

using Str = const Arr<const char>;

template <typename T, Cmp<T> cmp>
Comparison compareArr(const Arr<T> a, const Arr<T> b) {
	const Comparison sizeCmp = comparePrimitive(a.size, b.size);
	if (sizeCmp != Comparison::equal)
		return sizeCmp;
	else {
		for (const size_t i : Range{a.size}) {
			const Comparison c = cmp(at(a, i), at(b, i));
			if (c != Comparison::equal)
				return c;
		}
		return Comparison::equal;
	}
}

template <typename T, typename Cb>
inline const Bool exists(const Arr<T> arr, Cb cb) {
	for (T x : arr) {
		const Bool b = cb(x);
		if (b)
			return True;
	}
	return False;
}
