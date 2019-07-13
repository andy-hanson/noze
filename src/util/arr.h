#pragma once

#include <cstddef> // size_t

#include "./bool.h"
#include "./comparison.h"
#include "./range.h"

template <typename T>
struct Arr {
	T* const _begin;
	const size_t _size;

	inline constexpr Arr(T* const begin, const size_t size) : _begin{begin}, _size{size} {
		assert(size < 1000000); // sanity check
	}

	inline const T* begin() const {
		return _begin;
	}
	inline T* begin() {
		return _begin;
	}

	inline const T* end() const {
		return _begin + _size;
	}
	inline T* end() {
		return _begin + _size;
	}
};

template <typename T>
inline constexpr size_t size(const Arr<T> a) {
	return a._size;
}

template <typename T>
inline const Bool isEmpty(const Arr<T> a) {
	return eq(size(a), static_cast<size_t>(0));
}

template <typename T, typename U>
inline const Bool sizeEq(const Arr<T> a, const Arr<U> b) {
	return eq(size(a), size(b));
}

template <typename T>
inline constexpr T at(const Arr<T> a, const size_t index) {
	assert(index < size(a));
	return a._begin[index];
}

template <typename T>
inline T* getPtr(const Arr<T> a, const size_t index) {
	assert(index < size(a));
	return a._begin + index;
}

template <typename T>
inline Arr<T> emptyArr() {
	return Arr<T> { nullptr, 0 };
}

using Str = const Arr<const char>;

template <typename T, Cmp<T> cmp>
Comparison compareArr(const Arr<T> a, const Arr<T> b) {
	const Comparison sizeCmp = comparePrimitive(size(a), size(b));
	if (sizeCmp != Comparison::equal)
		return sizeCmp;
	else {
		for (const size_t i : Range{size(a)}) {
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
