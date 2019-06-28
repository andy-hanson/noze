#pragma once

#include "./mutArr.h"

template <typename T>
struct ArrBuilder {
	MutArr<T> inner;
	ArrBuilder(const ArrBuilder<T>&) = delete;

	inline ArrBuilder() : inner{} {}

	inline void add(Arena& arena, T value) {
		push<T>(arena, inner, value);
	}


	inline Arr<T> finish() {
		return freeze(inner);
	}

	inline const Arr<T> tempAsArr() const {
		return ::tempAsArr(inner);
	}

	inline size_t size() const {
		return inner.size();
	}
};

template <typename T>
inline const Bool isEmpty(const ArrBuilder<T>& a) {
	return isEmpty(a.inner);
}

