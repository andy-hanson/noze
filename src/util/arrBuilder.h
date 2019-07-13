#pragma once

#include "./mutArr.h"

template <typename T>
struct ArrBuilder {
	MutArr<T> inner;
	ArrBuilder(const ArrBuilder<T>&) = delete;
	inline ArrBuilder() : inner{} {}

	inline const Arr<T> tempAsArr() const {
		return ::tempAsArr(inner);
	}

	inline size_t size() const {
		return inner.size();
	}
};

template <typename T>
inline void add(Arena* arena, ArrBuilder<T>* builder, T value) {
	push<T>(arena, builder->inner, value);
}

template <typename T>
const Arr<T> finishArr(ArrBuilder<T>* builder) {
	return freeze(builder->inner);
}

template <typename T>
inline const Bool isEmpty(const ArrBuilder<T>& a) {
	return isEmpty(a.inner);
}

