#pragma once

#include "./mutArr.h"

template <typename T>
struct ArrBuilder {
	MutArr<T> inner;
	ArrBuilder(const ArrBuilder<T>&) = delete;
	inline ArrBuilder() : inner{} {}
};

template <typename T>
inline const Arr<T> arrBuilderTempAsArr(const ArrBuilder<T>* a) {
	return tempAsArr(&a->inner);
}

template <typename T>
inline size_t arrBuilderSize(const ArrBuilder<T>* builder) {
	return mutArrSize(&builder->inner);
}

template <typename T>
inline void add(Arena* arena, ArrBuilder<T>* builder, T value) {
	push<T>(arena, &builder->inner, value);
}

template <typename T>
const Arr<T> finishArr(ArrBuilder<T>* builder) {
	return freeze(&builder->inner);
}

template <typename T>
inline const Bool arrBuilderIsEmpty(const ArrBuilder<T>* a) {
	return mutArrIsEmpty(&a->inner);
}

