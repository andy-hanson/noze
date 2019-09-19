#pragma once

template <typename T>
struct MutSlice {
	T* _begin;
	size_t _size;
};

template <typename T>
inline size_t size(const MutSlice<T> m) {
	return m._size;
}

template <typename T>
inline T mutSliceAt(const MutSlice<T> m, const size_t index) {
	assert(index < size(m));
	return m._begin[index];
}

template <typename T>
inline void mutSliceSetAt(MutSlice<T> m, const size_t index, const T value) {
	assert(index < size(m));
	m._begin[index] = value;
}

template <typename T>
inline MutSlice<T> mutSlice(MutSlice<T> m, const size_t lo, const size_t newSize) {
	assert(lo + newSize <= size(m));
	return MutSlice<T>{m._begin + lo, newSize};
}

template <typename T>
inline MutSlice<T> mutSlice(MutSlice<T> m, const size_t lo) {
	assert(lo <= size(m));
	return mutSlice(m, lo, size(m) - lo);
}

template <typename T>
inline MutSlice<T> newUninitializedMutSlice(Arena* arena, const size_t size) {
	return MutSlice<T>{static_cast<T*>(alloc(arena, sizeof(T) * size)), size};
}

template <typename T>
inline const Arr<T> tempAsArr(const MutSlice<T> m) {
	return Arr<T>{m._begin, m._size};
}

template <typename T>
inline void mutSliceFill(const MutSlice<T> m, const T value) {
	for (size_t i : Range{size(m)})
		mutSliceSetAt(m, i, value);
}

