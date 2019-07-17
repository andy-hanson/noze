#pragma once

#include "./arena.h"
#include "./arr.h"
#include "./memory.h" // initMemory
#include "./opt.h"
#include "./range.h"

template <typename T>
struct MutArr {
	bool isFrozen;
	T* _begin;
	size_t capacity;
	size_t _size;

	inline MutArr() : isFrozen{false}, _begin{nullptr}, capacity{0}, _size{0} {}
	inline MutArr(bool _isFrozen, T* begin, size_t _capacity, size_t size)
		: isFrozen{_isFrozen}, _begin{begin}, capacity{_capacity}, _size{size} {}
	MutArr(const MutArr&) = delete;
	MutArr(MutArr&&) = default;
	inline MutArr(T* begin, const size_t len) : isFrozen{false}, _begin{begin}, capacity{len}, _size{len} {}

};

template <typename T>
inline T* mutArrBegin(MutArr<T>* m) {
	return m->_begin;
}

template <typename T>
inline MutArr<T> mutArrOfOneElement(Arena* arena, const T value) {
	return MutArr<T>{false, nu<T>{}(arena, value), 1, 1};
}

template <typename T>
inline size_t mutArrSize(const MutArr<T>* m) {
	return m->_size;
}

template <typename T>
inline const T mutArrAt(const MutArr<T>* m, const size_t index) {
	assert(index < m->_size);
	return m->_begin[index];
}

template <typename T>
inline T* mutArrPtrAt(MutArr<T>* m, const size_t index) {
	assert(index < m->_size);
	return &m->_begin[index];
}

template <typename T>
inline const T mutArrFirst(const MutArr<T>* m) {
	return mutArrAt(m, 0);
}

template <typename T>
inline void setAt(MutArr<T>* m, const size_t index, const T value) {
	assert(!m->isFrozen);
	assert(index < m->_size);
	overwriteConst(&m->_begin[index], value);
}

template <typename T>
void push(Arena* arena, MutArr<T>* m, const T value) {
	if (m->_size == m->capacity) {
		T* oldBegin = m->_begin;
		m->capacity = m->_size == 0 ? 4 : m->_size * 2;
		m->_begin = static_cast<T*>(alloc(arena, sizeof(T) * m->capacity));
		for (const size_t i : Range{m->_size})
			initMemoryFromPtr(&m->_begin[i], &oldBegin[i]);
	}
	assert(m->_size < m->capacity);
	initMemoryFromPtr(&m->_begin[m->_size], &value);
	m->_size++;
}

template <typename T>
const Opt<T> pop(MutArr<T>* m) {
	if (m->_size == 0)
		return none<T>();
	else {
		m->_size--;
		return some<T>(m->_begin[m->_size]);
	}
}

template <typename T>
inline const T mustPop(MutArr<T>* m) {
	return force(pop(m));
}

template <typename T>
inline const Opt<T> peek(const MutArr<T>* m) {
	return m->_size == 0
		? none<T>()
		: some<T>(m->_begin[m->_size - 1]);
}

template <typename T>
inline const T mustPeek(const MutArr<T>* m) {
	return force(peek(m));
}

template <typename T>
inline const Arr<T> freeze(MutArr<T>* m) {
	m->isFrozen = true;
	return tempAsArr(m);
}

template <typename T>
inline const Arr<T> tempAsArr(const MutArr<T>* m) {
	return Arr<T>{m->_begin, m->_size};
}

template <typename T>
void deleteAt(MutArr<T>& m, const size_t index) {
	assert(index < m._size);
	for (const size_t i : Range{index, m._size - 1})
		overwriteConst(&m._begin[i], m._begin[i + 1]);
	m._size--;
}

template <typename T>
inline MutArr<T> newUninitializedMutArr(Arena* arena, const size_t size) {
	return MutArr<T>{static_cast<T*>(alloc(arena, sizeof(T) * size)), size};
}

template <typename T>
inline const Bool mutArrIsEmpty(const MutArr<T>* a) {
	return eq(mutArrSize(a), static_cast<size_t>(0));
}

using MutStr = MutArr<const char>;
