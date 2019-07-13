#pragma once

#include <cstddef> // size_t
#include <new> // Support placement new
#include <type_traits> // remove_const

#include "./types.h"

using byte = uint8_t;

struct Arena {
	byte* begin;
	byte* cur;
	byte* end;

	inline Arena() : begin{nullptr}, cur{nullptr}, end{nullptr} {}
	Arena(const Arena* other) = delete;
	~Arena();

};

void* alloc(Arena* arena, const size_t n_bytes);

template <typename T>
inline T* newUninitialized(Arena* arena) {
	return static_cast<T*>(alloc(arena, sizeof(T)));
}

template <typename T>
struct nu {
	template <typename... Args>
	inline T* operator()(Arena* arena, Args... args) {
		typename std::remove_const<T>::type* res = newUninitialized<typename std::remove_const<T>::type>(arena);
		return new (res) T{args...};
	}
};
