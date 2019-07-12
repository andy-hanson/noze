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
	~Arena();

	Arena(const Arena& other) = delete;

	void* alloc(const size_t n_bytes);

	template <typename T>
	struct Nu {
		Arena* arena;

		template <typename... Args>
		inline T* operator()(Args... args) {
			typename std::remove_const<T>::type* res = arena->newUninitialized<typename std::remove_const<T>::type>();
			return new (res) T{args...};
		}
	};

	template <typename T>
	inline Nu<T> nu() {
		return Nu<T>{this};
	}

	template <typename T>
	inline T* newUninitialized() {
		return static_cast<T*>(alloc(sizeof(T)));
	}
};
