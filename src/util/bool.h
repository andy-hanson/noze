#pragma once

#include <type_traits> // std::is_fundamental

struct Bool {
	bool b;

	inline constexpr explicit Bool(bool _b) : b{_b} {}
	// This is necessary to use Bool in 'if' statements.
	// Unfortunately, this makes it implicitly convert to int too!
	// (and size_t, and float, and basically everything...)
	inline constexpr operator bool() const {
		return b;
	}
};

static const Bool True { true };
static const Bool False { false };

#define _not(a) Bool{!a}
#define _and(a, b) Bool{a && b}
#define _and3(a, b, c) Bool{a && b && c}
#define _or(a, b) Bool{a || b}
#define _or3(a, b, c) Bool{a || b || c}
#define _or4(a, b, c, d) Bool{a || b || c || d}

template <typename T>
inline const Bool enumEq(const T a, const T b) {
	static_assert(std::is_enum<T>::value && !std::is_convertible<T, int>::value, "must be enum class");
	return Bool{a == b};
}

inline bool boolToNat(const Bool b) {
	return b ? 1 : 0;
}

template <typename T>
inline constexpr const Bool eq(const T a, const T b) {
	static_assert(std::is_fundamental<T>::value, "must be primitive");
	return Bool{a == b};
}

template <typename T>
inline constexpr const Bool neq(const T a, const T b) {
	static_assert(std::is_fundamental<T>::value, "must be primitive");
	return Bool{a != b};
}

template <typename T>
inline const Bool gt(const T a, const T b) {
	static_assert(std::is_fundamental<T>::value, "must be primitive");
	return Bool{a > b};
}
