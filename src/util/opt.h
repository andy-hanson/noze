#pragma once

#include <cassert>

#include "./comparison.h"

template <typename T>
struct Opt {
private:
	const Bool _has;
	union {
		const T value;
	};

public:
	inline Opt() : _has{False} {}
	inline Opt(T v) : _has{True}, value{v} {}

	inline const Bool has() const {
		return _has;
	}

	inline const T& force() const {
		assert(has());
		return value;
	}
};

template <typename T>
inline Opt<T> none() {
	return Opt<T>{};
}

template <typename T>
inline Opt<T> some(T value) {
	return Opt<T>{value};
}

template <typename T>
inline T forceOrTodo(const Opt<T> opt) {
	if (opt.has())
		return opt.force();
	else
		assert(0);
}

template <typename T, Cmp<T> cmpValues>
Comparison compareOpt(const Opt<T> a, const Opt<T> b) {
	return a.has()
		? b.has()
			// some <=> some
			? cmpValues(a.force(), b.force())
			// some > none
			: Comparison::greater
		: b.has()
			// none < some
			? Comparison::less
			// none == none
			: Comparison::equal;
}
