#pragma once

#include <cassert>

#include "./comparison.h"

template <typename T>
struct Opt {
	const Bool _has;
	union {
		const Bool ignore;
		const T _value;
	};
};

template <typename T>
inline Opt<T> none() {
	return Opt<T>{False, {.ignore=False}};
}

template <typename T>
inline Opt<T> some(T value) {
	return Opt<T>{True, {._value=value}};
}

template <typename T>
inline const Bool has(const Opt<T> o) {
	return o._has;
}

template <typename T>
inline const T force(const Opt<T> o) {
	assert(has(o));
	return o._value;
}

template <typename T>
inline const T* forcePtr(const Opt<T>* o) {
	assert(has(*o));
	return &o->_value;
}

template <typename T>
inline T forceOrTodo(const Opt<T> opt) {
	if (has(opt))
		return force(opt);
	else
		assert(0);
}

template <typename T, Cmp<T> cmpValues>
Comparison compareOpt(const Opt<T> a, const Opt<T> b) {
	return has(a)
		? has(b)
			// some <=> some
			? cmpValues(force(a), force(b))
			// some > none
			: Comparison::greater
		: has(b)
			// none < some
			? Comparison::less
			// none == none
			: Comparison::equal;
}

template <typename T, typename Cb>
inline T optOr(const Opt<T> a, const Cb cb) {
	return has(a) ? force(a) : cb();
}

template <typename Out>
struct mapOption {
	template <typename In, typename Cb>
	const Opt<Out> operator()(const Opt<In> a, const Cb cb) {
		return has(a)
			? some<Out>(cb(force(a)))
			: none<Out>();
	}
};
