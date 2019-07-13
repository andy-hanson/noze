#pragma once

#include "./bool.h"
#include "./cell.h"
#include "./memory.h"

template <typename T>
struct Late {
	Cell<const Bool> _isSet;
	union {
		T _value;
	};
	Late(const Late&) = delete;
	inline Late() : _isSet{False} {}
	inline Late(const T value) : _isSet{True}, _value{value} {}
};

template <typename T>
inline const Bool lateIsSet(const Late<T>* late) {
	return cellGet(&late->_isSet);
}

template <typename T>
inline const T& lateGet(const Late<T>* late) {
	assert(lateIsSet(late));
	return late->_value;
}

template <typename T>
inline void lateSet(Late<T>* late, T v) {
	assert(!lateIsSet(late));
	initMemory(late->_value, v);
	cellSet<const Bool>(&late->_isSet, True);
}

template <typename T>
inline void lateSetOverwrite(Late<T>* late, T v) {
	assert(lateIsSet(late));
	overwriteConst(late->_value, v);
}

template <typename T, typename Cb>
inline const T lazilySet(Late<T>* late, Cb cb) {
	if (lateIsSet(late))
		return lateGet(late);
	else {
		const T value = cb();
		lateSet<T>(late, value);
		return value;
	}
}
