#pragma once

#include "./memory.h"

template <typename T>
struct Cell {
	T _value;
	Cell(const Cell&) = delete;
	Cell(Cell&&) = default;
	inline Cell(T v) : _value{v} {}
};

template <typename T>
inline const T cellGet(const Cell<T>* c) {
	return c->_value;
}

template <typename T>
inline void cellSet(Cell<T>* c, const T value) {
	overwriteConst(&c->_value, value);
}
