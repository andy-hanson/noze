#pragma once

#include "./memory.h"

template <typename T>
struct Cell {
private:
	T value;
public:
	Cell(const Cell&) = delete;
	Cell(Cell&&) = default;
	inline Cell(T v) : value{v} {}
	inline const T& get() const {
		return value;
	}
	inline void set(T v) {
		overwriteConst(value, v);
	}
};
