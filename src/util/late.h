#pragma once

#include "./bool.h"
#include "./cell.h"
#include "./memory.h"

template <typename T>
struct Late {
private:
	Cell<const Bool> _isSet;
	union {
		T value;
	};
public:
	Late(const Late&) = delete;
	inline Late(Late&&) = default;
	inline Late() : _isSet{False} {}
	inline Late(const T _value) : _isSet{True}, value{_value} {}

	inline const Bool isSet() const {
		return _isSet.get();
	}

	inline const T& get() const {
		assert(_isSet.get());
		return value;
	}

	inline void set(T v) {
		assert(!_isSet.get());
		initMemory(value, v);
		_isSet.set(True);
	}

	inline void setOverwrite(T v) {
		assert(_isSet.get());
		overwriteConst(value, v);
	}
};

template <typename T, typename Cb>
inline const T lazilySet(Late<T>& late, Cb cb) {
	if (late.isSet())
		return late.get();
	else {
		const T value = cb();
		late.set(value);
		return value;
	}
}
