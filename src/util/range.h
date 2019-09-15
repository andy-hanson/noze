#pragma once

#include "./bool.h"
#include "./types.h"

struct RangeIter {
	size_t cur;

	inline constexpr size_t operator*() const {
		return cur;
	}

	inline constexpr void operator++() {
		cur++;
	}

	inline constexpr const Bool operator!=(const RangeIter other) const {
		return neq(cur, other.cur);
	}
};

struct Range {
	const size_t lo;
	const size_t hi;

	inline constexpr explicit Range(const size_t _hi) : lo{0}, hi{_hi} {
		assert(lo <= hi);
	}
	inline constexpr Range(const size_t _lo, const size_t _hi) : lo{_lo}, hi{_hi} {
		assert(lo <= hi);
	}

	inline constexpr RangeIter begin() const {
		return RangeIter{lo};
	}

	inline constexpr RangeIter end() const {
		return RangeIter{hi};
	}
};

template <typename Cb>
void repeat(size_t n, Cb cb) {
	while (n != 0) {
		cb();
		n--;
	}
}

struct RangeDownIter {
	int64_t cur;

	inline size_t operator*() const {
		assert(cur >= 0);
		return static_cast<size_t>(cur);
	}

	inline void operator++() {
		cur--;
	}

	inline const Bool operator!=(const RangeDownIter other) const {
		return neq(cur, other.cur);
	}
};

struct RangeDown {
	const size_t n;

	inline RangeDownIter begin() const {
		return RangeDownIter{static_cast<int64_t>(n - 1)};
	}

	inline RangeDownIter end() const {
		return RangeDownIter{-1};
	}
};
