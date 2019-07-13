#pragma once

template <typename T, Cmp<T> cmp>
struct MutSet {
private:
	MutArr<T> arr;

	MutSet(const MutSet&) = delete;

public:
	inline MutSet() : arr{} {}

	const Bool has(const T value) const {
		for (T t : tempAsArr(arr))
			if (cmp(t, value) == Comparison::equal)
				return True;
		return False;
	}

	void add(Arena* arena, const T value) {
		const Bool added = tryAdd(arena, value);
		assert(added);
	}

	const Bool tryAdd(Arena* arena, const T value) {
		const Bool h = has(value);
		if (_not(h))
			push<T>(arena, arr, value);
		return _not(h);
	}
};
