#pragma once

#include <initializer_list>

#include "./cell.h"
#include "./mutArr.h"
#include "./result.h"
#include "./resultUtil.h"

template <typename T>
const Arr<T> arrOfRange(T* const begin, T* const end) {
	assert(begin <= end);
	return Arr<T>{begin, static_cast<size_t>(end - begin)};
}

template <typename T>
inline const T first(const Arr<T> a) {
	assert(!isEmpty(a));
	return at(a, 0);
}

template <typename T>
inline T only(const Arr<T> a) {
	assert(size(a) == 1);
	return first(a);
}

template <typename T>
inline const T* onlyPtr(const Arr<T> a) {
	assert(size(a) == 1);
	return ptrAt(a, 0);
}

template <typename T, typename U, typename Cb>
inline const T fold(const T start, const Arr<U> a, Cb cb) {
	Cell<const T> cell = Cell<const T>{start};
	for (const U x : a)
		cellSet<const T>(&cell, cb(cellGet(&cell), x));
	return cellGet<const T>(&cell);
}

template <typename T, typename Cb>
inline size_t sum(const Arr<T> a, Cb cb) {
	return fold(0, a, [&](const size_t l, const T t) {
		return l + cb(t);
	});
}

template <typename T, Cmp<T> cmp>
struct arrMax {
	template <typename U, typename Cb>
	T recur(const T acc, const Arr<U> a, const Cb cb) {
		return isEmpty(a)
			? acc
			: recur<U, Cb>(max<T, cmp>(acc, cb(first(a))), tail(a), cb);
	}

	template <typename U, typename Cb>
	T operator()(const Arr<U> a, const Cb cb) {
		return recur(cb(first(a)), tail(a), cb);
	}
};

template <typename T, Cmp<T> cmp>
struct arrMaxIndex {
	template <typename U, typename Cb>
	size_t recur(const size_t indexOfMax, const T maxValue, const Arr<U> a, size_t index, const Cb cb) {
		if (index == size(a))
			return indexOfMax;
		else {
			const T valueHere = cb(at(a, index), index);
			return cmp(valueHere, maxValue) == Comparison::greater
				? recur<U, Cb>(index, valueHere, a, index + 1, cb)
				: recur<U, Cb>(indexOfMax, maxValue, a, index + 1, cb);
		}
	}

	template <typename U, typename Cb>
	size_t operator()(const Arr<U> a, const Cb cb) {
		return recur(0, cb(first(a), 0), a, 1, cb);
	}
};

template <typename T>
struct PtrsIter {
	T* ptr;

	T* operator*() const {
		return ptr;
	}

	void operator++() {
		++ptr;
	}

	const Bool operator!=(PtrsIter<T> other) const {
		return Bool{ptr != other.ptr};
	}
};

template <typename T>
struct PtrsRange {
	T* const _begin;
	T* const _end;

	inline PtrsIter<T> begin() const {
		return PtrsIter<T>{_begin};
	}

	inline PtrsIter<T> end() const {
		return PtrsIter<T>{_end};
	}
};

template <typename T>
PtrsRange<T> ptrsRange(Arr<T> a) {
	return PtrsRange<T>{a.begin(), a.end()};
}

template <typename T>
inline const Arr<T> slice(const Arr<T> a, const size_t lo, const size_t sliceSize) {
	assert(lo + sliceSize <= size(a));
	return Arr<T>{a._begin + lo, sliceSize};
}

template <typename T>
inline const Arr<T> slice(const Arr<T> a, const size_t lo) {
	assert(lo <= size(a));
	return slice(a, lo, size(a) - lo);
}

template <typename T>
inline const Arr<T> sliceFromTo(const Arr<T> a, const size_t lo, const size_t hi) {
	assert(lo <= hi);
	return slice(a, lo, hi - lo);
}

template <typename T, typename Cb>
const Opt<T> find(Arr<T> a, Cb cb) {
	for (T t : a)
		if (cb(t))
			return some<T>(t);
	return none<T>();
}

template <typename T>
inline Range indices(const Arr<T> a) {
	return Range{size(a)};
}

template <typename T, typename Cb>
const Opt<const size_t> findIndex(Arr<T> a, Cb cb) {
	for (const size_t i : indices(a))
		if (cb(at(a, i)))
			return some<const size_t>(i);
	return none<const size_t>();
}

template <typename T, typename Cb>
const Opt<T*> findPtr(Arr<T> a, Cb cb) {
	for (T* ptr : ptrsRange(a))
		if (cb(ptr))
			return some<T*>(ptr);
	return none<T*>();
}

template <typename T>
inline const Arr<const T> asConstArr(const Arr<T> a) {
	return Arr<const T>{a._begin, size(a)};
}

template <typename T>
inline const Arr<T> tail(const Arr<T> a) {
	assert(!isEmpty(a));
	return slice(a, 1, size(a) - 1);
}

template <typename T>
inline const Arr<T> rtail(const Arr<T> a) {
	assert(!isEmpty(a));
	return slice(a, 0, size(a) - 1);
}

template <typename T>
const T last(const Arr<T> a) {
	assert(!isEmpty(a));
	return at(a, size(a) - 1);
}

template <typename T>
Arr<T> cat(Arena* arena, const Arr<T> a, const Arr<T> b) {
	MutArr<T> res = newUninitializedMutArr<T>(arena, size(a) + size(b));
	copyFrom(&res, 0, a);
	copyFrom(&res, size(a), b);
	return freeze(&res);
}

template <typename T>
Arr<T> cat(Arena* arena, const Arr<T> a, const Arr<T> b, const Arr<T> c) {
	MutArr<T> res = newUninitializedMutArr<T>(arena, size(a) + size(b) + size(c));
	copyFrom(&res, 0, a);
	copyFrom(&res, size(a), b);
	copyFrom(&res, size(a) + size(b), c);
	return freeze(&res);
}

template <typename T>
Arr<T> prepend(Arena* arena, const T a, const Arr<T> b) {
	MutArr<T> res = newUninitializedMutArr<T>(arena, 1 + size(b));
	setAt<T>(&res, 0, a);
	for (const size_t i : Range{size(b)})
		setAt<T>(&res, 1 + i, at(b, i));
	return freeze(&res);
}

template<typename T>
inline Arr<T> singleElementArr(Arena* arena, T a) {
	T* out = static_cast<T*>(alloc(arena, sizeof(T) * 1));
	initMemory(&out[0], a);
	return Arr<T>{out, 1};
}


template <typename T>
inline Arr<T> arrLiteral(Arena* arena, std::initializer_list<T> list) {
	const size_t size = list.size();
	T* out = static_cast<T*>(alloc(arena, sizeof(T) * size));
	const T* lBegin = list.begin();
	assert(lBegin + size == list.end());
	for (const size_t i : Range{size})
		initMemory(&out[i], lBegin[i]);
	return Arr<T>{out, size};
}

template <typename Out>
struct fillArr {
	template <typename Cb>
	const Arr<Out> operator()(Arena* arena, const size_t size, Cb cb) {
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * size));
		for (const size_t i : Range{size})
			initMemory(&out[i], cb(i));
		return Arr<Out>{out, size};
	}
};

template <typename Out>
struct fillArrOrFail {
	template <typename Cb>
	const Opt<const Arr<Out>> operator()(Arena* arena, const size_t size, Cb cb) {
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * size));
		for (const size_t i : Range{size}) {
			const Opt<Out> op = cb(i);
			if (has(op))
				initMemory(&out[i], force(op));
			else
				return none<const Arr<Out>>();
		}
		return some<const Arr<Out>>(Arr<Out>{out, size});
	}
};

template <typename Out>
struct map {
	template <typename In, typename Cb>
	const Arr<Out> operator()(Arena* arena, Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * size(in)));
		for (const size_t i : Range{size(in)})
			initMemory(&out[i], cb(at(in, i)));
		return Arr<Out>{out, size(in)};
	}
};

template <typename Out>
struct mapPtrs {
	template <typename In, typename Cb>
	const Arr<Out> operator()(Arena* arena, Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * size(in)));
		for (const size_t i : Range{size(in)})
			initMemory(&out[i], cb(ptrAt(in, i)));
		return Arr<Out>{out, size(in)};
	}
};

template <typename Out>
struct mapWithIndex {
	template <typename In, typename Cb>
	const Arr<Out> operator()(Arena* arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * size(in)));
		for (const size_t i : Range{size(in)})
			initMemory(&out[i], cb(at(in, i), i));
		return Arr<Out>{out, size(in)};
	}
};

template <typename Out>
struct mapOrNone {
	template <typename In, typename Cb>
	const Opt<const Arr<Out>> operator()(Arena* arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * size(in)));
		for (const size_t i : Range{size(in)}) {
			const Opt<Out> o = cb(at(in, i));
			if (has(o))
				initMemory(&out[i], force(o));
			else
				return none<const Arr<Out>>();
		}
		return some<const Arr<Out>>(Arr<Out>{out, size(in)});
	}
};

template <typename Out>
struct mapPtrsOrNone {
	template <typename In, typename Cb>
	const Opt<const Arr<Out>> operator()(Arena* arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * size(in)));
		for (const size_t i : Range{size(in)}) {
			const Opt<Out> o = cb(ptrAt(in, i));
			if (has(o))
				initMemory(&out[i], force(o));
			else
				return none<const Arr<Out>>();
		}
		return some<const Arr<Out>>(Arr<Out>{out, size(in)});
	}
};

template <typename OutSuccess, typename OutFailure>
struct mapOrFail {
	template <typename In, typename Cb>
	const Result<const _void, OutFailure> worker(OutSuccess* out, const Arr<In> in, Cb cb) {
		if (isEmpty(in))
			return success<const _void, OutFailure>(_void{0});
		else {
			const Result<OutSuccess, OutFailure> result = cb(first(in));
			return flatMapSuccess<const _void, OutFailure>{}(result, [&](const OutSuccess s) {
				initMemory(out, s);
				return worker(out + 1, tail(in), cb);
			});
		}
	}

	template <typename In, typename Cb>
	const Result<const Arr<OutSuccess>, OutFailure> operator()(Arena* arena, const Arr<In> in, Cb cb) {
		OutSuccess* out = static_cast<OutSuccess*>(alloc(arena, sizeof(OutSuccess) * size(in)));
		const Result<const _void, OutFailure> res = worker(out, in, cb);
		return mapSuccess<const Arr<OutSuccess>>{}(res, [&](const _void) {
			return Arr<OutSuccess>{out, size(in)};
		});
	}
};

// NOTE: Performs the mapping in reverse -- does not return reversed output!
template <typename OutSuccess, typename OutFailure>
struct mapOrFailReverse {
	template <typename In, typename Cb>
	const Result<const Arr<OutSuccess>, OutFailure> operator()(Arena* arena, const Arr<In> in, Cb cb) {
		OutSuccess* out = static_cast<OutSuccess*>(alloc(arena, sizeof(OutSuccess) * size(in)));
		for (const size_t i : RangeDown{size(in)}) {
			const Result<OutSuccess, OutFailure> result = cb(at(in, i));
			if (result.isSuccess())
				initMemory(&out[i], result.asSuccess());
			else
				return failure<const Arr<OutSuccess>, OutFailure>(result.asFailure());
		}
		return success<const Arr<OutSuccess>, OutFailure>(Arr<OutSuccess>{out, size(in)});
	}
};

template <typename In0, typename In1, typename Cb>
const Bool zipSome(const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
	const size_t sz = size(in0);
	assert(size(in1) == sz);
	for (const size_t i : Range{sz})
		if (cb(at(in0, i), at(in1, i)))
			return True;
	return False;
}

template <typename Out>
struct zipOrFail {
	template <typename In0, typename In1, typename Cb>
	const Opt<const Arr<Out>> operator()(Arena* arena, const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
		const size_t sz = size(in0);
		assert(size(in1) == sz);
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * sz));
		for (const size_t i : Range{sz}) {
			const Opt<Out> o = cb(at(in0, i), at(in1, i));
			if (has(o))
				initMemory(&out[i], force(o));
			else
				return none<const Arr<Out>>();
		}
		return some<const Arr<Out>>(Arr<Out>{out, sz});
	}
};

template <typename Out>
struct mapOp {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena* arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * size(in)));
		size_t out_i = 0;
		for (const size_t in_i : Range{size(in)}) {
			Opt<Out> op = cb(at(in, in_i));
			if (has(op)) {
				initMemory(&out[out_i], force(op));
				out_i++;
			}
		}
		assert(out_i <= size(in));
		return Arr<Out>{out, out_i};
	}
};

template <typename T, typename Cb>
const Arr<T> filter(Arena* arena, const Arr<T> a, Cb cb) {
	return mapOp<T>{}(arena, a, [&](const T t) {
		return cb(t) ? some<T>(t) : none<T>();
	});
}

template <typename Out>
struct mapZip {
	template <typename In0, typename In1, typename Cb>
	const Arr<Out> operator()(Arena* arena, const Arr<In0> in0, const Arr<In1> in1, const Cb cb) {
		const size_t sz = size(in0);
		assert(size(in1) == sz);
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * sz));
		for (const size_t i : Range{sz})
			initMemory(&out[i], cb(at(in0, i), at(in1, i)));
		return Arr<Out>{out, sz};
	}
};

template <typename Out>
struct mapZipOrNone {
	template <typename In0, typename In1, typename Cb>
	const Opt<const Arr<Out>> operator()(Arena* arena, const Arr<In0> in0, const Arr<In1> in1, const Cb cb) {
		const size_t sz = size(in0);
		assert(size(in1) == sz);
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * sz));
		for (const size_t i : Range{sz}) {
			const Opt<Out> x = cb(at(in0, i), at(in1, i));
			if (has(x))
				initMemory(&out[i], force(x));
			else
				return none<const Arr<Out>>();
		}
		return some<const Arr<Out>>(Arr<Out>{out, sz});
	}
};

template <typename Out>
struct mapZipWithIndex {
	template <typename In0, typename In1, typename Cb>
	Arr<Out> operator()(Arena* arena, const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
		const size_t sz = size(in0);
		assert(size(in1) == sz);
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * sz));
		for (const size_t i : Range{sz})
			initMemory(&out[i], cb(at(in0, i), at(in1, i), i));
		return Arr<Out>{out, sz};
	}
};

template <typename Out>
struct mapZipPtrs {
	template <typename In0, typename In1, typename Cb>
	Arr<Out> operator()(Arena* arena, const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
		const size_t sz = size(in0);
		assert(size(in1) == sz);
		Out* out = static_cast<Out*>(alloc(arena, sizeof(Out) * sz));
		for (const size_t i : Range{sz})
			initMemory(&out[i], cb(ptrAt(in0, i), ptrAt(in1, i)));
		return Arr<Out>{out, sz};
	}
};

template <typename T, typename U, typename Cb>
void zip(Arr<T> a, Arr<U> b, Cb cb) {
	assert(sizeEq(a, b));
	for (const size_t i : Range{size(a)})
		cb(at(a, i), at(b, i));
}

template <typename T, typename U, typename Cb>
void zipPtrs(Arr<T> a, Arr<U> b, Cb cb) {
	assert(sizeEq(a, b));
	for (const size_t i : Range{size(a)})
		cb(ptrAt(a, i), ptrAt(b, i));
}

template <typename T, typename U, typename Cb>
void zipWithIndex(Arr<T> a, Arr<U> b, Cb cb) {
	assert(sizeEq(a, b));
	for (const size_t i : Range{size(a)})
		cb(at(a, i), at(b, i), i);
}

template <typename T, typename U, typename Cb>
const Bool eachCorresponds(const Arr<T> a, const Arr<U> b, Cb cb) {
	assert(sizeEq(a, b));
	for (const size_t i : Range{size(a)}) {
		const Bool thisCorresponds = cb(at(a, i), at(b, i));
		if (!thisCorresponds)
			return False;
	}
	return True;
}

template <typename T, Eq<T> eq>
const Bool arrEq(const Arr<T> a, const Arr<T> b) {
	return _and(sizeEq(a, b), eachCorresponds(a, b, eq));
}

template <typename T, typename Cb>
const Bool every(const Arr<T> a, Cb cb) {
	for (const T t : a) {
		const Bool b = cb(t);
		if (!b)
			return False;
	}
	return True;
}

template <typename T>
const Arr<T> copyArr(Arena* arena, const Arr<T> in) {
	return map<T>{}(arena, in, [](T x) { return x; });
}

// Each of the returned arrays has elements considered equivalent by `cmp`
template <typename T, typename Cmp>
const Arr<const Arr<T>> sortAndGroup(Arena* arena, const Arr<T> a, Cmp cmp) {
	MutArr<MutArr<T>> res;

	auto addSingle = [&](const T x) -> void {
		for (const size_t i : Range{mutArrSize(&res)}) {
			// Each in res[i] should be equivalent, so just use 0th
			const Comparison c = cmp(x, mutArrFirst(mutArrPtrAt(&res, i)));
			switch (c) {
				case Comparison::less:
					todo<void>("insert a new arr here");
					return;
				case Comparison::equal:
					push<T>(arena, mutArrPtrAt(&res, i), x);
					return;
				case Comparison::greater:
					break;
			}
		}
		// Greater than everything in the list -- add it to the end
		push(arena, &res, mutArrOfOneElement<T>(arena, x));
	};

	for (const T x : a)
		addSingle(x);

	const Arr<const Arr<T>> arrRes = mapPtrs<const Arr<T>>{}(arena, freeze(&res), [&](MutArr<T>* m) {
		return freeze(m);
	});

	// Check that result size == input size
	size_t resultSize = 0;
	for (const Arr<T> group : arrRes)
		resultSize += size(group);
	assert(resultSize == size(a));
	return arrRes;
}

template <typename T, typename Pred>
void filterUnordered(MutArr<T>* a, Pred pred) {
	size_t i = 0;
	while (i < mutArrSize(a)) {
		const Bool b = pred(mutArrPtrAt(a, i));
		if (b)
			i++;
		else if (i == mutArrSize(a) - 1)
			mustPop(a);
		else {
			T t = mustPop(a);
			setAt(a, i, t);
		}
	}
}

template <typename T>
void copyFrom(MutArr<T>* m, const size_t index, const Arr<T> arr) {
	assert(index + size(arr) <= mutArrSize(m));
	for (const size_t i : Range{size(arr)})
		setAt<T>(m, index + i, at(arr, i));
}

template <typename T, Eq<T> eq>
inline const Bool contains(const Arr<T> arr, const T t) {
	return exists(arr, [&](const T value) {
		return eq(value, t);
	});
}
