#pragma once

#include <initializer_list>

#include "./mutArr.h"
#include "./result.h"
#include "./resultUtil.h"

template <typename T>
const Arr<T> arrOfRange(T* const begin, T* const end) {
	assert(begin <= end);
	return Arr<T>{begin, static_cast<size_t>(end - begin)};
}

template <typename T>
inline T only(const Arr<T> a) {
	assert(a.size == 1);
	return at(a, 0);
}

template <typename T>
struct PtrsIter {
	T* ptr;

	T* operator*() {
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
inline const Arr<T> slice(const Arr<T> a, const size_t lo, const size_t size) {
	assert(lo + size <= a.size);
	return Arr<T>{a._begin + lo, size};
}

template <typename T>
inline const Arr<T> slice(const Arr<T> a, const size_t lo) {
	assert(lo <= a.size);
	return slice(a, lo, a.size - lo);
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

template <typename T, typename Cb>
const Opt<const size_t> findIndex(Arr<T> a, Cb cb) {
	//TODO:EACHWITHRANGE
	for (const size_t i : Range{a.size})
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
	return Arr<const T>{a._begin, a.size};
}

template <typename T>
inline const T& first(const Arr<T> a) {
	assert(!isEmpty(a));
	return at(a, 0);
}

template <typename T>
inline const Arr<T> tail(const Arr<T> a) {
	assert(!isEmpty(a));
	return slice(a, 1, a.size - 1);
}

template <typename T>
inline const Arr<T> rtail(const Arr<T> a) {
	assert(!isEmpty(a));
	return slice(a, 0, a.size - 1);
}

template <typename T>
const T& last(const Arr<T> a) {
	assert(!isEmpty(a));
	return at(a, a.size - 1);
}

template <typename T>
const T& last(const MutArr<T>& a) {
	assert(!isEmpty(a));
	return at(a, a.size() - 1);
}

template <typename T>
Arr<T> cat(Arena& arena, const Arr<T> a, const Arr<T> b) {
	MutArr<T> res = newUninitializedMutArr<T>(arena, a.size + b.size);
	copyFrom(res, 0, a);
	copyFrom(res, a.size, b);
	return freeze(res);
}

template <typename T>
Arr<T> cat(Arena& arena, const Arr<T> a, const Arr<T> b, const Arr<T> c) {
	MutArr<T> res = newUninitializedMutArr<T>(arena, a.size + b.size + c.size);
	copyFrom(res, 0, a);
	copyFrom(res, a.size, b);
	copyFrom(res, a.size + b.size, c);
	return freeze(res);
}

template <typename T>
Arr<T> cat(Arena& arena, const Arr<T> a, const Arr<T> b, const Arr<T> c, const Arr<T> d) {
	MutArr<T> res = newUninitializedMutArr<T>(arena, a.size + b.size + c.size);
	copyFrom(res, 0, a);
	copyFrom(res, a.size, b);
	copyFrom(res, a.size + b.size, c);
	copyFrom(res, a.size + b.size + c.size, d);
	return freeze(res);
}

template <typename T>
Arr<T> prepend(Arena& arena, const T a, const Arr<T> b) {
	MutArr<T> res = newUninitializedMutArr<T>(arena, 1 + b.size);
	setAt<T>(res, 0, a);
	for (const size_t i : Range{b.size})
		setAt<T>(res, 1 + i, at(b, i));
	return freeze(res);
}

template<typename T>
inline Arr<T> arrLiteral(Arena& arena, T a) {
	T* out = static_cast<T*>(arena.alloc(sizeof(T) * 1));
	initMemory(out[0], a);
	return Arr<T>{out, 1};
}

template<typename T>
inline Arr<T> arrLiteral(Arena& arena, T a, T b) {
	T* out = static_cast<T*>(arena.alloc(sizeof(T) * 2));
	initMemory(out[0], a);
	initMemory(out[1], b);
	return Arr<T>{out, 2};
}

template <typename T>
inline Arr<T> arrLiteral(Arena& arena, T a, T b, T c) {
	T* out = static_cast<T*>(arena.alloc(sizeof(T) * 3));
	initMemory(out[0], a);
	initMemory(out[1], b);
	initMemory(out[2], c);
	return Arr<T>{out, 3};
}

template <typename T>
inline Arr<T> arrLiteral(Arena& arena, T a, T b, T c, T d) {
	return arrLiteral(arena, { a, b, c, d });
}

template <typename T>
inline Arr<T> arrLiteral(Arena& arena, std::initializer_list<T> list) {
	const size_t size = list.size();
	T* out = static_cast<T*>(arena.alloc(sizeof(T) * size));
	T* lBegin = list.begin();
	assert(lBegin + size == list.end());
	for (const size_t i : Range{size})
		initMemory(out[i], lBegin[i]);
	return Arr<T>{out, size};
}

template <typename Out>
struct fillArr {
	template <typename Cb>
	const Arr<Out> operator()(Arena& arena, const size_t size, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * size));
		for (const size_t i : Range{size})
			initMemory(out[i], cb(i));
		return Arr<Out>{out, size};
	}
};

template <typename Out>
struct fillArrOrFail {
	template <typename Cb>
	const Opt<const Arr<Out>> operator()(Arena& arena, const size_t size, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * size));
		for (const size_t i : Range{size}) {
			const Opt<Out> op = cb(i);
			if (op.has())
				initMemory(out[i], op.force());
			else
				return none<const Arr<Out>>();
		}
		return some<const Arr<Out>>(Arr<Out>{out, size});
	}
};

template <typename Out>
struct map {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{in.size})
			initMemory(out[i], cb(at(in, i)));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapPointers {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{in.size})
			initMemory(out[i], cb(getPtr(in, i)));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapWithIndex {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{in.size})
			initMemory(out[i], cb(at(in, i), i));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapOrNone {
	template <typename In, typename Cb>
	const Opt<const Arr<Out>> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{in.size}) {
			const Opt<Out> o = cb(at(in, i));
			if (o.has())
				initMemory(out[i], o.force());
			else
				return none<const Arr<Out>>();
		}
		return some<const Arr<Out>>(Arr<Out>{out, in.size});
	}
};

template <typename OutSuccess, typename OutFailure>
struct mapOrFail {
	template <typename In, typename Cb>
	const Result<const _void, OutFailure> worker(OutSuccess* out, const Arr<In> in, Cb& cb) {
		if (isEmpty(in))
			return success<const _void, OutFailure>(0);
		else {
			const Result<OutSuccess, OutFailure> result = cb(first(in));
			return flatMapSuccess<const _void, OutFailure>{}(result, [&](const OutSuccess s) {
				initMemory(*out, s);
				return worker(out + 1, tail(in), cb);
			});
		}
	}

	template <typename In, typename Cb>
	const Result<const Arr<OutSuccess>, OutFailure> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		OutSuccess* out = static_cast<OutSuccess*>(arena.alloc(sizeof(OutSuccess) * in.size));
		const Result<const _void, OutFailure> res = worker(out, in, cb);
		return mapSuccess<const Arr<OutSuccess>>{}(res, [&](const _void) {
			return Arr<OutSuccess>{out, in.size};
		});
	}
};

template <typename OutSuccess, typename OutFailure>
struct mapOrFailReverse {
	template <typename In, typename Cb>
	const Result<const Arr<OutSuccess>, OutFailure> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		OutSuccess* out = static_cast<OutSuccess*>(arena.alloc(sizeof(OutSuccess) * in.size));
		for (const size_t i : RangeDown{in.size}) {
			const Result<OutSuccess, OutFailure> result = cb(at(in, i));
			if (result.isSuccess())
				initMemory(out[i], result.asSuccess());
			else
				return failure<const Arr<OutSuccess>, OutFailure>(result.asFailure());
		}
		return success<const Arr<OutSuccess>, OutFailure>(Arr<OutSuccess>{out, in.size});
	}
};

template <typename In0, typename In1, typename Cb>
const Bool zipSome(const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
	const size_t size = in0.size;
	assert(in1.size == size);
	for (const size_t i : Range{size})
		if (!cb(at(in0, i), at(in1, i)))
			return False;
	return True;
}

template <typename Out>
struct zipOrFail {
	template <typename In0, typename In1, typename Cb>
	const Opt<const Arr<Out>> operator()(Arena& arena, const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
		const size_t size = in0.size;
		assert(in1.size == size);
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * size));
		for (const size_t i : Range{size}) {
			const Opt<Out> o = cb(at(in0, i), at(in1, i));
			if (o.has())
				initMemory(out[i], o.force());
			else
				return none<const Arr<Out>>();
		}
		return some<const Arr<Out>>(Arr<Out>{out, size});
	}
};

template <typename Out>
struct mapOp {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		size_t out_i = 0;
		for (const size_t in_i : Range{in.size}) {
			Opt<Out> op = cb(at(in, in_i));
			if (op.has()) {
				initMemory(out[out_i], op.force());
				out_i++;
			}
		}
		assert(out_i <= in.size);
		return Arr<Out>{out, out_i};
	}
};

template <typename T, typename Cb>
const Arr<T> filter(Arena& arena, const Arr<T> a, Cb cb) {
	return mapOp<T>{}(arena, a, [&](const T t) {
		return cb(t) ? some<T>(t) : none<T>();
	});
}

template <typename Out>
struct mapZip {
	template <typename In0, typename In1, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
		const size_t size = in0.size;
		assert(in1.size == size);
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * size));
		for (const size_t i : Range{size})
			initMemory(out[i], cb(at(in0, i), at(in1, i)));
		return Arr<Out>{out, size};
	}
};

template <typename Out>
struct mapZipWithIndex {
	template <typename In0, typename In1, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
		const size_t size = in0.size;
		assert(in1.size == size);
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * size));
		for (const size_t i : Range{size})
			initMemory(out[i], cb(at(in0, i), at(in1, i), i));
		return Arr<Out>{out, size};
	}
};

template <typename Out>
struct mapZipPtrs {
	template <typename In0, typename In1, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
		const size_t size = in0.size;
		assert(in1.size == size);
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * size));
		for (const size_t i : Range{size})
			initMemory(out[i], cb(getPtr(in0, i), getPtr(in1, i)));
		return Arr<Out>{out, size};
	}
};

template <typename T, typename U, typename Cb>
void zip(Arr<T> a, Arr<U> b, Cb cb) {
	assert(a.size == b.size);
	for (const size_t i : Range{a.size})
		cb(at(a, i), at(b, i));
}

template <typename T, typename U, typename Cb>
void zipPtrs(Arr<T> a, Arr<U> b, Cb cb) {
	assert(a.size == b.size);
	for (const size_t i : Range{a.size})
		cb(getPtr(a, i), getPtr(b, i));
}

template <typename T, typename U, typename Cb>
void zipWithIndex(Arr<T> a, Arr<U> b, Cb cb) {
	assert(a.size == b.size);
	for (const size_t i : Range{a.size})
		cb(at(a, i), at(b, i), i);
}

template <typename T, typename U, typename Cb>
const Bool eachCorresponds(const Arr<T> a, const Arr<U> b, Cb cb) {
	assert(a.size == b.size);
	for (const size_t i : Range{a.size}) {
		const Bool thisCorresponds = cb(at(a, i), at(b, i));
		if (!thisCorresponds)
			return False;
	}
	return True;
}

template <typename T, Eq<T> eq>
const Bool arrEq(const Arr<T> a, const Arr<T> b) {
	return _and(a.size == b.size, eachCorresponds(a, b, eq));
}

template <typename T, typename Cb>
const Bool every(const Arr<T> a, Cb cb) {
	for (const T& t : a) {
		const Bool b = cb(t);
		if (!b)
			return False;
	}
	return True;
}

template <typename T>
const Arr<T> copyArr(Arena& arena, const Arr<T> in) {
	return map<T>{}(arena, in, [](T x) { return x; });
}

// Each of the returned arrays has elements considered equivalent by `cmp`
template <typename T, typename Cmp>
const Arr<const Arr<T>> sortAndGroup(Arena& arena, const Arr<T> a, Cmp cmp) {
	MutArr<MutArr<T>> res;

	auto addSingle = [&](const T x) -> void {
		for (const size_t i : Range{res.size()}) {
			// Each in res[i] should be equivalent, so just use 0th
			const Comparison c = cmp(x, first(at(res, i)));
			switch (c) {
				case Comparison::less:
					todo<void>("insert a new arr here");
					return;
				case Comparison::equal:
					push<T>(arena, at(res, i), x);
					return;
				case Comparison::greater:
					break;
			}
		}
		// Greater than everything in the list -- add it to the end
		push(arena, res, MutArr<T>{arena, x});
	};

	for (const T x : a)
		addSingle(x);

	const Arr<const Arr<T>> arrRes = map<const Arr<T>>{}(arena, freeze(res), [&](MutArr<T>& m) {
		return freeze(m);
	});

	// Check that result size == input size
	size_t resultSize = 0;
	for (const Arr<T> a : arrRes)
		resultSize += a.size;
	assert(resultSize == a.size);
	return arrRes;
}

template <typename T, typename Pred>
void filterUnordered(MutArr<T>& a, Pred pred) {
	size_t i = 0;
	while (i < a.size()) {
		const Bool b = pred(at(a, i));
		if (b)
			i++;
		else if (i == a.size() - 1)
			mustPop(a);
		else {
			T t = mustPop(a);
			setAt(a, i, t);
		}
	}
}

template <typename T>
void copyFrom(MutArr<T>& m, const size_t index, const Arr<T> arr) {
	assert(index + arr.size <= m.size());
	for (const size_t i : Range{arr.size})
		setAt<T>(m, index + i, at(arr, i));
}

template <typename T>
struct reverse_iter {
	const T* ptr;

	inline const T& operator*() const {
		return *ptr;
	}

	inline void operator++() {
		ptr--;
	}

	inline const Bool operator!=(const reverse_iter<T> other) {
		return ptr != other.ptr;
	}
};

template <typename T>
struct Reverse {
	const Arr<T> a;

	inline reverse_iter<T> begin() const {
		return reverse_iter<T>{a.end() - 1};
	}

	inline reverse_iter<T> end() const {
		return reverse_iter<T>{a.begin() - 1};
	}
};

// Used to enable type inference
template <typename T>
inline Reverse<T> reverse(const Arr<T> a) {
	return Reverse<T>{a};
}

template <typename T, Eq<T> eq>
inline const Bool contains(const Arr<T>& arr, const T t) {
	return exists(arr, [&](const T value) {
		return eq(value, t);
	});
}
