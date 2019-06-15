#pragma once

#include "./resultUtil.h"

template <typename T, typename Cb>
const Opt<T> find(Arr<T> a, Cb cb) {
	for (T t : a)
		if (cb(t))
			return some<T>(t);
	return none<T>();
}

template <typename T, typename Cb>
const Opt<T*> findPtr(Arr<T> a, Cb cb) {
	for (T* ptr : ptrsRange(a))
		if (cb(ptr))
			return some<T*>(ptr);
	return none<T*>();
}

template <typename T>
Arr<T> cat(Arena& arena, const Arr<T> a, const Arr<T> b) {
	MutSlice<T> res = newUninitializedMutSlice<T>(arena, a.size + b.size);
	for (const size_t i : Range{a.size})
		res.set(i, a[i]);
	for (const size_t i : Range{b.size})
		res.set(a.size + i, b[i]);
	return res.freeze();
}

template <typename T>
Arr<T> prepend(Arena& arena, const T a, const Arr<T> b) {
	MutSlice<T> res = newUninitializedMutSlice<T>(arena, 1 + b.size);
	res.set(0, a);
	for (const size_t i : Range{b.size})
		res.set(1 + i, b[i]);
	return res.freeze();
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
			initMemory(out[i], cb(in[i]));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapPointers {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{in.size})
			initMemory(out[i], cb(in.getPtr(i)));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapWithIndex {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{in.size})
			initMemory(out[i], cb(in[i], i));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapOrNone {
	template <typename In, typename Cb>
	const Opt<const Arr<Out>> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{in.size}) {
			const Opt<Out> o = cb(in[i]);
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
			const Result<OutSuccess, OutFailure> result = cb(in[0]);
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
		for (ssize_t i = in.size - 1; i >= 0; i--) {
			const Result<OutSuccess, OutFailure> result = cb(in[i]);
			if (result.isSuccess())
				initMemory(out[i], result.asSuccess());
			else
				return failure<const Arr<OutSuccess>, OutFailure>(result.asFailure());
		}
		return success<const Arr<OutSuccess>, OutFailure>(Arr<OutSuccess>{out, in.size});
	}
};

template <typename Out>
struct zipOrFail {
	template <typename In0, typename In1, typename Cb>
	const Opt<const Arr<Out>> operator()(Arena& arena, const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
		const size_t size = in0.size;
		assert(in1.size == size);
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * size));
		for (const size_t i : Range{size}) {
			const Opt<Out> o = cb(in0[i], in1[i]);
			if (o.has())
				initMemory(out[i], o.force());
			else
				return none<const Arr<Out>>();
		}
		return some<const Arr<Out>>(Arr<Out>{out, size});
	}
};

template <typename Out>
struct mapOpWithIndex {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		size_t out_i = 0;
		for (const size_t in_i : Range{in.size}) {
			Opt<Out> op = cb(in[in_i], in_i);
			if (op.has()) {
				initMemory(out[out_i], op.force());
				out_i++;
			}
		}
		assert(out_i <= in.size);
		return Arr<Out>{out, out_i};
	}
};

template <typename Out>
struct mapZip {
	template <typename In0, typename In1, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In0> in0, const Arr<In1> in1, Cb cb) {
		const size_t size = in0.size;
		assert(in1.size == size);
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * size));
		for (const size_t i : Range{size})
			initMemory(out[i], cb(in0[i], in1[i]));
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
			initMemory(out[i], cb(in0.getPtr(i), in1.getPtr(i)));
		return Arr<Out>{out, size};
	}
};

template <typename T, typename U, typename Cb>
void zip(Arr<T> a, Arr<U> b, Cb cb) {
	assert(a.size == b.size);
	for (const size_t i : Range{a.size})
		cb(a[i], b[i]);
}

template <typename T, typename U, typename Cb>
bool eachCorresponds(const Arr<T> a, const Arr<U> b, Cb cb) {
	assert(a.size == b.size);
	for (const size_t i : Range{a.size}) {
		const bool thisCorresponds = cb(a[i], b[i]);
		if (!thisCorresponds)
			return false;
	}
	return true;
}

template <typename T, Eq<T> eq>
bool arrEq(const Arr<T> a, const Arr<T> b) {
	return eachCorresponds(a, b, eq);
}

template <typename T, typename Cb>
bool every(const Arr<T> a, Cb cb) {
	for (const T& t : a) {
		const bool b = cb(t);
		if (!b)
			return false;
	}
	return true;
}

template <typename T>
const Str copyStr(Arena& arena, const Arr<T> in) {
	return map<T>{}(arena, in, [](T x) { return x; });
}

// Each of the returned arrays has elements considered equivalent by `cmp`
template <typename T, typename Cmp>
const Arr<const Arr<T>> sortAndGroup(Arena& arena, const Arr<T> a, Cmp cmp) {
	MutArr<MutArr<T>> res;

	auto addSingle = [&](const T x) -> void {
		for (const size_t i : Range{res.size()}) {
			// Each in res[i] should be equivalent, so just use 0th
			const Comparison c = cmp(x, res[i][0]);
			switch (c) {
				case Comparison::less:
					todo<void>("insert a new arr here");
					return;
				case Comparison::equal:
					res[i].push(arena, x);
					return;
				case Comparison::greater:
					break;
			}
		}
	};

	for (const T x : a)
		addSingle(x);

	return map<const Arr<T>>{}(arena, res.freeze(), [&](MutArr<T>& m) {
		return m.freeze();
	});
}
