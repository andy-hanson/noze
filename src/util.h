#pragma once

#include <cassert>
#include <cstdint>
#include <cstdlib> // malloc
#include <new> // Support placement new
#include <cstdio> // printf
#include <type_traits> // remove_const

using byte = uint8_t;
using Int64 = int64_t;
using Nat64 = uint64_t;
using Float64 = double;

#define CHAR_BIT 8
static_assert(sizeof(byte) * CHAR_BIT == 8, "byte");
static_assert(sizeof(Int64) * CHAR_BIT == 64, "int64");
static_assert(sizeof(Nat64) * CHAR_BIT == 64, "nat64");
static_assert(sizeof(Float64) * CHAR_BIT == 64, "float64");

inline size_t max(const size_t a, const size_t b) {
	return a > b ? a : b;
}

inline size_t roundUp(const size_t a, const size_t b) {
	assert(b != 0);
	return a % b == 0 ? a : roundUp(a + 1, b);
}

struct RangeIter {
	size_t cur;

	inline size_t operator*() const {
		return cur;
	}

	inline void operator++() {
		cur++;
	}

	inline bool operator!=(const RangeIter other) const {
		return cur != other.cur;
	}
};

struct Range {
	const size_t lo;
	const size_t hi;

	inline Range(const size_t _lo, const size_t _hi) : lo{_lo}, hi{_hi} {
		assert(lo <= hi);
	}

	inline RangeIter begin() const {
		return RangeIter{lo};
	}

	inline RangeIter end() const {
		return RangeIter{hi};
	}
};

struct Arena {
	Arena() {
		begin = nullptr;
		cur = nullptr;
		end = nullptr;
	}

	Arena(const Arena& other) = delete;

	byte* begin;
	byte* cur;
	byte* end;

	void* alloc(const size_t n_bytes);

	template <typename T>
	struct Nu {
		Arena* arena;

		template <typename... Args>
		inline T* operator()(Args... args) {
			typename std::remove_const<T>::type* res = arena->newUninitialized<typename std::remove_const<T>::type>();
			return new (res) T{args...};
		}
	};

	template <typename T>
	inline Nu<T> nu() {
		return Nu<T>{this};
	}

	template <typename T>
	inline T* newUninitialized() {
		return static_cast<T*>(alloc(sizeof(T)));
	}
};

template <typename T>
struct Opt {
private:
	const bool _has;
	union {
		const T value;
	};

public:
	inline Opt() : _has{false} {}
	inline Opt(T v) : _has{true}, value{v} {}

	inline bool has() const {
		return _has;
	}

	inline T force() const {
		assert(has());
		return value;
	}
};

template <typename T>
using Eq = bool(*)(const T, const T);

template <typename T, Eq<T> eqValues>
bool optEq(const Opt<T> a, const Opt<T> b) {
	return a.has()
		? (b.has() && eqValues(a.force(), b.force()))
		: !b.has();
}

template <typename T>
inline Opt<T> none() {
	return Opt<T>{};
}

template <typename T>
inline Opt<T> some(T value) {
	return Opt<T>{value};
}

template <typename T>
void initMemory(const T& to, const T& from) {
	// T may contain const members, so do initialization by blitting
	byte* toByte = const_cast<byte*>(reinterpret_cast<const byte*>(&to));
	const byte* fromByte = reinterpret_cast<const byte*>(&from);
	for (const size_t i : Range{0, sizeof(T)})
		toByte[i] = fromByte[i];
}

template <typename T>
void overwriteConst(const T& to, T from) {
	initMemory(to, from);
}

template <size_t _size, typename T>
struct FixArr {
	T values[_size];

	inline const T* begin() const {
		return values;
	}

	inline const T* end() const {
		return values + _size;
	}

	inline size_t size() const {
		return _size;
	}

	inline const T& operator[](const size_t index) const {
		assert(index < _size);
		return values[index];
	}
};

template <typename T>
struct Arr {
	T* const _begin;
	const size_t size;

	inline const T* begin() const {
		return _begin;
	}

	inline const T* end() const {
		return _begin + size;
	}

	inline T& operator[](size_t index) {
		assert(index < size);
		return _begin[index];
	}

	inline const T& operator[](size_t index) const {
		assert(index < size);
		return _begin[index];
	}

	inline T* getPtr(size_t index) {
		assert(index < size);
		return _begin + index;
	}

	inline const T* getPtr(size_t index) const {
		assert(index < size);
		return _begin + index;
	}

	inline bool isEmpty() const {
		return size == 0;
	}

	static inline Arr<T> ofRange(T* const begin, T* const end) {
		assert(begin <= end);
		return Arr{begin, static_cast<size_t>(end - begin)};
	}

	inline const Arr<const T> asConst() const {
		return Arr<const T>{_begin, size};
	}
};

template <typename T>
inline const T& first(const Arr<T> a) {
	assert(!a.isEmpty());
	return a[0];
}

template <typename T>
inline const Arr<T> tail(const Arr<T> a) {
	assert(!a.isEmpty());
	return Arr<T>{a._begin + 1, a.size - 1};
}

template <typename T>
const T& last(const Arr<T> a) {
	assert(!a.isEmpty());
	return a[a.size - 1];
}

template <typename T>
struct PtrsIter {
	const T* ptr;

	const T* operator*() {
		return ptr;
	}

	void operator++() {
		++ptr;
	}

	bool operator!=(PtrsIter<T> other) const {
		return ptr != other.ptr;
	}
};

template <typename T>
struct PtrsRange {
	const T* _begin;
	const T* _end;

	inline PtrsIter<T> begin() const {
		return PtrsIter<T>{_begin};
	}

	inline PtrsIter<T> end() const {
		return PtrsIter<T>{_end};
	}
};

template <typename T>
PtrsRange<T> ptrsRange(const Arr<T> a) {
	return PtrsRange<T>{a.begin(), a.end()};
}

template <typename T>
inline Arr<T> emptyArr() {
	return Arr<T> { nullptr, 0 };
}

template <typename T, typename Cb>
inline bool exists(Arr<T> arr, Cb cb) {
	for (T x : arr) {
		const bool b = cb(x);
		if (b)
			return true;
	}
	return false;
}

template <typename T>
inline T only(Arr<T> a) {
	assert(a.size == 1);
	return a[0];
}

template <typename T>
struct MutSlice {
	bool isFrozen;
	T* begin;
	size_t size;

	inline MutSlice(T* const b, const size_t s)
		: isFrozen{false}, begin{b}, size{s} {}

	inline T& operator[](const size_t index) {
		assert(index < size);
		return begin[index];
	}

	inline const T& operator[](const size_t index) const {
		assert(index < size);
		return begin[index];
	}

	inline void set(const size_t index, const T value) {
		assert(index < size);
		assert(!isFrozen);
		overwriteConst(begin[index], value);
	}

	void copyFrom(const size_t index, const Arr<const T> arr) {
		assert(index + arr.size <= size);
		for (const size_t i : Range{0, arr.size})
			set(index + i, arr[i]);
	}

	inline Arr<T> freeze() {
		isFrozen = true;
		return tempAsArr();
	}

	inline const Arr<T> tempAsArr() const {
		return Arr<T>{begin, size};
	}
};

template <typename T>
inline MutSlice<T> newUninitializedMutSlice(Arena& arena, const size_t size) {
	return MutSlice<T>{static_cast<T*>(arena.alloc(sizeof(T) * size)), size};
}

template <typename T>
inline Arr<T> slice(Arr<T> a, size_t lo, size_t size) {
	assert(lo + size <= a.size);
	return Arr<T>{a._begin + lo, size};
}

template <typename T>
inline Arr<T> slice(Arr<T> a, size_t lo) {
	assert(lo <= a.size);
	return slice(a, lo, a.size - lo);
}

template <typename T>
struct MutArr {
private:
	MutSlice<T> slice;
	size_t _size;

public:
	inline MutArr() : slice{nullptr, 0}, _size{0} {}
	MutArr(const MutArr&) = delete;
	MutArr(MutArr&&) = default;
	MutArr(Arena& arena, T value) : slice{newUninitializedMutSlice<T>(arena, 1)}, _size{1} {
		initMemory(slice[0], value);
	}

	inline T& operator[](const size_t index) {
		return slice[index];
	}

	inline const T& operator[](const size_t index) const {
		return slice[index];
	}

	void set(const size_t index, const T value) {
		assert(index < _size);
		slice.set(index, value);
	}

	void push(Arena& arena, const T value) {
		if (_size == slice.size) {
			MutSlice<T> oldSlice = slice;
			slice = newUninitializedMutSlice<T>(arena, slice.size == 0 ? 4 : slice.size * 2);
			for (const size_t i : Range{0, oldSlice.size})
				initMemory(slice[i], oldSlice[i]);
		}

		assert(_size < slice.size);
		initMemory(slice[_size], value);
		_size++;
	}

	const Opt<T> pop() {
		if (isEmpty())
			return none<T>();
		else {
			_size--;
			return some<T>(slice[_size]);
		}
	}

	const T mustPop() {
		return pop().force();
	}

	const Opt<T> peek() const {
		return isEmpty()
			? none<T>()
			: some<T>(slice[_size - 1]);
	}

	inline size_t size() const {
		return _size;
	}

	inline bool isEmpty() const {
		return _size == 0;
	}

	inline Arr<T> freeze() {
		return ::slice<T>(slice.freeze(), 0, _size);
	}

	inline const Arr<T> tempAsArr() const {
		return ::slice<T>(slice.tempAsArr(), 0, _size);
	}

	void deleteAt(size_t index) {
		assert(index < _size);
		for (const size_t i : Range{index, _size - 1})
			overwriteConst(slice[i], slice[i + 1]);
		_size--;
	}
};

template <typename T, typename Pred>
void filterUnordered(MutArr<T>& a, Pred pred) {
	size_t i = 0;
	while (i < a.size()) {
		const bool b = pred(a[i]);
		if (b)
			i++;
		else if (i == a.size() - 1)
			a.mustPop();
		else {
			T t = a.mustPop();
			a.set(i, t);
		}
	}
}

template <typename T>
struct ArrBuilder {
private:
	MutArr<T> inner;
	ArrBuilder(const ArrBuilder<T>&) = delete;

public:
	inline ArrBuilder() : inner{} {}

	inline void add(Arena& arena, T value) {
		inner.push(arena, value);
	}

	inline bool isEmpty() const {
		return inner.isEmpty();
	}

	inline Arr<T> finish() {
		return inner.freeze();
	}

	inline const Arr<T> tempAsArr() const {
		return inner.tempAsArr();
	}

	inline size_t size() const {
		return inner.size();
	}
};

using Str = const Arr<const char>;
using NulTerminatedStr = Str;
using MutStr = MutSlice<const char>;

const Str copyStr(Arena& arena, const Str in);

bool strEq(const Str a, const Str b);
inline const Str strLiteral(const char* c) {
	const char* end = c;
	while (*end != '\0')
		++end;
	return Str{c, static_cast<size_t>(end - c)};
}
inline bool strEqLiteral(const Str s, const char* b) {
	return strEq(s, strLiteral(b));
}

template <typename T>
inline void unused(T&) {}
template <typename T, typename U>
inline void unused(T&, U&) {}
template <typename T, typename U, typename V>
inline void unused(T&, U&, V&) {}
template <typename T, typename U, typename V, typename W>
inline void unused(T&, U&, V&, W&) {}
template <typename T, typename U, typename V, typename W, typename X>
inline void unused(T&, U&, V&, W&, X&) {}
template <typename T, typename U, typename V, typename W, typename X, typename Y>
inline void unused(T&, U&, V&, W&, X&, Y&) {}

template <typename Success, typename Failure>
struct Result {
private:
	const bool _isSuccess;
	union {
		Success success;
		Failure failure;
	};

public:
	using SuccessType = Success;
	using FailureType = Failure;

	inline Result(const Success s) : _isSuccess{true}, success{s} {}
	inline Result(const Failure f) : _isSuccess{false}, failure{f} {}

	inline bool isSuccess() const {
		return _isSuccess;
	}

	inline Success asSuccess() const {
		assert(isSuccess());
		return success;
	}

	inline Failure asFailure() const {
		assert(!isSuccess());
		return failure;
	}
};

template <typename Success, typename Failure>
inline Result<Success, Failure> success(Success s) {
	return Result<Success, Failure>{s};
}

template <typename Success, typename Failure>
inline Result<Success, Failure> failure(Failure f) {
	return Result<Success, Failure>{f};
}

template <typename OutSuccess>
struct mapSuccess {
	template <typename InSuccess, typename Failure, typename Cb>
	inline const Result<OutSuccess, Failure> operator()(const Result<InSuccess, Failure> r, Cb cb) {
		return r.isSuccess()
			? success<OutSuccess, Failure>(cb(r.asSuccess()))
			: failure<OutSuccess, Failure>(r.asFailure());
	}
};

template <typename OutFailure>
struct mapFailure {
	template <typename Success, typename InFailure, typename Cb>
	inline const Result<Success, OutFailure> operator()(const Result<Success, InFailure> r, Cb cb) {
		return r.isSuccess()
			? success<Success, OutFailure>(r.asSuccess())
			: failure<Success, OutFailure>(cb(r.asFailure()));
	}
};

template <typename OutSuccess, typename Failure>
struct flatMapSuccess {
	template <typename InSuccess, typename Cb>
	inline const Result<OutSuccess, Failure> operator()(const Result<InSuccess, Failure> r, Cb cb) {
		return r.isSuccess()
			? cb(r.asSuccess())
			: failure<OutSuccess, Failure>(r.asFailure());
	}
};

template <typename T>
inline bool ptrEquals(T* a, T* b) {
	return a == b;
}

template <typename T>
inline T todo(const char* message) {
	printf("%s\n", message);
	throw message;
}

template <typename K, typename V>
struct KeyValuePair {
	K key;
	V value;
};

template <typename K, typename V, Eq<K> eq>
struct Dict {
	Arr<KeyValuePair<K, V>> pairs;

	const Opt<V> get(const K key) const {
		for (const KeyValuePair<K, V>& pair : pairs)
			if (eq(pair.key, key))
				return some<V>(pair.value);
		return none<V>();
	}

	inline const V mustGet(const K key) const {
		return get(key).force();
	}
};

template <typename K, typename V, Eq<K> eq>
struct DictBuilder {
private:
	ArrBuilder<KeyValuePair<K, V>> builder;

public:
	inline DictBuilder() : builder{} {}

	void add(Arena& arena, K key, V value) {
		builder.add(arena, KeyValuePair<K, V>{key, value});
	}

	template <typename CbConflict>
	Dict<K, V, eq> finish(Arena& arena, CbConflict cbConflict) {
		MutArr<KeyValuePair<K, V>> res;
		Arr<KeyValuePair<K, V>> allPairs = builder.finish();
		for (const size_t i : Range{0, allPairs.size}) {
			bool isConflict = false;
			for (const size_t j : Range{0, res.size()}) {
				if (eq(allPairs[i].key, res[j].key)) {
					cbConflict(allPairs[i].key, res[j].value, allPairs[i].value);
					isConflict = true;
					break;
				}
			}
			if (!isConflict)
				res.push(arena, allPairs[i]);
		}
		return Dict<K, V, eq>{res.freeze()};
	}
};

template <typename K, typename V, Eq<K> eq>
struct MultiDict {
	const Dict<K, const Arr<V>, eq> inner;

	const Arr<V> get(const K key) const {
		const Opt<const Arr<V>> res = inner.get(key);
		return res.has() ? res.force() : emptyArr<V>();
	}
};

template <typename T, Eq<T> eq>
struct MutSet {
private:
	MutArr<T> arr;

	MutSet(const MutSet&) = delete;

public:
	inline MutSet() : arr{} {}

	void add(Arena& arena, T value) {
		bool added = tryAdd(arena, value);
		assert(added);
	}

	bool tryAdd(Arena& arena, T value) {
		for (T t : arr.tempAsArr())
			if (eq(t, value))
				return false;
		arr.push(arena, value);
		return true;
	}
};

template <typename K, typename V, Eq<K> eq>
struct MutDict {
private:
	MutArr<KeyValuePair<K, V>> pairs;

public:
	inline bool has(const K key) const {
		return get(key).has();
	}

	const Opt<V> get(const K key) const {
		for (const KeyValuePair<K, V>& pair : pairs.tempAsArr())
			if (eq(pair.key, key))
				return some<V>(pair.value);
		return none<V>();
	}

	inline const V mustGet(const K key) const {
		return get(key).force();
	}

	void add(Arena& arena, const K key, const V value) {
		assert(!has(key));
		pairs.push(arena, KeyValuePair<K, V>{key, value});
	}

	template <typename GetValue>
	const V getOrAdd(Arena& arena, const K key, GetValue getValue) {
		for (const KeyValuePair<K, V>& pair : pairs.tempAsArr())
			if (eq(pair.key, key))
				return pair.value;

		const V value = getValue();
		pairs.push(arena, KeyValuePair<K, V>{key, value});
		return value;
	}

	const Opt<V> tryDeleteAndGet(const K key) {
		for (const size_t i : Range{0, pairs.size()})
			if (eq(pairs[i].key, key)) {
				const V res = pairs[i].value;
				pairs.deleteAt(i);
				return some<V>(res);
			}
		return none<V>();
	}

	const V mustDelete(const K key) {
		return tryDeleteAndGet(key).force();
	}
};

template <typename T>
struct Late {
private:
	bool _isSet;
	union {
		T value;
	};
public:
	Late(const Late&) = delete;
	inline Late(Late&&) = default;
	inline Late() {}
	inline Late(const T _value) : value{_value} {}

	inline bool isSet() const {
		return _isSet;
	}

	inline const T& get() const {
		assert(_isSet);
		return value;
	}

	inline void set(T v) {
		assert(!_isSet);
		initMemory(value, v);
		_isSet = true;
	}

	inline void setOverwrite(T v) {
		assert(_isSet);
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

template <typename T>
inline T unreachable() {
	assert(0);
}

inline ssize_t safeSizeTToSSizeT(const size_t s) {
	assert(s <= 9999);
	return static_cast<ssize_t>(s);
}

inline uint safeSizeTToUint(const size_t s) {
	assert(s <= 9999);
	return s;
}

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
	for (const size_t i : Range{0, a.size})
		res.set(i, a[i]);
	for (const size_t i : Range{0, b.size})
		res.set(a.size + i, b[i]);
	return res.freeze();
}

template <typename T>
Arr<T> prepend(Arena& arena, const T a, const Arr<T> b) {
	MutSlice<T> res = newUninitializedMutSlice<T>(arena, 1 + b.size);
	res.set(0, a);
	for (const size_t i : Range{0, b.size})
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
		for (const size_t i : Range{0, size})
			initMemory(out[i], cb(i));
		return Arr<Out>{out, size};
	}
};

template <typename Out>
struct fillArrOrFail {
	template <typename Cb>
	const Opt<const Arr<Out>> operator()(Arena& arena, const size_t size, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * size));
		for (const size_t i : Range{0, size}) {
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
		for (const size_t i : Range{0, in.size})
			initMemory(out[i], cb(in[i]));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapPointers {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{0, in.size})
			initMemory(out[i], cb(in.getPtr(i)));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapWithIndex {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{0, in.size})
			initMemory(out[i], cb(in[i], i));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapOrNone {
	template <typename In, typename Cb>
	const Opt<const Arr<Out>> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (const size_t i : Range{0, in.size}) {
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
	const Result<const Arr<OutSuccess>, OutFailure> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		OutSuccess* out = static_cast<OutSuccess*>(arena.alloc(sizeof(OutSuccess) * in.size));
		for (const size_t i : Range{0, in.size}) {
			const Result<OutSuccess, OutFailure> result = cb(in[i]);
			if (result.isSuccess())
				initMemory(out[i], result.asSuccess());
			else
				return failure<const Arr<OutSuccess>, OutFailure>(result.asFailure());
		}
		return success<const Arr<OutSuccess>, OutFailure>(Arr<OutSuccess>{out, in.size});
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
		for (const size_t i : Range{0, size}) {
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
		for (const size_t in_i : Range{0, in.size}) {
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
		for (const size_t i : Range{0, size})
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
		for (const size_t i : Range{0, size})
			initMemory(out[i], cb(in0.getPtr(i), in1.getPtr(i)));
		return Arr<Out>{out, size};
	}
};

template <typename T, typename U, typename Cb>
void zip(Arr<T> a, Arr<U> b, Cb cb) {
	assert(a.size == b.size);
	for (const size_t i : Range{0, a.size})
		cb(a[i], b[i]);
}

template <typename T, typename U, typename Cb>
bool eachCorresponds(const Arr<T> a, const Arr<U> b, Cb cb) {
	assert(a.size == b.size);
	for (const size_t i : Range{0, a.size}) {
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

template <typename K, typename V, Eq<K> eq>
struct buildDict {
	template <typename T, typename GetPair, typename OnConflict>
	Dict<K, V, eq> operator()(Arena& arena, const Arr<T> inputs, GetPair getPair, OnConflict onConflict) {
		MutArr<KeyValuePair<K, V>> res;
		for (const T& input : inputs) {
			KeyValuePair<K, V> pair = getPair(input);
			bool wasConflict = false;
			for (const size_t i : Range{0, res.size()})
				if (eq(res[i].key, pair.key)) {
					onConflict(pair.key, res[i].value, pair.value);
					wasConflict = true;
					break;
				}
			if (!wasConflict)
				res.push(arena, pair);
		}
		return Dict<K, V, eq>{res.freeze()};
	}
};

template <typename K, typename V, Eq<K> eq>
struct buildMultiDict {
	template <typename T, typename GetPair>
	MultiDict<K, V, eq> operator()(Arena& arena, const Arr<T> inputs, GetPair getPair) {
		MutArr<KeyValuePair<K, MutArr<V>>> res {};
		for (const T& input : inputs) {
			KeyValuePair<K, V> pair = getPair(input);
			bool didAdd = false;
			for (const size_t i : Range{0, res.size()})
				if (eq(res[i].key, pair.key)) {
					res[i].value.push(arena, pair.value);
					didAdd = true;
					break;
				}
			if (!didAdd)
				res.push(arena, KeyValuePair<K, MutArr<V>>{pair.key, MutArr<V>{arena, pair.value}});
		}
		const Arr<KeyValuePair<K, MutArr<V>>> arr = res.freeze();
		const Arr<KeyValuePair<K, const Arr<V>>> pairs = map<KeyValuePair<K, const Arr<V>>>{}(arena, arr, [](KeyValuePair<K, MutArr<V>>& m) {
			return KeyValuePair<K, const Arr<V>>{m.key, m.value.freeze()};
		});
		return MultiDict<K, V, eq>{Dict<K, const Arr<V>, eq>{pairs}};
	}
};
