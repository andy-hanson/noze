 #pragma once

#include <cassert>
#include <cstdio> // printf
#include <cstdint>

#include "./util/arena.h"

using _void = uint8_t;
using Int64 = int64_t;
using Nat64 = uint64_t;
using Float64 = double;
using uint = uint32_t;

#define CHAR_BIT 8
static_assert(sizeof(byte) * CHAR_BIT == 8, "byte");
static_assert(sizeof(Int64) * CHAR_BIT == 64, "int64");
static_assert(sizeof(Nat64) * CHAR_BIT == 64, "nat64");
static_assert(sizeof(Float64) * CHAR_BIT == 64, "float64");

using CStr = const char*;

template <typename T>
inline T todo(const CStr message) {
	printf("%s\n", message);
	throw message;
}

inline size_t safeIntToSizeT(const int i) {
	assert(i >= 0);
	return static_cast<size_t>(i);
}

inline ssize_t safeSizeTToSSizeT(const size_t s) {
	assert(s <= 9999);
	return static_cast<ssize_t>(s);
}

inline int safeSizeTToInt(const size_t s) {
	assert(s <= 9999);
	return s;
}

inline uint safeSizeTToUint(const size_t s) {
	assert(s <= 9999);
	return s;
}

struct Bool {
	bool b;

	inline explicit Bool(bool _b) : b{_b} {}
	// This is necessary to use Bool in 'if' statements.
	// Unfortunately, this makes it implicitly convert to int too!
	// (and size_t, and float, and basically everything...)
	inline operator bool() const {
		return b;
	}
};

static const Bool True { true };
static const Bool False { false };

template <typename T>
const Bool eq(const T a, const T b) {
	static_assert(std::is_fundamental<T>::value, "must be primitive");
	return Bool{a == b};
}

template <typename T>
const Bool neq(const T a, const T b) {
	static_assert(std::is_fundamental<T>::value, "must be primitive");
	return Bool{a != b};
}

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

	inline const Bool operator!=(const RangeIter other) const {
		return neq(cur, other.cur);
	}
};

struct Range {
	const size_t lo;
	const size_t hi;

	inline Range(const size_t _hi) : lo{0}, hi{_hi} {
		assert(lo <= hi);
	}
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

struct RangeDownIter {
	ssize_t cur;

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
		return RangeDownIter{safeSizeTToSSizeT(n - 1)};
	}

	inline RangeDownIter end() const {
		return RangeDownIter{-1};
	}
};

template <typename T>
void initMemory(const T& to, const T& from) {
	// T may contain const members, so do initialization by blitting
	byte* toByte = const_cast<byte*>(reinterpret_cast<const byte*>(&to));
	const byte* fromByte = reinterpret_cast<const byte*>(&from);
	for (const size_t i : Range{sizeof(T)})
		toByte[i] = fromByte[i];
}

template <typename T>
void overwriteConst(const T& to, T from) {
	initMemory(to, from);
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
struct Opt {
private:
	const Bool _has;
	union {
		const T value;
	};

public:
	inline Opt() : _has{False} {}
	inline Opt(T v) : _has{True}, value{v} {}

	inline const Bool has() const {
		return _has;
	}

	inline T force() const {
		assert(has());
		return value;
	}
};

template <typename T>
inline T forceOrTodo(const Opt<T> opt) {
	if (opt.has())
		return opt.force();
	else
		return todo<T>("forceOrTodo");
}

enum class Comparison {
	less,
	equal,
	greater,
};

template <typename T>
using Eq = const Bool(*)(const T, const T);

template <typename T>
using Cmp = Comparison(*)(const T, const T);

template <typename T>
inline const Bool enumEq(const T a, const T b) {
	static_assert(std::is_enum<T>::value && !std::is_convertible<T, int>::value, "must be enum class");
	return Bool{a == b};
}

inline const Bool gt(const size_t a, const size_t b) {
	return Bool{a > b};
}

#define _not(a) Bool{!a}
#define _and(a, b) Bool{a && b}
#define _or(a, b) Bool{a || b}
#define _or3(a, b, c) Bool{a || b || c}

template <typename T>
inline Comparison comparePrimitive(const T a, const T b) {
	static_assert(std::is_fundamental<T>::value || std::is_enum<T>::value, "must be primitive");
	return a < b ? Comparison::less : a > b ? Comparison::greater : Comparison::equal;
}

inline Comparison compareBool(const Bool a, const Bool b) {
	return comparePrimitive(a.b, b.b);
}
inline Comparison compareChar(const char a, const char b) {
	return comparePrimitive(a, b);
}
inline Comparison compareInt64(const Int64 a, const Int64 b) {
	return comparePrimitive(a, b);
}
inline Comparison compareNat64(const Nat64 a, const Nat64 b) {
	return comparePrimitive(a, b);
}
inline const Bool charEq(const char a, const char b) {
	return eq(a, b);
}

template <typename T, Cmp<T> cmpValues>
Comparison compareOpt(const Opt<T> a, const Opt<T> b) {
	return a.has()
		? b.has()
			// some <=> some
			? cmpValues(a.force(), b.force())
			// some > none
			: Comparison::greater
		: b.has()
			// none < some
			? Comparison::less
			// none == none
			: Comparison::equal;
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
struct Arr {
	T* const _begin;
	const size_t size;

	inline const T* begin() const {
		return _begin;
	}
	inline T* begin() {
		return _begin;
	}

	inline const T* end() const {
		return _begin + size;
	}
	inline T* end() {
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
};

template <typename T, Cmp<T> cmp>
Comparison compareArr(const Arr<T> a, const Arr<T> b) {
	const Comparison sizeCmp = comparePrimitive(a.size, b.size);
	if (sizeCmp != Comparison::equal)
		return sizeCmp;
	else {
		for (const size_t i : Range{a.size}) {
			const Comparison c = cmp(a[i], b[i]);
			if (c != Comparison::equal)
				return c;
		}
		return Comparison::equal;
	}
}

template <typename T>
const Arr<T> arrOfRange(T* const begin, T* const end) {
	assert(begin <= end);
	return Arr<T>{begin, static_cast<size_t>(end - begin)};
}

template <typename T>
inline const Bool isEmpty(const Arr<T> a) {
	return eq(a.size, static_cast<size_t>(0));
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

	const Bool operator!=(PtrsIter<T> other) const {
		return Bool{ptr != other.ptr};
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
inline const Bool exists(const Arr<T> arr, Cb cb) {
	for (T x : arr) {
		const Bool b = cb(x);
		if (b)
			return True;
	}
	return False;
}

template <typename T>
inline T only(const Arr<T> a) {
	assert(a.size == 1);
	return a[0];
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

template <typename T>
struct MutArr {
private:
	bool isFrozen;
	T* _begin;
	size_t capacity;
	size_t _size;

public:
	inline MutArr() : isFrozen{false}, _begin{nullptr}, capacity{0}, _size{0} {}
	MutArr(const MutArr&) = delete;
	MutArr(MutArr&&) = default;
	inline MutArr(T* begin, const size_t len) : isFrozen{false}, _begin{begin}, capacity{len}, _size{len} {}
	inline MutArr(Arena& arena, T value) : isFrozen{false}, _begin{arena.nu<T>()(value)}, capacity{1}, _size{1} {}

	inline T& operator[](const size_t index) {
		assert(index < _size);
		return _begin[index];
	}

	inline const T& operator[](const size_t index) const {
		assert(index < _size);
		return _begin[index];
	}

	void set(const size_t index, const T value) {
		assert(!isFrozen);
		assert(index < _size);
		overwriteConst(_begin[index], value);
	}

	void push(Arena& arena, const T value) {
		if (_size == capacity) {
			T* oldBegin = _begin;
			capacity = _size == 0 ? 4 : _size * 2;
			_begin = static_cast<T*>(arena.alloc(sizeof(T) * capacity));
			for (const size_t i : Range{_size})
				initMemory(_begin[i], oldBegin[i]);
		}
		assert(_size < capacity);
		initMemory(_begin[_size], value);
		_size++;
	}

	const Opt<T> pop() {
		if (_size == 0)
			return none<T>();
		else {
			_size--;
			return some<T>(_begin[_size]);
		}
	}

	const T mustPop() {
		return pop().force();
	}

	const Opt<T> peek() const {
		return _size == 0
			? none<T>()
			: some<T>(_begin[_size - 1]);
	}

	inline size_t size() const {
		return _size;
	}

	const T* begin() const {
		return _begin;
	}

	inline const Arr<T> freeze() {
		isFrozen = true;
		return tempAsArr();
	}

	inline const Arr<T> tempAsArr() const {
		return Arr<T>{_begin, _size};
	}
	inline Arr<T> tempAsArr() {
		return Arr<T>{_begin, _size};
	}

	void deleteAt(size_t index) {
		assert(index < _size);
		for (const size_t i : Range{index, _size - 1})
			overwriteConst(_begin[i], _begin[i + 1]);
		_size--;
	}
};

template <typename T>
inline MutArr<T> newUninitializedMutArr(Arena& arena, const size_t size) {
	return MutArr<T>{static_cast<T*>(arena.alloc(sizeof(T) * size)), size};
}

template <typename T>
const Bool isEmpty(const MutArr<T>& a) {
	return eq(a.size(), static_cast<size_t>(0));
}

template <typename T>
struct ArrBuilder {
	MutArr<T> inner;
	ArrBuilder(const ArrBuilder<T>&) = delete;

	inline ArrBuilder() : inner{} {}

	inline void add(Arena& arena, T value) {
		inner.push(arena, value);
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

template <typename T>
inline const Bool isEmpty(const ArrBuilder<T>& a) {
	return isEmpty(a.inner);
}

using Str = const Arr<const char>;
struct NulTerminatedStr {
	const Str str;
	explicit inline NulTerminatedStr(const Str _str) : str{_str} {
		assert(str[str.size - 1] == '\0');
	}

	inline CStr asCStr() const {
		return str.begin();
	}
};

using MutStr = MutArr<const char>;

inline CStr end(const CStr c) {
	return *c == '\0' ? c : end(c + 1);
}

inline const Str strLiteral(const CStr c) {
	return Str{c, static_cast<size_t>(end(c) - c)};
}

inline const NulTerminatedStr nulTerminatedStrLiteral(const CStr c) {
	return NulTerminatedStr{Str{c, static_cast<size_t>(end(c) + 1 - c)}};
}

const NulTerminatedStr strToNulTerminatedStr(Arena& arena, const Str s);

const Bool strEq(const Str a, const Str b);

Comparison compareStr(const Str a, const Str b);

inline const Bool strEqLiteral(const Str s, const CStr b) {
	return strEq(s, strLiteral(b));
}

template <typename T>
inline void unused(T&) {}
template <typename T, typename U>
inline void unused(T&, U&) {}

template <typename Success, typename Failure>
struct Result {
private:
	const Bool _isSuccess;
	union {
		Success success;
		Failure failure;
	};

public:
	using SuccessType = Success;
	using FailureType = Failure;

	inline Result(const Success s) : _isSuccess{True}, success{s} {}
	inline Result(const Failure f) : _isSuccess{False}, failure{f} {}

	inline const Bool isSuccess() const {
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

	template <typename CbSuccess, typename CbFailure>
	inline auto match(CbSuccess cbSuccess, CbFailure cbFailure) const {
		if (_isSuccess)
			return cbSuccess(success);
		else
			return cbFailure(failure);
	}
};

template <typename T>
inline const Bool ptrEquals(const T* a, const T* b) {
	return Bool{a == b};
}

template <typename T>
inline Comparison comparePointer(const T* a, const T* b) {
	return a < b ? Comparison::less : a >  b ? Comparison::greater : Comparison::equal;
}

template <typename K, typename V>
struct KeyValuePair {
	K key;
	V value;
};

template <typename K, typename V, Cmp<K> cmp>
struct Dict {
	Arr<KeyValuePair<K, V>> pairs;

	const Opt<V> get(const K key) const {
		for (const KeyValuePair<K, V>& pair : pairs)
			if (cmp(pair.key, key) == Comparison::equal)
				return some<V>(pair.value);
		return none<V>();
	}

	inline const V mustGet(const K key) const {
		return get(key).force();
	}
};

template <typename K, typename V, Cmp<K> cmp>
struct DictBuilder {
private:
	ArrBuilder<KeyValuePair<K, V>> builder;

public:
	inline DictBuilder() : builder{} {}

	void add(Arena& arena, K key, V value) {
		builder.add(arena, KeyValuePair<K, V>{key, value});
	}

	template <typename CbConflict>
	Dict<K, V, cmp> finish(Arena& arena, CbConflict cbConflict) {
		MutArr<KeyValuePair<K, V>> res;
		Arr<KeyValuePair<K, V>> allPairs = builder.finish();
		for (const size_t i : Range{allPairs.size}) {
			Cell<const Bool> isConflict { False };
			for (const size_t j : Range{res.size()}) {
				if (cmp(allPairs[i].key, res[j].key) == Comparison::equal) {
					cbConflict(allPairs[i].key, res[j].value, allPairs[i].value);
					isConflict.set(True);
					break;
				}
			}
			if (!isConflict.get())
				res.push(arena, allPairs[i]);
		}
		return Dict<K, V, cmp>{res.freeze()};
	}

	Dict<K, V, cmp> finishShouldBeNoConflict() {
		Arr<KeyValuePair<K, V>> allPairs = builder.finish();
		for (const size_t i : Range{allPairs.size})
			for (const size_t j : Range{i})
				assert(cmp(allPairs[i].key, allPairs[j].key) != Comparison::equal);
		return Dict<K, V, cmp>{allPairs};
	}
};

template <typename K, typename V, Cmp<K> cmp>
struct MultiDict {
	const Dict<K, const Arr<V>, cmp> inner;

	const Arr<V> get(const K key) const {
		const Opt<const Arr<V>> res = inner.get(key);
		return res.has() ? res.force() : emptyArr<V>();
	}
};

template <typename T, Cmp<T> cmp>
struct MutSet {
private:
	MutArr<T> arr;

	MutSet(const MutSet&) = delete;

public:
	inline MutSet() : arr{} {}

	const Bool has(const T value) const {
		for (T t : arr.tempAsArr())
			if (cmp(t, value) == Comparison::equal)
				return True;
		return False;
	}

	void add(Arena& arena, const T value) {
		const Bool added = tryAdd(arena, value);
		assert(added);
	}

	const Bool tryAdd(Arena& arena, const T value) {
		const Bool h = has(value);
		if (_not(h))
			arr.push(arena, value);
		return _not(h);
	}
};

template <typename K, typename V, Cmp<K> cmp>
struct MutDict {
	MutArr<KeyValuePair<K, V>> pairs;

	inline const Bool has(const K key) const {
		return get(key).has();
	}

	const Opt<V> get(const K key) const {
		for (const KeyValuePair<K, V>& pair : pairs.tempAsArr())
			if (cmp(pair.key, key) == Comparison::equal)
				return some<V>(pair.value);
		return none<V>();
	}

	inline const V mustGet(const K key) const {
		return get(key).force();
	}

	void set(Arena& arena, const K key, const V value) {
		for (KeyValuePair<K, V>& pair : pairs.tempAsArr())
			if (cmp(pair.key, key) == Comparison::equal) {
				overwriteConst(pair.value, value);
				return;
			}
		pairs.push(arena, KeyValuePair<K, V>{key, value});
	}

	void add(Arena& arena, const K key, const V value) {
		assert(!has(key));
		pairs.push(arena, KeyValuePair<K, V>{key, value});
	}

	template <typename GetValue>
	const V getOrAdd(Arena& arena, const K key, GetValue getValue) {
		for (const KeyValuePair<K, V>& pair : pairs.tempAsArr())
			if (cmp(pair.key, key) == Comparison::equal)
				return pair.value;

		const V value = getValue();
		pairs.push(arena, KeyValuePair<K, V>{key, value});
		return value;
	}

	const Opt<V> tryDeleteAndGet(const K key) {
		for (const size_t i : Range{pairs.size()})
			if (cmp(pairs[i].key, key) == Comparison::equal) {
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

template <typename K, typename V, Cmp<K> cmp>
inline const Bool isEmpty(const MutDict<K, V, cmp>& d) {
	return isEmpty(d.pairs);
}

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

template <typename T>
inline T unreachable() {
	assert(0);
}
