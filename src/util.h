#pragma once

#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <new> // Support placement new
#include <cstdio> // printf
#include <type_traits> // remove_const

using byte = uint8_t;

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
inline Opt<T> none() {
	return Opt<T>{};
}

template <typename T>
inline Opt<T> some(T value) {
	return Opt<T>{value};
}

template <typename T>
void initMemory(const T& to, T from) {
	// T may contain const members, so do initialization by blitting
	std::memcpy(const_cast<typename std::remove_const<T>::type*>(&to), &from, sizeof(T));
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

	inline T* getPtr(size_t index) const {
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

	inline const Arr<T> tail() const {
		assert(!isEmpty());
		return Arr<T>{_begin + 1, size - 1};
	}

	inline const Arr<const T> asConst() const {
		return Arr<const T>{_begin, size};
	}
};

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
		for (size_t i = 0; i < arr.size; i++)
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
struct MutArr {
private:
	MutSlice<T> slice;
	size_t _size;

public:
	inline MutArr() : slice{nullptr, 0}, _size{0} {}

	inline const T& operator[](const size_t index) const {
		return slice[index];
	}

	void push(Arena& arena, T value) {
		if (_size == slice.size) {
			MutSlice<T> oldSlice = slice;
			slice = newUninitializedMutSlice<T>(arena, slice.size == 0 ? 4 : slice.size * 2);
			for (size_t i = 0; i < oldSlice.size; i++)
				initMemory(slice[i], oldSlice[i]);
		}

		assert(_size < slice.size);
		initMemory(slice[_size], value);
		_size++;
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
};

template <typename T>
struct ArrBuilder {
private:
	MutArr<T> inner;

public:
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

template <typename T>
Arr<T> cat(Arena& arena, const Arr<T> a, const Arr<T> b) {
	MutSlice<T> res = newUninitializedMutSlice<T>(arena, a.size + b.size);
	for (size_t i = 0; i < a.size; i++)
		res.set(i, a[i]);
	for (size_t i = 0; i < b.size; i++)
		res.set(a.size + i, b[i]);
	return res.freeze();
}

template <typename T>
Arr<T> prepend(Arena& arena, const T a, const Arr<T> b) {
	MutSlice<T> res = newUninitializedMutSlice<T>(arena, 1 + b.size);
	res.set(0, a);
	for (size_t i = 0; i < b.size; i++)
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

template <typename Out>
struct map {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (size_t i = 0; i < in.size; i++)
			initMemory(out[i], cb(in[i]));
		return Arr<Out>{out, in.size};
	}
};

template <typename Out>
struct mapWithIndex {
	template <typename In, typename Cb>
	Arr<Out> operator()(Arena& arena, const Arr<In> in, Cb cb) {
		Out* out = static_cast<Out*>(arena.alloc(sizeof(Out) * in.size));
		for (size_t i = 0; i < in.size; i++)
			initMemory(out[i], cb(in[i], i));
		return Arr<Out>{out, in.size};
	}
};

template <typename T, typename U, typename Cb>
void zip(Arr<T> a, Arr<U> b, Cb cb) {
	assert(a.size == b.size);
	for (size_t i = 0; i < a.size; i++)
		cb(a[i], b[i]);
}

using Str = const Arr<const char>;
using NulTerminatedStr = Str;
using MutStr = MutSlice<char>;

const Str copyStr(Arena& arena, const Str in);

bool strEq(const Str& a, const Str& b);
bool strEqLiteral(const Str s, const char* c);

inline Str strLiteral(const char* c) {
	return Str{c, strlen(c)};
}

template <typename T>
inline void unused(__attribute__((unused)) T& t) {}

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

template <typename InSuccess, typename Failure, typename Cb>
inline auto mapSuccess(const Result<InSuccess, Failure> r, Cb cb) {
	using OutSuccess = decltype(cb(r.asSuccess()));
	return r.isSuccess()
		? success<OutSuccess, Failure>(cb(r.asSuccess()))
		: failure<OutSuccess, Failure>(r.asFailure());
}

template <typename InSuccess, typename Failure, typename Cb>
inline auto flatMapSuccess(const Result<InSuccess, Failure> r, Cb cb) {
	using OutSuccess = typename decltype(cb(r.asSuccess()))::SuccessType;
	return r.isSuccess()
		? cb(r.asSuccess())
		: failure<OutSuccess, Failure>(r.asFailure());
}

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

template <typename T>
using Eq = bool(*)(const T&, const T&);

template <typename K, typename V, Eq<K> eq>
struct Dict {
	Arr<KeyValuePair<K, V>> pairs;

	const Opt<const V> get(const K key) const {
		for (const KeyValuePair<K, V>& pair : pairs)
			if (eq(pair.key, key))
				return some<const V>(pair.value);
		return none<const V>();
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
		for (size_t i = 0; i < allPairs.size; i++) {
			bool isConflict = false;
			for (size_t j = 0; j < res.size(); j++) {
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

};

template <typename T>
struct Late {
private:
	bool _isSet;
	union {
		T value;
	};
public:
	Late() {}

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
};

template <typename T>
struct Cell {
private:
	T value;
public:
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
