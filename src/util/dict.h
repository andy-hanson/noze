#pragma once

#include "./arr.h"
#include "./opt.h"

template <typename K, typename V>
struct KeyValuePair {
	K key;
	V value;
};

template <typename K, typename V, Cmp<K> cmp>
struct Dict {
	Arr<KeyValuePair<K, V>> pairs;
};

template <typename K, typename V, Cmp<K> cmp>
const Opt<V> getAt(const Dict<K, V, cmp> d, const K key) {
	for (const KeyValuePair<K, V>& pair : d.pairs)
		if (cmp(pair.key, key) == Comparison::equal)
			return some<V>(pair.value);
	return none<V>();
}

template <typename K, typename V, Cmp<K> cmp>
inline const V mustGetAt(const Dict<K, V, cmp> d, const K key) {
	return force(getAt<K, V, cmp>(d, key));
}

template <typename K, typename V, Cmp<K> cmp>
struct MultiDict {
	const Dict<K, const Arr<V>, cmp> inner;
};

template <typename K, typename V, Cmp<K> cmp>
const Arr<V> multiDictGetAt(const MultiDict<K, V, cmp> d, const K key) {
	const Opt<const Arr<V>> res = getAt<K, const Arr<V>, cmp>(d.inner, key);
	return has(res) ? force(res) : emptyArr<V>();
}
