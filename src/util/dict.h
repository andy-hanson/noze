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

	const Opt<V> get(const K key) const {
		for (const KeyValuePair<K, V>& pair : pairs)
			if (cmp(pair.key, key) == Comparison::equal)
				return some<V>(pair.value);
		return none<V>();
	}

	inline const V mustGet(const K key) const {
		return force(get(key));
	}
};

template <typename K, typename V, Cmp<K> cmp>
struct MultiDict {
	const Dict<K, const Arr<V>, cmp> inner;

	const Arr<V> get(const K key) const {
		const Opt<const Arr<V>> res = inner.get(key);
		return has(res) ? force(res) : emptyArr<V>();
	}
};
