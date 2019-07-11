#pragma once

#include "./dict.h"
#include "./mutArr.h"

template <typename K, typename V, Cmp<K> cmp>
struct MutDict {
	MutArr<KeyValuePair<K, V>> pairs;

	inline const Bool has(const K key) const {
		return ::has(get(key));
	}

	const Opt<V> get(const K key) const {
		for (const KeyValuePair<K, V>& pair : tempAsArr(pairs))
			if (cmp(pair.key, key) == Comparison::equal)
				return some<V>(pair.value);
		return none<V>();
	}

	inline const V mustGet(const K key) const {
		return force(get(key));
	}

	void set(Arena& arena, const K key, const V value) {
		for (KeyValuePair<K, V>& pair : tempAsArr(pairs))
			if (cmp(pair.key, key) == Comparison::equal) {
				overwriteConst(pair.value, value);
				return;
			}
		push(arena, pairs, KeyValuePair<K, V>{key, value});
	}

	void add(Arena& arena, const K key, const V value) {
		assert(!has(key));
		push(arena, pairs, KeyValuePair<K, V>{key, value});
	}

	template <typename GetValue>
	const V getOrAdd(Arena& arena, const K key, GetValue getValue) {
		for (const KeyValuePair<K, V>& pair : tempAsArr(pairs))
			if (cmp(pair.key, key) == Comparison::equal)
				return pair.value;

		const V value = getValue();
		push(arena, pairs, KeyValuePair<K, V>{key, value});
		return value;
	}

	const Opt<V> tryDeleteAndGet(const K key) {
		for (const size_t i : Range{pairs.size()})
			if (cmp(at(pairs, i).key, key) == Comparison::equal) {
				const V res = at(pairs, i).value;
				deleteAt(pairs, i);
				return some<V>(res);
			}
		return none<V>();
	}

	const V mustDelete(const K key) {
		return force(tryDeleteAndGet(key));
	}
};

template <typename K, typename V, Cmp<K> cmp>
inline const Bool isEmpty(const MutDict<K, V, cmp>& d) {
	return isEmpty(d.pairs);
}
