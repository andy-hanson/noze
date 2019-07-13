#pragma once

#include "./dict.h"
#include "./mutArr.h"

template <typename K, typename V, Cmp<K> cmp>
struct MutDict {
	MutArr<KeyValuePair<K, V>> pairs;
};

template <typename K, typename V, Cmp<K> cmp>
const Opt<V> getAt_mut(const MutDict<K, V, cmp>* d, const K key) {
	for (const KeyValuePair<K, V>& pair : tempAsArr(d->pairs))
		if (cmp(pair.key, key) == Comparison::equal)
			return some<V>(pair.value);
	return none<V>();
}

template <typename K, typename V, Cmp<K> cmp>
inline const Bool hasKey_mut(const MutDict<K, V, cmp>* d, const K key) {
	return has(getAt_mut<K, V, cmp>(d, key));
}

template <typename K, typename V, Cmp<K> cmp>
inline const V mustGetAt_mut(const MutDict<K, V, cmp>* d, const K key) {
	return force(getAt_mut(d, key));
}

template <typename K, typename V, Cmp<K> cmp>
void setInDict(Arena& arena, MutDict<K, V, cmp>* d, const K key, const V value) {
	for (KeyValuePair<K, V>& pair : tempAsArr(d->pairs))
		if (cmp(pair.key, key) == Comparison::equal) {
			overwriteConst(pair.value, value);
			return;
		}
	push(arena, d->pairs, KeyValuePair<K, V>{key, value});
}

template <typename K, typename V, Cmp<K> cmp>
void addToDict(Arena& arena, MutDict<K, V, cmp>* d, const K key, const V value) {
	const Bool has = hasKey_mut<K, V, cmp>(d, key);
	assert(!has);
	push(arena, d->pairs, KeyValuePair<K, V>{key, value});
}

template <typename K, typename V, Cmp<K> cmp>
struct getOrAdd {
	template <typename GetValue>
	const V operator()(Arena& arena, MutDict<K, V, cmp>* d, const K key, GetValue getValue) {
		for (const KeyValuePair<K, V>& pair : tempAsArr(d->pairs))
			if (cmp(pair.key, key) == Comparison::equal)
				return pair.value;

		const V value = getValue();
		push(arena, d->pairs, KeyValuePair<K, V>{key, value});
		return value;
	}
};

// Like getOrAdd, but the key is allowed to be temporary; if we need to add we'll make a copy then
template <typename K, typename V, Cmp<K> cmp>
struct getOrAddAndCopyKey {
	template <typename GetKeyCopy, typename GetValue>
	const V operator()(Arena& arena, MutDict<K, V, cmp>* d, const K key, GetKeyCopy getKeyCopy, GetValue getValue) {
		for (const KeyValuePair<K, V>& pair : tempAsArr(d->pairs))
			if (cmp(pair.key, key) == Comparison::equal)
				return pair.value;

		const K keyCopy = getKeyCopy();
		assert(cmp(keyCopy, key) == Comparison::equal);
		const V value = getValue();
		push(arena, d->pairs, KeyValuePair<K, V>{keyCopy, value});
		return value;
	}
};

template <typename K, typename V, Cmp<K> cmp>
const Opt<V> tryDeleteAndGet(MutDict<K, V, cmp>* d, const K key) {
	for (const size_t i : Range{d->pairs.size()})
		if (cmp(at(d->pairs, i).key, key) == Comparison::equal) {
			const V res = at(d->pairs, i).value;
			deleteAt(d->pairs, i);
			return some<V>(res);
		}
	return none<V>();
}

template <typename K, typename V, Cmp<K> cmp>
const V mustDelete(MutDict<K, V, cmp>* d, const K key) {
	return force(tryDeleteAndGet(d, key));
}

template <typename K, typename V, Cmp<K> cmp>
inline const Bool isEmpty(const MutDict<K, V, cmp>& d) {
	return isEmpty(d.pairs);
}
