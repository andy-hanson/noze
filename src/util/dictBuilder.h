#pragma once

#include "./arrBuilder.h"
#include "./dict.h"

template <typename K, typename V, Cmp<K> cmp>
struct DictBuilder {
	ArrBuilder<KeyValuePair<K, V>> builder;
	inline DictBuilder() : builder{} {}
};

template <typename K, typename V, Cmp<K> cmp>
void addToDict(Arena* arena, DictBuilder<K, V, cmp>* db, K key, V value) {
	add<KeyValuePair<K, V>>(arena, &db->builder, KeyValuePair<K, V>{key, value});
}

template <typename K, typename V, Cmp<K> cmp, typename CbConflict>
Dict<K, V, cmp> finishDict(Arena* arena, DictBuilder<K, V, cmp>* db, CbConflict cbConflict) {
	MutArr<KeyValuePair<K, V>> res;
	Arr<KeyValuePair<K, V>> allPairs = finishArr(&db->builder);
	for (const size_t i : Range{size(allPairs)}) {
		const KeyValuePair<K, V> pair = at(allPairs, i);
		Cell<const Bool> isConflict { False };
		for (const size_t j : Range{mutArrSize(&res)}) {
			const KeyValuePair<K, V> resPair = mutArrAt(&res, j);
			if (cmp(pair.key, resPair.key) == Comparison::equal) {
				cbConflict(pair.key, resPair.value, pair.value);
				cellSet<const Bool>(&isConflict, True);
				break;
			}
		}
		if (!cellGet(&isConflict))
			push(arena, &res, pair);
	}
	return Dict<K, V, cmp>{freeze(&res)};
}

template <typename K, typename V, Cmp<K> cmp>
Dict<K, V, cmp> finishDictShouldBeNoConflict(DictBuilder<K, V, cmp>* db) {
	Arr<KeyValuePair<K, V>> allPairs = finishArr(&db->builder);
	for (const size_t i : Range{size(allPairs)})
		for (const size_t j : Range{i})
			// If this fails, there was a conflict somewhere
			assert(cmp(at(allPairs, i).key, at(allPairs, j).key) != Comparison::equal);
	return Dict<K, V, cmp>{allPairs};
}
