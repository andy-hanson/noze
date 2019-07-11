#pragma once

#include "./arrBuilder.h"
#include "./dict.h"

template <typename K, typename V, Cmp<K> cmp>
struct DictBuilder {
	ArrBuilder<KeyValuePair<K, V>> builder;
	inline DictBuilder() : builder{} {}
};

template <typename K, typename V, Cmp<K> cmp>
void addToDict(Arena& arena, DictBuilder<K, V, cmp>* db, K key, V value) {
	add<KeyValuePair<K, V>>(arena, &db->builder, KeyValuePair<K, V>{key, value});
}

template <typename K, typename V, Cmp<K> cmp, typename CbConflict>
Dict<K, V, cmp> finishDict(Arena& arena, DictBuilder<K, V, cmp>* db, CbConflict cbConflict) {
	MutArr<KeyValuePair<K, V>> res;
	Arr<KeyValuePair<K, V>> allPairs = finishArr(&db->builder);
	for (const size_t i : Range{allPairs.size}) {
		Cell<const Bool> isConflict { False };
		for (const size_t j : Range{res.size()}) {
			if (cmp(at(allPairs, i).key, at(res, j).key) == Comparison::equal) {
				cbConflict(at(allPairs, i).key, at(res, j).value, at(allPairs, i).value);
				cellSet<const Bool>(&isConflict, True);
				break;
			}
		}
		if (!cellGet(&isConflict))
			push(arena, res, at(allPairs, i));
	}
	return Dict<K, V, cmp>{freeze(res)};
}

template <typename K, typename V, Cmp<K> cmp>
Dict<K, V, cmp> finishDictShouldBeNoConflict(DictBuilder<K, V, cmp>* db) {
	Arr<KeyValuePair<K, V>> allPairs = finishArr(&db->builder);
	for (const size_t i : Range{allPairs.size})
		for (const size_t j : Range{i})
			assert(cmp(at(allPairs, i).key, at(allPairs, j).key) != Comparison::equal);
	return Dict<K, V, cmp>{allPairs};
}
