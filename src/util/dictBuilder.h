#pragma once

#include "./arrBuilder.h"
#include "./dict.h"

template <typename K, typename V, Cmp<K> cmp>
struct DictBuilder {
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

	Dict<K, V, cmp> finishShouldBeNoConflict() {
		Arr<KeyValuePair<K, V>> allPairs = builder.finish();
		for (const size_t i : Range{allPairs.size})
			for (const size_t j : Range{i})
				assert(cmp(at(allPairs, i).key, at(allPairs, j).key) != Comparison::equal);
		return Dict<K, V, cmp>{allPairs};
	}
};
