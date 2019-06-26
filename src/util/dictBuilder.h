#pragma once

#include "../util.h"

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
