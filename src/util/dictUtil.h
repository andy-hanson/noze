#pragma once

template <typename K, typename V, Cmp<K> cmp>
struct buildDict {
	template <typename T, typename GetPair, typename OnConflict>
	Dict<K, V, cmp> operator()(Arena* arena, const Arr<T> inputs, GetPair getPair, OnConflict onConflict) {
		MutArr<KeyValuePair<K, V>> res;
		for (const T& input : inputs) {
			KeyValuePair<K, V> pair = getPair(input);
			Cell<const Bool> wasConflict { False };
			for (const size_t i : Range{res.size()})
				if (cmp(at(res, i).key, pair.key) == Comparison::equal) {
					onConflict(pair.key, at(res, i).value, pair.value);
					cellSet<const Bool>(&wasConflict, True);
					break;
				}
			if (!cellGet(&wasConflict))
				push(arena, res, pair);
		}
		return Dict<K, V, cmp>{freeze(res)};
	}
};

template <typename K, typename V, Cmp<K> cmp>
struct buildMultiDict {
	template <typename T, typename GetPair>
	MultiDict<K, V, cmp> operator()(Arena* arena, const Arr<T> inputs, GetPair getPair) {
		MutArr<KeyValuePair<K, MutArr<V>>> res {};
		for (const T& input : inputs) {
			KeyValuePair<K, V> pair = getPair(input);
			Cell<const Bool> didAdd { False };
			for (const size_t i : Range{res.size()})
				if (cmp(at(res, i).key, pair.key) == Comparison::equal) {
					push(arena, at(res, i).value, pair.value);
					cellSet<const Bool>(&didAdd, True);
					break;
				}
			if (!cellGet(&didAdd))
				push(arena, res, KeyValuePair<K, MutArr<V>>{pair.key, MutArr<V>{arena, pair.value}});
		}
		const Arr<KeyValuePair<K, MutArr<V>>> arr = freeze(res);
		const Arr<KeyValuePair<K, const Arr<V>>> pairs = mapPtrs<KeyValuePair<K, const Arr<V>>>{}(arena, arr, [](KeyValuePair<K, MutArr<V>>* m) {
			return KeyValuePair<K, const Arr<V>>{m->key, freeze(m->value)};
		});
		return MultiDict<K, V, cmp>{Dict<K, const Arr<V>, cmp>{pairs}};
	}
};

