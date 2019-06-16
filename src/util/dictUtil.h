#pragma once

template <typename K, typename V, Cmp<K> cmp>
struct buildDict {
	template <typename T, typename GetPair, typename OnConflict>
	Dict<K, V, cmp> operator()(Arena& arena, const Arr<T> inputs, GetPair getPair, OnConflict onConflict) {
		MutArr<KeyValuePair<K, V>> res;
		for (const T& input : inputs) {
			KeyValuePair<K, V> pair = getPair(input);
			Cell<const Bool> wasConflict { False };
			for (const size_t i : Range{res.size()})
				if (cmp(res[i].key, pair.key) == Comparison::equal) {
					onConflict(pair.key, res[i].value, pair.value);
					wasConflict.set(True);
					break;
				}
			if (!wasConflict.get())
				res.push(arena, pair);
		}
		return Dict<K, V, cmp>{res.freeze()};
	}
};

template <typename K, typename V, Cmp<K> cmp>
struct buildMultiDict {
	template <typename T, typename GetPair>
	MultiDict<K, V, cmp> operator()(Arena& arena, const Arr<T> inputs, GetPair getPair) {
		MutArr<KeyValuePair<K, MutArr<V>>> res {};
		for (const T& input : inputs) {
			KeyValuePair<K, V> pair = getPair(input);
			Cell<const Bool> didAdd { False };
			for (const size_t i : Range{res.size()})
				if (cmp(res[i].key, pair.key) == Comparison::equal) {
					res[i].value.push(arena, pair.value);
					didAdd.set(True);
					break;
				}
			if (!didAdd.get())
				res.push(arena, KeyValuePair<K, MutArr<V>>{pair.key, MutArr<V>{arena, pair.value}});
		}
		const Arr<KeyValuePair<K, MutArr<V>>> arr = res.freeze();
		const Arr<KeyValuePair<K, const Arr<V>>> pairs = map<KeyValuePair<K, const Arr<V>>>{}(arena, arr, [](KeyValuePair<K, MutArr<V>>& m) {
			return KeyValuePair<K, const Arr<V>>{m.key, m.value.freeze()};
		});
		return MultiDict<K, V, cmp>{Dict<K, const Arr<V>, cmp>{pairs}};
	}
};

