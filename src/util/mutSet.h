#pragma once

template <typename T, Cmp<T> cmp>
struct MutSet {
	MutArr<T> arr {};
	MutSet(const MutSet&) = delete;
};

template <typename T, Cmp<T> cmp>
const Bool mutSetHas(const MutSet<T, cmp>* s, const T value) {
	for (const T t : tempAsArr(&s->arr))
		if (cmp(t, value) == Comparison::equal)
			return True;
	return False;
}

template <typename T, Cmp<T> cmp>
const Bool tryAddToMutSet(Arena* arena, MutSet<T, cmp>* s, const T value) {
	const Bool h = mutSetHas<T, cmp>(s, value);
	if (_not(h))
		push<T>(arena, &s->arr, value);
	return _not(h);
}

template <typename T, Cmp<T> cmp>
void addToMutSet(Arena* arena, MutSet<T, cmp>* s, const T value) {
	const Bool added = tryAddToMutSet<T, cmp>(arena, s, value);
	assert(added);
}
