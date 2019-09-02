#pragma once

#include "./dict.h"
#include "./dictBuilder.h"

struct Void {};

template <typename T, Cmp<T> cmp>
struct Set {
	Dict<T> d {};
};

template <typename T, Cmp<T> cmp>
const Bool setHas(const Set<T, cmp> s, const T value) {
	return hasKey(s->d, value);
}

template <typename T, Cmp<T> cmp>
struct SetBuilder {
	DictBuilder<T, Void, cmp> db;
};

template <typename T, Cmp<T> cmp>
void addToSet(Arena* arena, SetBuilder<T, cmp> sb, const T value) {
	addToDict(arena, sb.db, value, Void{});
}

void finishSet(Arena* arena, SetBuilder<T, cmp> sb) {
	// Ignore conflicts, since all Void values must be equal anyway
	return Set{finishDict(arena, sb, [&](T _k, Void _v0, Void _v1) {})};
}

