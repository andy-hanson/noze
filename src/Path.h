#pragma once

#include "./util.h"

struct Path {
	const Opt<const Path*> parent;
	const Str baseName;

	Path(const Opt<const Path*> p, const Str bn) : parent{p}, baseName{bn} {}

	static inline const Path* root(Arena& arena, const Str name) {
		char* p = arena.newUninitialized<char>();
		return new (p) Path{none<const Path*>(), name};
		//return arena.nu<const Path>()(none<const Path*>(), name);
	}
};

inline const Path* childPath(Arena& arena, const Path* p, const Str name) {
	return arena.nu<const Path>()(some<const Path*>(p), name);
}

inline const Path* addExt(Arena& arena, const Path* p, const Str ext) {
	return arena.nu<const Path>()(p->parent, cat(arena, p->baseName, ext));
}

struct RelPath {
	const uint nParents;
	const Path* path;
};

Opt<const Path*> resolvePath(Arena& arena, const Path* path, const RelPath relPath);

const Str pathToStr(Arena& arena, const Path* path);
const NulTerminatedStr pathToNulTerminatedStr(Arena& arena, const Path* path);
