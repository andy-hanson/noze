#pragma once

#include "./util.h"

inline const Bool isValidPathPartChar(const char c) {
	switch (c) {
		case '\n':
		case '/':
		case '\0':
		case '\\':
			return False;
		default:
			return True;
	}
}

struct Path {
	const Opt<const Path*> parent;
	const Str baseName;

	Path(const Opt<const Path*> _parent, const Str _baseName) : parent{_parent}, baseName{_baseName} {
		assert(!isEmpty(baseName));
		for (const char c : baseName) {
			assert(isValidPathPartChar(c));
		}
	}
};

inline const Path* rootPath(Arena& arena, const Str name) {
	return arena.nu<const Path>()(none<const Path*>(), name);
}

inline const Path* childPath(Arena& arena, const Path* p, const Str name) {
	return arena.nu<const Path>()(some<const Path*>(p), name);
}

const Path* addExt(Arena& arena, const Path* p, const Str ext);

struct RelPath {
	const uint nParents;
	const Path* path;
};

Opt<const Path*> resolvePath(Arena& arena, const Path* path, const RelPath relPath);

const Path* addManyChildren(Arena& arena, const Path* a, const Path* b);
const Str pathToStr(Arena& arena, const Path* path);
const NulTerminatedStr pathToNulTerminatedStr(Arena& arena, const Path* path);

// NOTE: this does *not* do a copy, original str must be kept alive!
const Path* pathFromNulTerminatedStr(Arena& arena, const NulTerminatedStr str);

inline Comparison comparePath(const Path* a, const Path* b) {
	const Comparison res = compareStr(a->baseName, b->baseName);
	const Comparison result = res != Comparison::equal ? res : compareOpt<const Path*, comparePath>(a->parent, b->parent);
	return result;
}
