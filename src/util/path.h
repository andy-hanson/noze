#pragma once

#include "./opt.h"
#include "./str.h"

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

struct AbsolutePath {
	const Path* path;
};

inline const Opt<const AbsolutePath> parent(const AbsolutePath a) {
	const Opt<const Path*> parent = a.path->parent;
	return parent.has() ? some<const AbsolutePath>(AbsolutePath{parent.force()}) : none<const AbsolutePath>();
}

inline const Str baseName(const AbsolutePath a) {
	return a.path->baseName;
}

inline const Path* rootPath(Arena& arena, const Str name) {
	return arena.nu<const Path>()(none<const Path*>(), name);
}

inline const Path* childPath(Arena& arena, const Opt<const Path*> parent, const Str name) {
	return arena.nu<const Path>()(parent, name);
}

inline const Path* childPath(Arena& arena, const Path* parent, const Str name) {
	return childPath(arena, some<const Path*>(parent), name);
}

inline const AbsolutePath childPath(Arena& arena, const AbsolutePath parent, const Str name) {
	return AbsolutePath{childPath(arena, parent.path, name)};
}

inline const Path* childPath(Arena& arena, const Path* parent, const Str name0, const Str name1) {
	return childPath(arena, childPath(arena, parent, name0), name1);
}

inline const AbsolutePath childPath(Arena& arena, const AbsolutePath parent, const Str name0, const Str name1) {
	return AbsolutePath{childPath(arena, parent.path, name0, name1)};
}

const Path* removeExtension(Arena& arena, const Path* path);
inline const AbsolutePath removeExtension(Arena& arena, const AbsolutePath path) {
	return AbsolutePath{removeExtension(arena, path.path)};
}

const Path* addExtension(Arena& arena, const Path* path, const Str extension);
inline const AbsolutePath addExtension(Arena& arena, const AbsolutePath path, const Str extension) {
	return AbsolutePath{addExtension(arena, path.path, extension)};
}

const Path* changeExtension(Arena& arena, const Path* path, const Str extension);
inline const AbsolutePath changeExtension(Arena& arena, const AbsolutePath path, const Str extension) {
	return AbsolutePath{changeExtension(arena, path.path, extension)};
}


struct RelPath {
	const uint nParents;
	const Path* path;
};

const Opt<const Path*> resolvePath(Arena& arena, const Path* path, const RelPath relPath);

inline const Opt<const AbsolutePath> resolvePath(Arena& arena, const AbsolutePath path, const RelPath relPath) {
	const Opt<const Path*> p = resolvePath(arena, path.path, relPath);
	return p.has() ? some<const AbsolutePath>(AbsolutePath{p.force()}) : none<const AbsolutePath>();
}

struct AbsoluteOrRelPath {
private:
	enum class Kind {
		absolutePath,
		relPath,
	};
	const Kind kind;
	union {
		const AbsolutePath absolutePath;
		const RelPath relPath;
	};
public:
	explicit inline AbsoluteOrRelPath(const AbsolutePath _absolutePath)
		: kind{Kind::absolutePath}, absolutePath{_absolutePath} {}
	explicit inline AbsoluteOrRelPath(const RelPath _relPath)
		: kind{Kind::relPath}, relPath{_relPath} {}

	template <
		typename CbAbsolutePath,
		typename CbRelPath
	>
	inline auto match(
		CbAbsolutePath cbAbsolutePath,
		CbRelPath cbRelPath
	) const {
		switch (kind) {
			case Kind::absolutePath:
				return cbAbsolutePath(absolutePath);
			case Kind::relPath:
				return cbRelPath(relPath);
			default:
				assert(0);
		}
	}
};

const AbsoluteOrRelPath parseAbsoluteOrRelPath(Arena& arena, const Str s);

const Path* addManyChildren(Arena& arena, const Path* a, const Path* b);

inline const AbsolutePath addManyChildren(Arena& arena, const AbsolutePath a, const Path* b) {
	return AbsolutePath{addManyChildren(arena, a.path, b)};
}

const Str pathToStr(Arena& arena, const Path* path);
const NulTerminatedStr pathToNulTerminatedStr(Arena& arena, const Path* path);

inline const Str pathToStr(Arena& arena, const AbsolutePath path) {
	return pathToStr(arena, path.path);
}
inline const NulTerminatedStr pathToNulTerminatedStr(Arena& arena, const AbsolutePath path) {
	return pathToNulTerminatedStr(arena, path.path);
}
inline CStr pathToCStr(Arena& arena, const AbsolutePath path) {
	return pathToNulTerminatedStr(arena, path).asCStr();
}

// NOTE: this does *not* do a copy, original str must be kept alive!
const Path* parsePath(Arena& arena, const Str str);
const AbsolutePath parseAbsolutePath(Arena& arena, const Str str);

inline Comparison comparePath(const Path* a, const Path* b) {
	const Comparison res = compareStr(a->baseName, b->baseName);
	const Comparison result = res != Comparison::equal ? res : compareOpt<const Path*, comparePath>(a->parent, b->parent);
	return result;
}

const Path* copyPath(Arena& arena, const Path* path);
