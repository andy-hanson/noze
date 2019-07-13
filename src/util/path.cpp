#include "./path.h"

#include "./arrUtil.h"

namespace {
	template <typename Cb>
	static void walkPathBackwards(const Path* p, Cb cb) {
		for (;;) {
			cb(p->baseName);
			if (!has(p->parent))
				break;
			p = force(p->parent);
		}
	}

	size_t pathToStrSize(const Path* path, const Bool nulTerminated) {
		size_t size = 0;
		walkPathBackwards(path, [&](const Str part) { size += part.size + 1; });
		return nulTerminated ? size + 1 : size;
	}

	const Str pathToStrWorker(Arena* arena, const Path* path, const Bool nulTerminated) {
		const size_t size = pathToStrSize(path, nulTerminated);
		MutStr res = newUninitializedMutArr<const char>(arena, size);
		size_t i = size;
		if (nulTerminated) {
			i --;
			setAt<const char>(res, i, '\0');
		}
		walkPathBackwards(path, [&](const Str part) {
			i -= part.size;
			copyFrom(res, i, part);
			i--;
			setAt<const char>(res, i, '/');
		});
		assert(i == 0);
		return freeze(res);
	}

	const Str removeExtension(const Str s) {
		// Find the '.'
		for (const size_t i : RangeDown{s.size}) {
			if (at(s, i) == '.')
				return slice(s, 0, i);
		}
		return todo<const Str>("removeExtension -- no '.'");
	}

	const Str addExtension(Arena* arena, const Str s, const Str extension) {
		return cat(arena, s, strLiteral("."), extension);
	}

	const Str changeExtension(Arena* arena, const Str s, const Str extension) {
		return addExtension(arena, removeExtension(s), extension);
	}
}

const Path* addManyChildren(Arena* arena, const Path* a, const Path* b) {
	const Path* p = has(b->parent)
		? addManyChildren(arena, a, force(b->parent))
		: a;
	return childPath(arena, p, b->baseName);
}

const Path* removeExtension(Arena* arena, const Path* path) {
	return childPath(arena, path->parent, removeExtension(path->baseName));
}

const Path* addExtension(Arena* arena, const Path* path, const Str extension) {
	return childPath(arena, path->parent, addExtension(arena, path->baseName, extension));
}

const Path* changeExtension(Arena* arena, const Path* path, const Str extension) {
	return childPath(arena, path->parent, changeExtension(arena, path->baseName, extension));
}

const Opt<const Path*> resolvePath(Arena* arena, const Path* path, const RelPath relPath) {
	Path const* cur = path;
	for (uint i = 0; i < relPath.nParents; i++) {
		if (!has(cur->parent))
			return i == relPath.nParents - 1
				? some<const Path*>(relPath.path)
				: none<const Path*>();
		cur = force(cur->parent);
	}
	return some<const Path*>(addManyChildren(arena, cur, relPath.path));
}

namespace {
	const RelPath parseRelPath(Arena* arena, const Str s) {
		if (at(s, 0) == '.') {
			if (at(s, 1) == '/')
				return parseRelPath(arena, slice(s, 2));
			else if (at(s, 1) == '.' && at(s, 2) == '/') {
				const RelPath r = parseRelPath(arena, slice(s, 3));
				return RelPath{r.nParents + 1, r.path};
			} else
				// Path component happens to start with '.' but is not '.' or '..'
				return RelPath{0, parsePath(arena, s)};
		} else
			return RelPath{0, parsePath(arena, s)};
	}
}

const AbsoluteOrRelPath parseAbsoluteOrRelPath(Arena* arena, const Str s) {
	switch (at(s, 0)) {
		case '.':
			return AbsoluteOrRelPath{parseRelPath(arena, s)};
		case '/':
			return AbsoluteOrRelPath{parseAbsolutePath(arena, s)};
		case '\\':
			return todo<const AbsoluteOrRelPath>("unc path?");
		default:
			if (at(s, 1) == ':')
				return todo<const AbsoluteOrRelPath>("C:/?");
			else
				return AbsoluteOrRelPath{RelPath{0, parsePath(arena, s)}};
	}
}

const Str pathToStr(Arena* arena, const Path* path) {
	return pathToStrWorker(arena, path, /*nulTerminated*/ False);
}

const NulTerminatedStr pathToNulTerminatedStr(Arena* arena, const Path* path) {
	return NulTerminatedStr{pathToStrWorker(arena, path, /*nulTerminated*/ True)};
}

const Path* parsePath(Arena* arena, const Str str) {
	const size_t len = str.size;
	size_t i = 0;
	if (i < len && at(str, i) == '/')
		// Ignore leading slash
		i++;

	Path const* path = [&]() {
		const size_t begin = i;
		while (i < len && at(str, i) != '/')
			i++;
		assert(i != begin);
		return rootPath(arena, sliceFromTo(str, begin, i));
	}();


	while (i < len) {
		if (at(str, i) == '/')
			i++;
		if (i == len)
			break;
		const size_t begin = i;
		while (i < len && at(str, i) != '/')
			i++;
		path = childPath(arena, path, sliceFromTo(str, begin, i));
	}

	return path;
}

const AbsolutePath parseAbsolutePath(Arena* arena, const Str str) {
	// TODO: handle 'C:/'
	return AbsolutePath{parsePath(arena, str)};
}

const Path* copyPath(Arena* arena, const Path* path) {
	return nu<const Path>{}(
		arena,
		has(path->parent)
			? some<const Path*>(copyPath(arena, force(path->parent)))
			: none<const Path*>(),
		copyStr(arena, path->baseName));
}
