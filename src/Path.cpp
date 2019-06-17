#include "./Path.h"

#include "./util/arrUtil.h"

namespace {
	template <typename Cb>
	static void walkPathBackwards(const Path* p, Cb cb) {
		for (;;) {
			cb(p->baseName);
			if (!p->parent.has())
				break;
			p = p->parent.force();
		}
	}

	size_t pathToStrSize(const Path* path, const Bool nulTerminated) {
		size_t size = 0;
		walkPathBackwards(path, [&](const Str part) { size += part.size + 1; });
		return nulTerminated ? size + 1 : size;
	}

	Str pathToStrWorker(Arena& arena, const Path* path, const Bool nulTerminated) {
		const size_t size = pathToStrSize(path, nulTerminated);
		MutStr res = newUninitializedMutArr<const char>(arena, size);
		size_t i = size;
		if (nulTerminated) {
			i --;
			res.set(i, '\0');
		}
		walkPathBackwards(path, [&](const Str part) {
			i -= part.size;
			copyFrom(res, i, part);
			i--;
			res.set(i, '/');
		});
		assert(i == 0);
		return res.freeze();
	}
}

const Path* addManyChildren(Arena& arena, const Path* a, const Path* b) {
	const Path* p = b->parent.has()
		? addManyChildren(arena, a, b->parent.force())
		: a;
	return childPath(arena, p, b->baseName);
}

const Path* addExt(Arena& arena, const Path* p, const Str ext) {
	return arena.nu<const Path>()(p->parent, cat(arena, p->baseName, ext));
}

Opt<const Path*> resolvePath(Arena& arena, const Path* path, const RelPath relPath) {
	Path const* cur = path;
	for (uint i = 0; i < relPath.nParents; i++) {
		if (!cur->parent.has())
			return i == relPath.nParents - 1
				? some<const Path*>(relPath.path)
				: none<const Path*>();
		cur = cur->parent.force();
	}
	return some<const Path*>(addManyChildren(arena, cur, relPath.path));
}

const Str pathToStr(Arena& arena, const Path* path) {
	return pathToStrWorker(arena, path, /*nulTerminated*/ False);
}

const NulTerminatedStr pathToNulTerminatedStr(Arena& arena, const Path* path) {
	return pathToStrWorker(arena, path, /*nulTerminated*/ True);
}

const Path* pathFromNulTerminatedStr(Arena& arena, const NulTerminatedStr str) {
	const char* cur = str.begin();
	if (*cur == '/')
		// Ignore leading slash
		cur++;

	Path const* path = [&]() {
		const char* beginA = cur;
		while (*cur != '/' && *cur != '\0')
			cur++;
		assert(cur != beginA);
		return rootPath(arena, arrOfRange(beginA, cur));
	}();

	if (*cur == '/')
		cur++;

	while (*cur != '\0') {
		const char* begin = cur;
		while (*cur != '/' && *cur != '\0')
			cur++;
		path = childPath(arena, path, arrOfRange(begin, cur));
		if (*cur == '/')
			cur++;
	}

	return path;
}

const Path* copyPath(Arena& arena, const Path* path) {
	return arena.nu<const Path>()(
		path->parent.has() ? some<const Path*>(copyPath(arena, path->parent.force())) : none<const Path*>(),
		copyStr(arena, path->baseName));
}
