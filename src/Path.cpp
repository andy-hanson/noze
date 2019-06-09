#include "./Path.h"

namespace {
	template <typename Cb>
	static void walkPathBackwards(const Path* p, Cb cb) {
		while (true) {
			cb(p->baseName);
			if (!p->parent.has())
				break;
			p = p->parent.force();
		}
	}

	size_t pathToStrSize(const Path* path, const bool nulTerminated) {
		size_t size = 0;
		walkPathBackwards(path, [&](const Str part) { size += part.size + 1; });
		return nulTerminated ? size + 1 : size;
	}

	MutStr pathToMutStr(Arena& arena, const Path* path, bool nulTerminated) {
		const size_t size = pathToStrSize(path, nulTerminated);
		MutStr res = newUninitializedMutSlice<char>(arena, size);
		size_t i = size;
		if (nulTerminated) {
			i --;
			res.set(i, '\0');
		}
		walkPathBackwards(path, [&](const Str part) {
			i -= part.size;
			res.copyFrom(i, part);
			i--;
			res.set(i, '/');
		});
		assert(i == 0);
		return res;
	}

	const Path* addManyChildren(Arena& arena, const Path* a, const Path* b) {
		const Path* p = b->parent.has()
			? addManyChildren(arena, a, b->parent.force())
			: a;
		return childPath(arena, p, b->baseName);
	}
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
	return pathToMutStr(arena, path, false).freeze().asConst();
}

const NulTerminatedStr pathToNulTerminatedStr(Arena& arena, const Path* path) {
	return pathToMutStr(arena, path, true).freeze().asConst();
}
