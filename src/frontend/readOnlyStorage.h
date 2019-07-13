#pragma once

#include "../model.h" // StorageKind

#include "../util/io.h"

struct ReadOnlyStorage {
	const AbsolutePath root;

	inline const Opt<const NulTerminatedStr> tryReadFile(Arena* arena, const Path* path) const {
		return ::tryReadFile(arena, addManyChildren(arena, root, path));
	}
};

struct ReadOnlyStorages {
	const ReadOnlyStorage include;
	const ReadOnlyStorage user;

	const AbsolutePathsGetter absolutePathsGetter() const {
		return AbsolutePathsGetter{include.root, user.root};
	}

	inline const ReadOnlyStorage choose(const StorageKind storageKind) const {
		switch (storageKind) {
			case StorageKind::global:
				return include;
			case StorageKind::local:
				return user;
			default:
				return unreachable<const ReadOnlyStorage>();
		}
	}
};
