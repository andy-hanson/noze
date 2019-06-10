#pragma once

#include "../model.h" // StorageKind

struct ReadOnlyStorage {
	const Path* root;

	const Opt<const NulTerminatedStr> tryReadFile(Arena& arena, const Path* path) const;
};

struct ReadOnlyStorages {
	const ReadOnlyStorage include;
	const ReadOnlyStorage user;

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
