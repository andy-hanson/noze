#pragma once

#include "../diag.h"
#include "../model.h"

struct IncludeAndImportsIter {
	const Opt<const Module*> include;
	const Arr<const Module*> imports;
	// -1 is for 'include'
	ssize_t index;

	inline const Module* operator*() const {
		return index == -1 ? include.force() : imports[index];
	}

	inline void operator++() {
		index++;
	}

	inline bool operator==(const IncludeAndImportsIter other) const {
		return index == other.index;
	}
};

struct IncludeAndImportsRange {
	const Opt<const Module*> include;
	const Arr<const Module*> imports;

	inline IncludeAndImportsIter begin() const {
		return IncludeAndImportsIter{include, imports, include.has() ? -1 : 0};
	}

	inline IncludeAndImportsIter end() {
		return IncludeAndImportsIter{include, imports, safeSizeTToSSizeT(imports.size)};
	}
};

struct CheckCtx {
	Arena& arena;
	const PathAndStorageKind path;
	const Opt<const Module*> include;
	const Arr<const Module*> imports;
	ArrBuilder<const Diagnostic> diagsBuilder = ArrBuilder<const Diagnostic>{};

	inline void diag(const SourceRange range, const Diag diag) {
		diagsBuilder.add(arena, Diagnostic{path, range, diag});
	}

	inline bool hasDiags() const {
		return !diagsBuilder.isEmpty();
	}

	inline const Arr<const Diagnostic> diags() {
		return diagsBuilder.finish();
	}

	inline const IncludeAndImportsRange includeAndImportsRange() const {
		return IncludeAndImportsRange{include, imports};
	}

	inline const Str copyStr(const Str s) const {
		return ::copyStr(arena, s);
	}
};
