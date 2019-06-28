#pragma once

#include "../diag.h"
#include "../model.h"

#include "../util/arrUtil.h" // copyStr

struct IncludeAndImportsIter {
	const Opt<const Module*> include;
	const Arr<const Module*> imports;
	// -1 is for 'include'
	ssize_t index;

	inline const Module* operator*() const {
		return index == -1 ? include.force() : at(imports, index);
	}

	inline void operator++() {
		index++;
	}

	inline const Bool operator!=(const IncludeAndImportsIter other) const {
		return neq(index, other.index);
	}
};

struct IncludeAndImportsRange {
	const Opt<const Module*> include;
	const Arr<const Module*> imports;

	inline IncludeAndImportsIter begin() const {
		return IncludeAndImportsIter{include, imports, include.has() ? -1 : 0};
	}

	inline IncludeAndImportsIter end() {
		return IncludeAndImportsIter{include, imports, static_cast<ssize_t>(imports.size)};
	}
};

struct CheckCtx {
	Arena& arena;
	const PathAndStorageKind path;
	const Opt<const Module*> include;
	const Arr<const Module*> imports;
	ArrBuilder<const Diagnostic> diagsBuilder {};

	CheckCtx(const CheckCtx&) = delete;

	inline void diag(const SourceRange range, const Diag diag) {
		diagsBuilder.add(arena, Diagnostic{path, range, diag});
	}

	inline const Bool hasDiags() const {
		return _not(isEmpty(diagsBuilder));
	}

	inline const Arr<const Diagnostic> diags() {
		return diagsBuilder.finish();
	}

	inline IncludeAndImportsRange includeAndImportsRange() const {
		return IncludeAndImportsRange{include, imports};
	}

	inline const Str copyStr(const Str s) const {
		return ::copyStr(arena, s);
	}
};
