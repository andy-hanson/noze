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
		return index == -1 ? force(include) : at(imports, index);
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
		return IncludeAndImportsIter{include, imports, has(include) ? -1 : 0};
	}

	inline IncludeAndImportsIter end() {
		return IncludeAndImportsIter{include, imports, static_cast<ssize_t>(size(imports))};
	}
};

struct CheckCtx {
	Arena* arena;
	const PathAndStorageKind path;
	const Opt<const Module*> include;
	const Arr<const Module*> imports;
	ArrBuilder<const Diagnostic> diagsBuilder {};

	CheckCtx(const CheckCtx&) = delete;
};

inline void addDiag(CheckCtx* ctx, const SourceRange range, const Diag diag) {
	add<const Diagnostic>(ctx->arena, &ctx->diagsBuilder, Diagnostic{ctx->path, range, diag});
}

inline const Bool hasDiags(const CheckCtx* ctx) {
	return _not(arrBuilderIsEmpty(&ctx->diagsBuilder));
}

inline const Arr<const Diagnostic> diags(CheckCtx* ctx) {
	return finishArr(&ctx->diagsBuilder);
}

inline IncludeAndImportsRange includeAndImportsRange(const CheckCtx* ctx) {
	return IncludeAndImportsRange{ctx->include, ctx->imports};
}
