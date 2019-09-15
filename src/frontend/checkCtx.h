#pragma once

#include "../diag.h"
#include "../model.h"
#include "../util/arrUtil.h" // copyStr

#include "./programState.h"

struct CheckCtx {
	Arena* arena;
	ProgramState* programState;
	const PathAndStorageKind path;
	const Arr<const Module*> allFlattenedImports;
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
