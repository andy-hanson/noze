#pragma once

#include "../diag.h"
#include "../model.h"
#include "./ast.h"

struct IncludeCheck {
	const Module* module;
	const CommonTypes commonTypes;
};

const Result<const IncludeCheck, const Arr<const Diagnostic>> checkIncludeNz(
	Arena* arena, const FileAst ast, const PathAndStorageKind path);

const Result<const Module*, const Arr<const Diagnostic>> check(
	Arena* arena,
	const Arr<const Module*> imports,
	const FileAst ast,
	const PathAndStorageKind path,
	const IncludeCheck includeCheck);
