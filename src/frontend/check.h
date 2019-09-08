#pragma once

#include "../diag.h"
#include "../model.h"

#include "./ast.h"
#include "./programState.h"

struct PathAndAst {
	const PathAndStorageKind pathAndStorageKind;
	const FileAst ast;
};

struct IncludeCheck {
	const Module* module;
	const CommonTypes commonTypes;
};

const Result<const IncludeCheck, const Arr<const Diagnostic>> checkIncludeNz(
	Arena* arena,
	ProgramState* programState,
	const PathAndAst pathAndAst);

const Result<const Module*, const Arr<const Diagnostic>> check(
	Arena* arena,
	ProgramState* programState,
	const Arr<const Module*> imports,
	const PathAndAst pathAndAst,
	const IncludeCheck includeCheck);
