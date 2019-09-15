#pragma once

#include "../diag.h"
#include "../model.h"

#include "./ast.h"
#include "./programState.h"

struct PathAndAst {
	const PathAndStorageKind pathAndStorageKind;
	const FileAst ast;
};

struct BootstrapCheck {
	const Module* module;
	const CommonTypes commonTypes;
};

const Result<const BootstrapCheck, const Diags> checkBootstrapNz(
	Arena* arena,
	ProgramState* programState,
	const PathAndAst pathAndAst);

const Result<const Module*, const Diags> check(
	Arena* arena,
	ProgramState* programState,
	const Arr<const Module*> imports,
	const Arr<const Module*> exports,
	const PathAndAst pathAndAst,
	const CommonTypes commonTypes);
