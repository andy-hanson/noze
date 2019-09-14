#pragma once

#include "./ast.h"
#include "../diag.h" // ParseDiag
#include "../model.h"

const Result<const FileAst, const ParseDiagnostic> parseFile(
	Arena* astArena,
	AllSymbols* allSymbols,
	const NulTerminatedStr source);
