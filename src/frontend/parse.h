#pragma once

#include "./ast.h"
#include "../diag.h" // ParseDiag
#include "../model.h"

// Only used temporarily while parsing
struct ParseDiagnostic {
	const SourceRange range;
	const ParseDiag diag;
};

const Result<const FileAst, const ParseDiagnostic> parseFile(
	Arena* astArena,
	AllSymbols* allSymbols,
	const NulTerminatedStr source);
