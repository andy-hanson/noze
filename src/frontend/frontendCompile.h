#pragma once

#include "../diag.h"
#include "../model.h"
#include "./readOnlyStorage.h"

const Result<const Program, const Diagnostics> frontendCompile(
	Arena* modelArena,
	Symbols* symbols,
	const ReadOnlyStorages storages,
	const Path* mainPath);
