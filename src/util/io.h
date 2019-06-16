#pragma once

#include "../util.h"
#include "../Path.h"

const Opt<const NulTerminatedStr> tryReadFile(Arena& arena, const Path* path);
void writeFileSync(const Path* path, const Str content);
const Path* getCwd(Arena& arena);
