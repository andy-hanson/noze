#pragma once

#include "../util/writer.h"

const Str mangleName(Arena& arena, const Str declName);

void writeMangledName(Writer& writer, const Str name);
