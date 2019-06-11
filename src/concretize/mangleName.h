#pragma once

#include "../util.h"

#include "./writer.h"

const Str mangleName(Arena& arena, const Str declName);

void writeMangledName(Writer& writer, const Str name);
