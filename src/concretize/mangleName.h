#pragma once

#include "../util/sym.h"
#include "../util/writer.h"

const Str mangleName(Arena* arena, const Sym name);

const Str mangleExternFunName(Arena* arena, const Sym name);

void writeMangledName(Writer* writer, const Sym name);

const Bool isMangledName(const Str name);
