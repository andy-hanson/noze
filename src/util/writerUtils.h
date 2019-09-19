#pragma once

#include "./lineAndColumnGetter.h"
#include "./path.h"
#include "./sym.h"
#include "./writer.h"

void writePath(Writer* writer, const Path* p);

void writeRelPath(Writer* writer, const RelPath p);

void writePathAndStorageKind(Writer* writer, const PathAndStorageKind p);

void writeLineAndColumn(Writer* writer, const LineAndColumn lc);

void writePos(Writer* writer, const LineAndColumnGetter lc, const Pos pos);

void writeRange(Writer* writer, const LineAndColumnGetter lc, const SourceRange range);

void showChar(Writer* writer, char c);

void writeName(Writer* writer, const Sym name);

inline void writeNl(Writer* writer) {
	writeChar(writer, '\n');
}

void writeSpaces(Writer* writer, const size_t nSpaces);

inline void writeIndent(Writer* writer) {
	writeSpaces(writer, 2);
}

inline void writeNlIndent(Writer* writer) {
	writeNl(writer);
	writeIndent(writer);
}

void writeSymPadded(Writer* writer, const Sym name, const size_t size);
