#include "./writerUtils.h"

void writePath(Writer* writer, const Path* p) {
	if (has(p->parent)) {
		writePath(writer, force(p->parent));
		writeChar(writer, '/');
	}
	writeStr(writer, p->baseName);
}

void writeRelPath(Writer* writer, const RelPath p) {
	repeat(p.nParents, [&]() {
		writeStatic(writer, "../");
	});
	writePath(writer, p.path);
}

void writePathAndStorageKind(Writer* writer, const PathAndStorageKind p) {
	writePath(writer, p.path);
}

//TODO:MOVE
void writeLineAndColumn(Writer* writer, const LineAndColumn lc) {
	writeNat(writer, lc.line + 1);
	writeChar(writer, ':');
	writeNat(writer, lc.column + 1);
}

void writePos(Writer* writer, const LineAndColumnGetter lc, const Pos pos) {
	writeLineAndColumn(writer, lineAndColumnAtPos(lc, pos));
}

void writeRange(Writer* writer, const LineAndColumnGetter lc, const SourceRange range) {
	writePos(writer, lc, range.start);
	writeChar(writer, '-');
	writePos(writer, lc, range.end);
}

void showChar(Writer* writer, char c) {
	switch (c) {
		case '\0':
			writeStatic(writer, "\\0");
			break;
		case '\n':
			writeStatic(writer, "\\n");
			break;
		case '\t':
			writeStatic(writer, "\\t");
			break;
		default:
			writeChar(writer, c);
			break;
	}
}

void writeName(Writer* writer, const Sym name) {
	writeChar(writer, '\'');
	writeSym(writer, name);
	writeChar(writer, '\'');
}

void writeSpaces(Writer* writer, const size_t nSpaces) {
	repeat(nSpaces, [&]() {
		writeChar(writer, ' ');
	});
}

void writeSymPadded(Writer* writer, const Sym name, const size_t size) {
	const size_t symSize = writeSymAndGetSize(writer, name);
	assert(symSize < size);
	writeSpaces(writer, size - symSize);
}
