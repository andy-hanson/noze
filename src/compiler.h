#include "./util.h"
#include "./util/io.h" // Environ
#include "./util/path.h"

// These return program exit codes

int build(
	const AbsolutePath nozeDir,
	const AbsolutePath programDir,
	const Path* mainPath,
	const Environ environ);

int buildAndRun(
	const AbsolutePath nozeDir,
	const AbsolutePath programDir,
	const Path* mainPath,
	const Arr<const Str> programArgs,
	const Environ environ);
