#pragma once

#include "../util.h"
#include "../util/path.h"

const Bool fileExists(const AbsolutePath path);
const Opt<const NulTerminatedStr> tryReadFile(Arena& arena, const AbsolutePath path);
void writeFileSync(const AbsolutePath path, const Str content);

using Environ = const Arr<const KeyValuePair<const Str, const Str>>;

// Returns the child process' error code.
// WARN: A first arg will be prepended that is the executable path.
int spawnAndWaitSync(
	const AbsolutePath executable,
	const Arr<const Str> args,
	const Environ environ);

struct CommandLineArgs {
	const AbsolutePath cwd;
	const AbsolutePath pathToThisExecutable;
	const Arr<const Str> args;
	const Environ environ;
};
const CommandLineArgs parseArgs(Arena& arena, const int argc, CStr const* const argv);

const Environ getEnviron(Arena& arena);
