#include "./compiler.h"

#include <stdio.h>

#include "./backend/writeToC.h"
#include "./concreteModel.h"
#include "./diag.h"
#include "./concretize/concretize.h"
#include "./frontend/frontendCompile.h"
#include "./frontend/readOnlyStorage.h"
#include "./frontend/showDiag.h"
#include "./model.h"
#include "./util/arrUtil.h"

namespace {
	void emitProgram(const Program program, const AbsolutePath cppPath) {
		Arena concreteArena {};
		const ConcreteProgram concreteProgram = concretize(concreteArena, program);
		Arena writeArena {};
		const Str emitted = writeToC(writeArena, concreteProgram);
		writeFileSync(cppPath, emitted);
	}

	void compileCpp(const AbsolutePath cppPath, const AbsolutePath exePath, const Environ environ) {
		Arena arena {};
		// TODO: option to use g++
		const AbsolutePath cppCompiler = childPath(
			arena,
			AbsolutePath{rootPath(arena, strLiteral("usr"))},
			strLiteral("bin"),
			strLiteral("c++"));
		const Arr<const Str> args = arrLiteral<const Str>(arena, {
			strLiteral("-Werror"),
			strLiteral("-Wextra"),
			strLiteral("-Wall"),
			strLiteral("-ansi"),
			strLiteral("-pedantic"),
			strLiteral("-Wno-unused-parameter"),
			strLiteral("-Wno-unused-but-set-variable"),
			strLiteral("-Wno-unused-variable"),
			strLiteral("-std=c++17"),
			// TODO: configurable whether we want debug or release
			strLiteral("-g"),
			pathToStr(arena, cppPath),
			strLiteral("-o"),
			pathToStr(arena, exePath)
		});
		int err = spawnAndWaitSync(cppCompiler, args, environ);
		if (err != 0) {
			printf("c++ compile error! Exit code: %d\n", err);
			todo<void>("compile error");
		}
	}

	// mainPath is relative to programDir
	const Opt<const AbsolutePath> buildWorker(
		const AbsolutePath nozeDir,
		const AbsolutePath programDir,
		const Path* mainPath,
		const Environ environ
	) {
		Arena modelArena {};
		const AbsolutePath include = childPath(modelArena, nozeDir, strLiteral("include"));
		const ReadOnlyStorages storages = ReadOnlyStorages{ReadOnlyStorage{include}, ReadOnlyStorage{programDir}};
		const Result<const Program, const Diagnostics> programResult = frontendCompile(modelArena, storages, mainPath);

		return programResult.match(
			[&](const Program program) {
				const AbsolutePath fullMainPath = addManyChildren(modelArena, programDir, mainPath);
				const AbsolutePath cppPath = changeExtension(modelArena, fullMainPath, strLiteral("cpp"));
				emitProgram(program, cppPath);
				const AbsolutePath exePath = removeExtension(modelArena, fullMainPath);
				compileCpp(cppPath, exePath, environ);
				return some<const AbsolutePath>(exePath);
			},
			[](const Diagnostics diagnostics) {
				printDiagnostics(diagnostics);
				return none<const AbsolutePath>();
			});
	}
}

int build(
	const AbsolutePath nozeDir,
	const AbsolutePath programDir,
	const Path* mainPath,
	const Environ environ
) {
	const Opt<const AbsolutePath> b = buildWorker(nozeDir, programDir, mainPath, environ);
	return b.has() ? 0 : 1;
}

int buildAndRun(
	const AbsolutePath nozeDir,
	const AbsolutePath programDir,
	const Path* mainPath,
	const Arr<const Str> programArgs,
	const Environ environ
) {
	const Opt<const AbsolutePath> exePath = buildWorker(nozeDir, programDir, mainPath, environ);
	if (exePath.has())
		return spawnAndWaitSync(exePath.force(), programArgs, environ);
	else
		return 1;
}
