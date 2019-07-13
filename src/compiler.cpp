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
	void emitProgram(const Program program, const AbsolutePath cPath) {
		Arena concreteArena {};
		const ConcreteProgram concreteProgram = concretize(&concreteArena, program);
		Arena writeArena {};
		const Str emitted = writeToC(&writeArena, concreteProgram);
		writeFileSync(cPath, emitted);
	}

	void compileC(const AbsolutePath cPath, const AbsolutePath exePath, const Environ environ) {
		Arena arena {};
		// TODO: option to use g++
		const AbsolutePath cCompiler = childPath(
			&arena,
			AbsolutePath{rootPath(&arena, strLiteral("usr"))},
			strLiteral("bin"),
			strLiteral("cc"));
		const Arr<const Str> args = arrLiteral<const Str>(&arena, {
			strLiteral("-Werror"),
			strLiteral("-Wextra"),
			strLiteral("-Wall"),
			strLiteral("-ansi"),
			strLiteral("-pedantic"),
			strLiteral("-std=c11"),
			strLiteral("-Wno-unused-parameter"),
			strLiteral("-Wno-unused-but-set-variable"),
			strLiteral("-Wno-unused-variable"),
			strLiteral("-Wno-unused-value"),
			// TODO: configurable whether we want debug or release
			strLiteral("-g"),
			pathToStr(&arena, cPath),
			strLiteral("-o"),
			pathToStr(&arena, exePath)
		});
		int err = spawnAndWaitSync(cCompiler, args, environ);
		if (err != 0) {
			printf("c++ compile error! Exit code: %d\n", err);
			todo<void>("compile error");
		}
	}

	// mainPath is relative to programDir
	// Returns exePath
	const Opt<const AbsolutePath> buildWorker(
		Arena* outputArena, // Just for exePath
		const AbsolutePath nozeDir,
		const AbsolutePath programDir,
		const Path* mainPath,
		const Environ environ
	) {
		Arena modelArena {};
		Symbols symbols {};
		const AbsolutePath include = childPath(&modelArena, nozeDir, strLiteral("include"));
		const ReadOnlyStorages storages = ReadOnlyStorages{ReadOnlyStorage{include}, ReadOnlyStorage{programDir}};
		const Result<const Program, const Diagnostics> programResult = frontendCompile(&modelArena, &symbols, storages, mainPath);

		return programResult.match(
			[&](const Program program) {
				const AbsolutePath fullMainPath = addManyChildren(outputArena, programDir, mainPath);
				const AbsolutePath cPath = changeExtension(&modelArena, fullMainPath, strLiteral("c"));
				emitProgram(program, cPath);
				const AbsolutePath exePath = removeExtension(outputArena, fullMainPath);
				compileC(cPath, exePath, environ);
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
	Arena exePathArena {};
	const Opt<const AbsolutePath> exePath = buildWorker(&exePathArena, nozeDir, programDir, mainPath, environ);
	return has(exePath) ? 0 : 1;
}

int buildAndRun(
	const AbsolutePath nozeDir,
	const AbsolutePath programDir,
	const Path* mainPath,
	const Arr<const Str> programArgs,
	const Environ environ
) {
	Arena exePathArena {};
	const Opt<const AbsolutePath> exePath = buildWorker(&exePathArena, nozeDir, programDir, mainPath, environ);
	return has(exePath)
		? spawnAndWaitSync(force(exePath), programArgs, environ)
		: 1;
}
