#include "./frontendCompile.h"

#include "../util/arrUtil.h"
#include "../util/dictBuilder.h"
#include "../util/mutDict.h"
#include "../util/mutSet.h"
#include "../util/resultUtil.h"
#include "./check.h"
#include "./parse.h"

namespace {
	const PathAndStorageKind includePath(Arena* arena) {
		return PathAndStorageKind{rootPath(arena, strLiteral("include.nz")), StorageKind::global};
	}

	const Opt<const NulTerminatedStr> getFile(Arena* fileArena, const PathAndStorageKind pk, const ReadOnlyStorages storages) {
		return storages.choose(pk.storageKind).tryReadFile(fileArena, pk.path);
	}

	const Arr<const Diagnostic> parseDiagnostics(Arena* modelArena, const PathAndStorageKind where, const ParseDiagnostic p) {
		return arrLiteral<const Diagnostic>(modelArena, Diagnostic{where, p.range, Diag{p.diag}});
	}

	using LineAndColumnGettersBuilder = DictBuilder<const PathAndStorageKind, const LineAndColumnGetter, comparePathAndStorageKind>;

	const Result<const FileAst, const Arr<const Diagnostic>> parseSingle(
		Arena* modelArena,
		Arena* astsArena,
		Symbols* symbols,
		const PathAndStorageKind where,
		const ReadOnlyStorages storages,
		LineAndColumnGettersBuilder* lineAndColumnGetters
	) {
		// File content must go in astsArena because we refer to strings without copying
		const Opt<const NulTerminatedStr> opFileContent = getFile(astsArena, where, storages);
		if (has(opFileContent)) {
			const NulTerminatedStr text = force(opFileContent);
			addToDict<const PathAndStorageKind, const LineAndColumnGetter, comparePathAndStorageKind>(modelArena, lineAndColumnGetters, where, lineAndColumnGetterForText(modelArena, stripNulTerminator(text)));
			return mapFailure<const Arr<const Diagnostic>>{}(
				parseFile(astsArena, symbols, text),
				[&](const ParseDiagnostic p) { return parseDiagnostics(modelArena, where, p); });
		}
		else
			return failure<const FileAst, const Arr<const Diagnostic>>(
				arrLiteral<const Diagnostic>(modelArena, Diagnostic{where, SourceRange::empty(), Diag{Diag::FileDoesNotExist{}}}));
	}

	struct PathAndAst {
		const PathAndStorageKind pathAndStorageKind;
		const FileAst ast;
	};

	struct IncludeAndPathAndAsts {
		const IncludeCheck includeCheck;
		const Arr<const PathAndAst> pathAndAsts;
	};

	// NOTE: does not ensure that the file exists. Only returns none for a relative import that tries climbing past the root.
	const Opt<const PathAndStorageKind> resolveImport(Arena* modelArena, const PathAndStorageKind from, const ImportAst ast) {
		const Path* path = copyPath(modelArena, ast.path);
		if (ast.nDots == 0)
			return some<const PathAndStorageKind>(PathAndStorageKind{path, StorageKind::global});
		else {
			const Opt<const Path*> rel = resolvePath(modelArena, from.path, RelPath{ast.nDots - 1, path});
			return has(rel)
				? some<const PathAndStorageKind>(PathAndStorageKind{force(rel), from.storageKind})
				: none<const PathAndStorageKind>();
		}
	}

	const Result<const Arr<const PathAndAst>, const Arr<const Diagnostic>> parseEverything(
		Arena* modelArena,
		Arena* astsArena,
		Symbols* symbols,
		const Path* mainPath,
		ReadOnlyStorages storages,
		LineAndColumnGettersBuilder* lineAndColumnGetters
	) {
		Arena tempArena;

		ArrBuilder<const PathAndAst> res {};
		MutArr<const PathAndStorageKind> toParse {};
		// Set of all modules seen, either in 'res' or 'toParse'.
		MutSet<const PathAndStorageKind, comparePathAndStorageKind> seenSet {};

		const PathAndStorageKind firstPathAndStorageKind = PathAndStorageKind{mainPath, StorageKind::local};
		push<const PathAndStorageKind>(&tempArena, toParse, firstPathAndStorageKind);
		seenSet.add(&tempArena, firstPathAndStorageKind);

		for (;;) {
			const Opt<const PathAndStorageKind> opPath = pop(toParse);
			if (!has(opPath))
				break;

			const PathAndStorageKind path = force(opPath);
			const Result<const FileAst, const Arr<const Diagnostic>> parseResult = parseSingle(modelArena, astsArena, symbols, path, storages, lineAndColumnGetters);
			if (!parseResult.isSuccess())
				return failure<const Arr<const PathAndAst>, const Arr<const Diagnostic>>(parseResult.asFailure());

			add<const PathAndAst>(astsArena, &res, PathAndAst{path, parseResult.asSuccess()});

			for (const ImportAst i : parseResult.asSuccess().imports) {
				const Opt<const PathAndStorageKind> opDependencyPath = resolveImport(modelArena, path, i);
				if (!has(opDependencyPath))
					todo<void>("diagnostic: import resolution failed");
				const PathAndStorageKind dependencyPath = force(opDependencyPath);
				if (seenSet.tryAdd(&tempArena, dependencyPath))
					push<const PathAndStorageKind>(&tempArena, toParse, dependencyPath);
			}
		}

		return success<const Arr<const PathAndAst>, const Arr<const Diagnostic>>(finishArr(&res));
	}

	const Result<const Arr<const Module*>, const Arr<const Diagnostic>> getImports(
		Arena* modelArena,
		const Arr<const ImportAst> imports,
		const PathAndStorageKind curPath,
		const MutDict<const PathAndStorageKind, const Module*, comparePathAndStorageKind>& compiled
	) {
		return mapOrFail<const Module*, const Arr<const Diagnostic>>{}(modelArena, imports, [&](const ImportAst ast) {
			// resolveImport should succeed because we already did this in parseEverything. (TODO: then keep it around with the ast?)
			const PathAndStorageKind importPath = force(resolveImport(modelArena, curPath, ast));
			const Opt<const Module*> i = getAt_mut<const PathAndStorageKind, const Module*, comparePathAndStorageKind>(&compiled, importPath);
			if (has(i))
				return success<const Module*, const Arr<const Diagnostic>>(force(i));
			else {
				const Diagnostic diag = Diagnostic{curPath, ast.range, Diag{Diag::CircularImport{}}};
				return failure<const Module*, const Arr<const Diagnostic>>(arrLiteral<const Diagnostic>(modelArena, diag));
			}
		});
	}
}

const Result<const Program, const Diagnostics> frontendCompile(
	Arena* modelArena,
	Symbols* symbols,
	const ReadOnlyStorages storages,
	const Path* mainPath
) {
	// NOTE: use modelArena for paths since those are stored along with the module
	Arena astsArena {};
	Arena tempArena {};
	LineAndColumnGettersBuilder lineAndColumnGetters {};

	const PathAndStorageKind inclPath = includePath(modelArena);

	const Result<const IncludeCheck, const Arr<const Diagnostic>> include = flatMapSuccess<const IncludeCheck, const Arr<const Diagnostic>>{}(
		parseSingle(modelArena, &astsArena, symbols, inclPath, storages, &lineAndColumnGetters),
		[&](const FileAst ast) {
			return checkIncludeNz(modelArena, ast, inclPath);
		});

	const Result<const IncludeAndPathAndAsts, const Arr<const Diagnostic>> includeAndParsed = flatMapSuccess<const IncludeAndPathAndAsts, const Arr<const Diagnostic>>{}(
		include,
		[&](const IncludeCheck includeCheck) {
			return mapSuccess<const IncludeAndPathAndAsts>{}(
				parseEverything(modelArena, &astsArena, symbols, mainPath, storages, &lineAndColumnGetters),
				[&](const Arr<const PathAndAst> everything) {
					return IncludeAndPathAndAsts{includeCheck, everything};
				});
		});

	// TODO: less nested callbacks, more intermediate Result values
	const Result<const Program, const Arr<const Diagnostic>> res = flatMapSuccess<const Program, const Arr<const Diagnostic>>{}(includeAndParsed, [&](const IncludeAndPathAndAsts pair) {
		const IncludeCheck includeCheck = pair.includeCheck;
		const Arr<const PathAndAst> fileAsts = pair.pathAndAsts;

		// Go in reverse order (starting at leaf dependencies, finishing with mainPath)
		// If we ever see some dependency that's not compiled yet, it indicates a circular dependency.
		MutDict<const PathAndStorageKind, const Module*, comparePathAndStorageKind> compiled {};
		const Result<const Arr<const Module*>, const Arr<const Diagnostic>> modules = mapOrFailReverse<const Module*, const Arr<const Diagnostic>>{}(
			modelArena,
			fileAsts,
			[&](const PathAndAst ast) {
				const Result<const Arr<const Module*>, const Arr<const Diagnostic>> resultImports = getImports(modelArena, ast.ast.imports, ast.pathAndStorageKind, compiled);
				return flatMapSuccess<const Module*, const Arr<const Diagnostic>>{}(
					resultImports,
					[&](const Arr<const Module*> imports) {
						const Result<const Module*, const Arr<const Diagnostic>> res = check(modelArena, imports, ast.ast, ast.pathAndStorageKind, includeCheck);
						if (res.isSuccess())
							addToDict<const PathAndStorageKind, const Module*, comparePathAndStorageKind>(&tempArena, &compiled, ast.pathAndStorageKind, res.asSuccess());
						return res;
					});
			});

		return mapSuccess<const Program>{}(modules, [&](const Arr<const Module*> modules) {
			const Arr<const Module*> allModules = prepend<const Module*>(modelArena, includeCheck.module, modules);
			const LineAndColumnGetters lc = finishDictShouldBeNoConflict<const PathAndStorageKind, const LineAndColumnGetter, comparePathAndStorageKind>(&lineAndColumnGetters);
			return Program{includeCheck.module, last(modules), allModules, includeCheck.commonTypes, lc};
		});
	});

	return mapFailure<const Diagnostics>{}(res, [&](const Arr<const Diagnostic> d) {
		return Diagnostics{d, FilesInfo{storages.absolutePathsGetter(), finishDictShouldBeNoConflict<const PathAndStorageKind, const LineAndColumnGetter, comparePathAndStorageKind>(&lineAndColumnGetters)}};
	});
}
