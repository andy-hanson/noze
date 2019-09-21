#include "./frontendCompile.h"

#include "../util/arrUtil.h"
#include "../util/dictBuilder.h"
#include "../util/mutDict.h"
#include "../util/mutQueue.h"
#include "../util/mutSet.h"
#include "../util/resultUtil.h"
#include "../instantiate.h"
#include "./check.h"
#include "./parse.h"

namespace {
	const PathAndStorageKind pathInInclude(Arena* arena, const Str name) {
		return PathAndStorageKind{rootPath(arena, name), StorageKind::global};
	}

	const PathAndStorageKind bootstrapPath(Arena* arena) {
		return pathInInclude(arena, strLiteral("bootstrap.nz"));
	}

	const PathAndStorageKind stdPath(Arena* arena) {
		return pathInInclude(arena, strLiteral("std.nz"));
	}

	const PathAndStorageKind gcPath(Arena* arena) {
		return pathInInclude(arena, strLiteral("gc.nz"));
	}

	const PathAndStorageKind runtimePath(Arena* arena) {
		return pathInInclude(arena, strLiteral("runtime.nz"));
	}

	const PathAndStorageKind runtimeMainPath(Arena* arena) {
		return pathInInclude(arena, strLiteral("runtime-main.nz"));
	}

	const Opt<const NulTerminatedStr> getFile(
		Arena* fileArena,
		const PathAndStorageKind pk,
		const ReadOnlyStorages storages
	) {
		return storages.choose(pk.storageKind).tryReadFile(fileArena, pk.path);
	}

	const Diags parseDiagnostics(
		Arena* modelArena,
		const PathAndStorageKind where,
		const ParseDiagnostic p
	) {
		return arrLiteral<const Diagnostic>(modelArena, { Diagnostic{where, p.range, Diag{p.diag}} });
	}

	using LineAndColumnGettersBuilder =
		DictBuilder<const PathAndStorageKind, const LineAndColumnGetter, comparePathAndStorageKind>;

	void addEmptyLineAndColumnGetter(
		Arena* arena,
		LineAndColumnGettersBuilder* lineAndColumnGetters,
		const PathAndStorageKind where
	) {
		// Even a non-existent path needs LineAndColumnGetter since that's where the diagnostic is
		addToDict<
			const PathAndStorageKind,
			const LineAndColumnGetter,
			comparePathAndStorageKind
		>(
			arena,
			lineAndColumnGetters,
			where,
			lineAndColumnGetterForEmptyFile(arena));
	}

	const Result<const FileAst, const Diags> parseSingle(
		Arena* modelArena,
		Arena* astsArena,
		AllSymbols* allSymbols,
		const Opt<const PathAndStorageKindAndRange> importedFrom,
		const PathAndStorageKind where,
		const ReadOnlyStorages storages,
		LineAndColumnGettersBuilder* lineAndColumnGetters
	) {
		// File content must go in astsArena because we refer to strings without copying
		const Opt<const NulTerminatedStr> opFileContent = getFile(astsArena, where, storages);
		if (has(opFileContent)) {
			const NulTerminatedStr text = force(opFileContent);
			addToDict<
				const PathAndStorageKind,
				const LineAndColumnGetter,
				comparePathAndStorageKind
			>(
				modelArena,
				lineAndColumnGetters,
				where,
				lineAndColumnGetterForText(modelArena, stripNulTerminator(text)));
			return mapFailure<const Diags>{}(
				parseFile(astsArena, allSymbols, text),
				[&](const ParseDiagnostic p) { return parseDiagnostics(modelArena, where, p); });
		} else {
			const Bool isImport = has(importedFrom);
			const PathAndStorageKindAndRange diagWhere = isImport
				? force(importedFrom)
				: PathAndStorageKindAndRange{where, SourceRange::empty()};
			if (!isImport)
				addEmptyLineAndColumnGetter(modelArena, lineAndColumnGetters, where);
			const Diag::FileDoesNotExist::Kind kind = isImport
				? Diag::FileDoesNotExist::Kind::import
				: Diag::FileDoesNotExist::Kind::root;
			return failure<const FileAst, const Diags>(
				arrLiteral<const Diagnostic>(
					modelArena,
					{ Diagnostic{diagWhere, Diag{Diag::FileDoesNotExist{kind, where}}} }));
		}
	}

	struct ResolvedImport {
		// This is arbitrarily the first module we saw to import this.
		// This is just used for error reporting in case the file can't be read.
		const PathAndStorageKindAndRange importedFrom;
		const PathAndStorageKind resolvedPath;
	};

	// NOTE: does not ensure that the file exists.
	// Only returns none for a relative import that tries climbing past the root.
	const Result<const ResolvedImport, const Diags> tryResolveImport(
		Arena* modelArena,
		const PathAndStorageKind from,
		const ImportAst ast
	) {
		const Path* path = copyPath(modelArena, ast.path); //TODO: shouldn't be necessary to copy?
		const PathAndStorageKindAndRange importedFrom = PathAndStorageKindAndRange{from, ast.range};
		if (ast.nDots == 0)
			return success<const ResolvedImport, const Diags>(
				ResolvedImport{importedFrom, PathAndStorageKind{path, StorageKind::global}});
		else {
			const RelPath relPath = RelPath{ast.nDots - 1, path};
			const Opt<const Path*> rel = resolvePath(modelArena, from.path->parent, relPath);
			return has(rel)
				? success<const ResolvedImport, const Diags>(
					ResolvedImport{importedFrom, PathAndStorageKind{force(rel), from.storageKind}})
				: failure<const ResolvedImport, const Diags>(
					arrLiteral<const Diagnostic>(
						modelArena,
						{ Diagnostic{from, ast.range, Diag{Diag::RelativeImportReachesPastRoot{relPath}}} }));
		}
	}

	struct ImportAndExportPaths {
		const Arr<const ResolvedImport> imports;
		const Arr<const ResolvedImport> exports;
	};

	const Result<const Arr<const ResolvedImport>, const Diags> resolveImportsOrExports(
		Arena* modelArena,
		Arena* astsArena,
		const PathAndStorageKind from,
		const Arr<const ImportAst> importsOrExports
	) {
		return mapOrFail<const ResolvedImport, const Diags>{}(
			astsArena,
			importsOrExports,
			[&](const ImportAst i) {
				return tryResolveImport(modelArena, from, i);
			});
	}

	const Result<const ImportAndExportPaths, const Diags> resolveImportsAndExports(
		Arena* modelArena,
		Arena* astsArena,
		const PathAndStorageKind from,
		const Arr<const ImportAst> imports,
		const Arr<const ImportAst> exports
	) {
		return joinResults<const ImportAndExportPaths, const Diags>{}(
			resolveImportsOrExports(modelArena, astsArena, from, imports),
			resolveImportsOrExports(modelArena, astsArena, from, exports),
			[](
				const Arr<const ResolvedImport> resolvedImports,
				const Arr<const ResolvedImport> resolvedExports
			) {
				return ImportAndExportPaths{resolvedImports, resolvedExports};
			});
	}

	struct PathAndAstAndResolvedImports {
		const PathAndStorageKind pathAndStorageKind;
		const FileAst ast;
		const Arr<const PathAndStorageKind> resolvedImports;
		const Arr<const PathAndStorageKind> resolvedExports;

		inline PathAndAst pathAndAst() const {
			return PathAndAst{pathAndStorageKind, ast};
		}
	};

	const Arr<const PathAndStorageKind> stripRange(Arena* arena, const Arr<const ResolvedImport> a) {
		return map<const PathAndStorageKind>{}(arena, a, [](const ResolvedImport i) {
			return i.resolvedPath;
		});
	}

	enum class ParseStatus {
		started,
		finished
	};

	const Opt<const Diags> parseRecur(
		Arena* modelArena,
		Arena* astsArena,
		AllSymbols* allSymbols,
		ReadOnlyStorages storages,
		LineAndColumnGettersBuilder* lineAndColumnGetters,
		ArrBuilder<const PathAndAstAndResolvedImports>* res,
		MutDict<const PathAndStorageKind, const ParseStatus, comparePathAndStorageKind>* statuses,
		const Opt<const PathAndStorageKindAndRange> importedFrom,
		const PathAndStorageKind path
	) {
		setInDict<const PathAndStorageKind, const ParseStatus, comparePathAndStorageKind>(
			astsArena, statuses, path, ParseStatus::started);

		// Parse it now
		const Result<const FileAst, const Diags> parseResult =
			parseSingle(modelArena, astsArena, allSymbols, importedFrom, path, storages, lineAndColumnGetters);
		return parseResult.match(
			[&](const FileAst ast) {
				const Result<const ImportAndExportPaths, const Diags> importsResult =
					resolveImportsAndExports(modelArena, astsArena, path, ast.imports, ast.exports);
				return importsResult.match(
					[&](const ImportAndExportPaths importsAndExports) {
						// Ensure all imports are added before adding this
						const Arr<const ResolvedImport> importsAndExportsArr =
							cat(astsArena, importsAndExports.imports, importsAndExports.exports);
						for (const ResolvedImport import : importsAndExportsArr) {
							const Opt<const ParseStatus> status =
								getAt_mut<const PathAndStorageKind, const ParseStatus, comparePathAndStorageKind>(
									statuses, import.resolvedPath);
							if (has(status)) {
								switch (force(status)) {
									case ParseStatus::started: {
										const Diagnostic diag = Diagnostic{
											import.importedFrom,
											Diag{Diag::CircularImport{path, import.resolvedPath}}};
										return some<const Diags>(
											arrLiteral<const Diagnostic>(modelArena, { diag }));
									}
									case ParseStatus::finished:
										break;
									default:
										assert(0);
								}
							} else {
								const Opt<const Diags> err = parseRecur(
									modelArena,
									astsArena,
									allSymbols,
									storages,
									lineAndColumnGetters,
									res,
									statuses,
									some<const PathAndStorageKindAndRange>(import.importedFrom),
									import.resolvedPath);
								if (has(err))
									return err;
							}
						}
						const PathAndAstAndResolvedImports pa = PathAndAstAndResolvedImports{
							path,
							ast,
							stripRange(astsArena, importsAndExports.imports),
							stripRange(astsArena, importsAndExports.exports)};
						add<const PathAndAstAndResolvedImports>(astsArena, res, pa);
						setInDict<const PathAndStorageKind, const ParseStatus, comparePathAndStorageKind>(
							astsArena,
							statuses,
							path,
							ParseStatus::finished);
						return none<const Diags>();
					},
					[](const Diags d) {
						return some<const Diags>(d);
					});
			},
			[](const Diags d) {
				return some<const Diags>(d);
			});
	};

	using AllAsts = const Arr<const PathAndAstAndResolvedImports>;

	// Starts at 'main' and recursively parses all imports too.
	// Result will be in import order -- asts at lower indices are imported by asts at higher indices.
	// So, don't have to worry about circularity when checking.
	const Result<const AllAsts, const Diags> parseEverythingWorker(
		Arena* modelArena,
		Arena* astsArena,
		AllSymbols* allSymbols,
		const PathAndStorageKind mainPath,
		ReadOnlyStorages storages,
		LineAndColumnGettersBuilder* lineAndColumnGetters
	) {
		if (endsWith(mainPath.path->baseName, strLiteral(".nz"))) {
			ArrBuilder<const PathAndAstAndResolvedImports> res {};
			MutDict<const PathAndStorageKind, const ParseStatus, comparePathAndStorageKind> statuses {};
			const Arr<const PathAndStorageKind> rootPaths =  arrLiteral<const PathAndStorageKind>(
				astsArena,
				{
					// Ensure bootstrap.nz is parsed first
					bootstrapPath(modelArena),
					// Ensure std.nz is available
					stdPath(modelArena),
					// gc/runtime are needed by concretize so make sure they're available
					gcPath(modelArena),
					runtimePath(modelArena),
					runtimeMainPath(modelArena),
					mainPath,
				});
			for (const PathAndStorageKind path : rootPaths)
				if (!hasKey_mut<const PathAndStorageKind, const ParseStatus, comparePathAndStorageKind>(
					&statuses, path)) {
					const Opt<const Diags> err = parseRecur(
						modelArena,
						astsArena,
						allSymbols,
						storages,
						lineAndColumnGetters,
						&res,
						&statuses,
						none<const PathAndStorageKindAndRange>(),
						path);
					if (has(err))
						return failure<const AllAsts, const Diags>(force(err));
				}
			return success<const AllAsts, const Diags>(finishArr(&res));
		} else {
			addEmptyLineAndColumnGetter(modelArena, lineAndColumnGetters, mainPath);
			return failure<const AllAsts, const Diags>(arrLiteral<const Diagnostic>(
				modelArena,
				{
					Diagnostic{
						PathAndStorageKindAndRange{mainPath, SourceRange::empty()},
						Diag{Diag::FileShouldEndInNz{}}},
				}));
		}
	}

	struct LcgsAndAllAsts {
		const LineAndColumnGetters lineAndColumnGetters;
		const Result<const AllAsts, const Diags> allAsts;
	};

	LcgsAndAllAsts parseEverything(
		// Use modelArena for paths since those are stored along with the module
		Arena* modelArena,
		AllSymbols* allSymbols,
		const ReadOnlyStorages storages,
		const PathAndStorageKind mainPath,
		Arena* astsArena
	) {
		LineAndColumnGettersBuilder lineAndColumnGetters {};
		const Result<const Arr<const PathAndAstAndResolvedImports>, const Diags> asts =
			parseEverythingWorker(modelArena, astsArena, allSymbols, mainPath, storages, &lineAndColumnGetters);
		const LineAndColumnGetters lc = finishDictShouldBeNoConflict<
			const PathAndStorageKind,
			const LineAndColumnGetter,
			comparePathAndStorageKind
		>(&lineAndColumnGetters);
		return LcgsAndAllAsts{lc, asts};
	}

	struct ImportsAndExports {
		// This includes implicit import of 'std.nz' if this file is not itself in 'include'
		const Arr<const Module*> allImports;
		const Arr<const Module*> exports;
	};

	const Arr<const Module*> mapImportsOrExports(
		Arena* modelArena,
		const Arr<const PathAndStorageKind> paths,
		const MutDict<const PathAndStorageKind, const Module*, comparePathAndStorageKind>* compiled
	) {
		return map<const Module*>{}(modelArena, paths, [&](const PathAndStorageKind importPath) {
			return mustGetAt_mut<
				const PathAndStorageKind,
				const Module*,
				comparePathAndStorageKind
			>(compiled, importPath);
		});
	}

	struct ModulesAndCommonTypes {
		const Arr<const Module*> modules;
		const CommonTypes commonTypes;
	};

	// Result does not include the 'bootstrap' module.
	const Result<const ModulesAndCommonTypes, const Diags> getModules(
		Arena* modelArena,
		ProgramState* programState,
		const Arr<const PathAndAstAndResolvedImports> fileAsts
	) {
		Late<const CommonTypes> commonTypes {};

		Arena compiledArena {};
		MutDict<const PathAndStorageKind, const Module*, comparePathAndStorageKind> compiled {};
		const Result<const Arr<const Module*>, const Diags> res =
			mapOrFail<const Module*, const Diags>{}(
				modelArena,
				fileAsts,
				[&](const PathAndAstAndResolvedImports ast) {
					const Result<const Module*, const Diags> res = [&]() {
						if (lateIsSet(&commonTypes)) {
							const Bool isInInclude = enumEq(ast.pathAndStorageKind.storageKind, StorageKind::global);
							const Arr<const PathAndStorageKind> allImports = isInInclude
								? ast.resolvedImports
								: prepend<const PathAndStorageKind>(
									modelArena,
									stdPath(modelArena),
									ast.resolvedImports);
							return check(
								modelArena,
								programState,
								mapImportsOrExports(modelArena, allImports, &compiled),
								mapImportsOrExports(modelArena, ast.resolvedExports, &compiled),
								ast.pathAndAst(),
								lateGet(&commonTypes));
						} else {
							// The first module to checkis always 'bootstrap.nz'
							assert(pathAndStorageKindEq(ast.pathAndStorageKind, bootstrapPath(modelArena)));
							assert(isEmpty(ast.resolvedImports));
							const Result<const BootstrapCheck, const Diags> res =
								checkBootstrapNz(modelArena, programState, ast.pathAndAst());
							if (res.isSuccess())
								lateSet<const CommonTypes>(&commonTypes, res.asSuccess().commonTypes);
							return mapSuccess<const Module*>{}(res, [](const BootstrapCheck c) {
								return c.module;
							});
						}
					}();
					if (res.isSuccess())
						addToDict<
							const PathAndStorageKind,
							const Module*,
							comparePathAndStorageKind
						>(&compiledArena, &compiled, ast.pathAndStorageKind, res.asSuccess());
					return res;
				});
		return mapSuccess<const ModulesAndCommonTypes>{}(res, [&](const Arr<const Module*> modules) {
			return ModulesAndCommonTypes{modules, lateGet(&commonTypes)};
		});
	}

	const Module* findModule(const PathAndStorageKind pk, const Arr<const Module*> modules) {
		return force(find(modules, [&](const Module* m) {
			return pathAndStorageKindEq(m->pathAndStorageKind, pk);
		}));
	}

	const Result<const Program, const Diags> checkEverything(
		Arena* modelArena,
		const AllAsts allAsts,
		const PathAndStorageKind mainPath,
		const LineAndColumnGetters lineAndColumnGetters
	) {
		ProgramState programState {};
		return mapSuccess<const Program>{}(
			getModules(modelArena, &programState, allAsts),
			[&](const ModulesAndCommonTypes modulesAndCommonTypes) {
				const Arr<const Module*> modules = modulesAndCommonTypes.modules;
				const Module* bootstrapModule = findModule(bootstrapPath(modelArena), modules);
				const StructDecl* ctxStructDecl = mustGetAt<const Sym, const StructOrAlias, compareSym>(
					bootstrapModule->structsAndAliasesMap,
					shortSymAlphaLiteral("ctx")
				).asStructDecl();
				const StructInst* ctxStructInst = instantiateNonTemplateStruct(modelArena, ctxStructDecl);

				return Program{
					.bootstrapModule = bootstrapModule,
					.gcModule = findModule(gcPath(modelArena), modules),
					.runtimeModule = findModule(runtimePath(modelArena), modules),
					.runtimeMainModule = findModule(runtimeMainPath(modelArena), modules),
					.mainModule = findModule(mainPath, modules),
					.allModules = modules,
					.commonTypes = modulesAndCommonTypes.commonTypes,
					.ctxStructInst = ctxStructInst,
					.lineAndColumnGetters = lineAndColumnGetters
				};
			});
	}
}

const Result<const Program, const Diagnostics> frontendCompile(
	Arena* modelArena,
	AllSymbols* allSymbols,
	const ReadOnlyStorages storages,
	const Path* mainPath
) {
	Arena astsArena {};

	const PathAndStorageKind main = PathAndStorageKind{mainPath, StorageKind::local};
	const LcgsAndAllAsts parsed = parseEverything(modelArena, allSymbols, storages, main, &astsArena);
	const Result<const Program, const Diags> res =
		flatMapSuccess<const Program, const Diags>{}(parsed.allAsts, [&](const AllAsts allAsts) {
			return checkEverything(modelArena, allAsts, main, parsed.lineAndColumnGetters);
		});
	return mapFailure<const Diagnostics>{}(res, [&](const Diags diagnostics) {
		return Diagnostics{diagnostics, FilesInfo{storages.absolutePathsGetter(), parsed.lineAndColumnGetters}};
	});
}
