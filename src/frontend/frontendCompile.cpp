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

	const Opt<const NulTerminatedStr> getFile(
		Arena* fileArena,
		const PathAndStorageKind pk,
		const ReadOnlyStorages storages
	) {
		return storages.choose(pk.storageKind).tryReadFile(fileArena, pk.path);
	}

	const Arr<const Diagnostic> parseDiagnostics(
		Arena* modelArena,
		const PathAndStorageKind where,
		const ParseDiagnostic p
	) {
		return arrLiteral<const Diagnostic>(modelArena, { Diagnostic{where, p.range, Diag{p.diag}} });
	}

	using LineAndColumnGettersBuilder =
		DictBuilder<const PathAndStorageKind, const LineAndColumnGetter, comparePathAndStorageKind>;

	const Result<const FileAst, const Arr<const Diagnostic>> parseSingle(
		Arena* modelArena,
		Arena* astsArena,
		AllSymbols* allSymbols,
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
			return mapFailure<const Arr<const Diagnostic>>{}(
				parseFile(astsArena, allSymbols, text),
				[&](const ParseDiagnostic p) { return parseDiagnostics(modelArena, where, p); });
		}
		else
			return failure<const FileAst, const Arr<const Diagnostic>>(
				arrLiteral<const Diagnostic>(
					modelArena,
					{ Diagnostic{where, SourceRange::empty(), Diag{Diag::FileDoesNotExist{}}} }));
	}

	// NOTE: does not ensure that the file exists.
	// Only returns none for a relative import that tries climbing past the root.
	const Opt<const PathAndStorageKind> resolveImport(
		Arena* modelArena,
		const PathAndStorageKind from,
		const ImportAst ast
	) {
		const Path* path = copyPath(modelArena, ast.path);
		if (ast.nDots == 0)
			return some<const PathAndStorageKind>(PathAndStorageKind{path, StorageKind::global});
		else {
			const Opt<const Path*> rel = resolvePath(modelArena, from.path->parent, RelPath{ast.nDots - 1, path});
			return has(rel)
				? some<const PathAndStorageKind>(PathAndStorageKind{force(rel), from.storageKind})
				: none<const PathAndStorageKind>();
		}
	}

	enum class Status {
		started,
		finished
	};

	struct PathAndStorageKindAndRange {
		const PathAndStorageKind path;
		const SourceRange range;
	};

	struct ImportAndExportPaths {
		const Arr<const PathAndStorageKindAndRange> imports;
		const Arr<const PathAndStorageKindAndRange> exports;
	};

	const Arr<const PathAndStorageKindAndRange> resolveImportsOrExports(
		Arena* modelArena,
		Arena* astsArena,
		const PathAndStorageKind path,
		const Arr<const ImportAst> importsOrExports
	) {
		return map<const PathAndStorageKindAndRange>{}(astsArena, importsOrExports, [&](const ImportAst i) {
			const Opt<const PathAndStorageKind> opDependencyPath = resolveImport(modelArena, path, i);
			if (!has(opDependencyPath))
				todo<void>("diagnostic: import resolution failed");
			return PathAndStorageKindAndRange{force(opDependencyPath), i.range};
		});
	}

	const Result<const ImportAndExportPaths, const Arr<const Diagnostic>> resolveImportsAndExports(
		Arena* modelArena,
		Arena* astsArena,
		const PathAndStorageKind path,
		const Arr<const ImportAst> imports,
		const Arr<const ImportAst> exports
	) {
		// TODO: resolveImport should potentially fail, so dont' always just return success
		return success<const ImportAndExportPaths, const Arr<const Diagnostic>>(
			ImportAndExportPaths{
				resolveImportsOrExports(modelArena, astsArena, path, imports),
				resolveImportsOrExports(modelArena, astsArena, path, exports)});
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

	const Arr<const PathAndStorageKind> stripRange(Arena* arena, const Arr<const PathAndStorageKindAndRange> a) {
		return map<const PathAndStorageKind>{}(
			arena,
			a,
			[](const PathAndStorageKindAndRange p) {
				return p.path;
			});
	}

	const Opt<const Arr<const Diagnostic>> parseRecur(
		Arena* modelArena,
		Arena* astsArena,
		AllSymbols* allSymbols,
		ReadOnlyStorages storages,
		LineAndColumnGettersBuilder* lineAndColumnGetters,
		ArrBuilder<const PathAndAstAndResolvedImports>* res,
		MutDict<const PathAndStorageKind, const Status, comparePathAndStorageKind>* statuses,
		const PathAndStorageKind path
	) {
		setInDict<const PathAndStorageKind, const Status, comparePathAndStorageKind>(
			astsArena, statuses, path, Status::started);

		// Parse it now
		const Result<const FileAst, const Arr<const Diagnostic>> parseResult =
			parseSingle(modelArena, astsArena, allSymbols, path, storages, lineAndColumnGetters);
		return parseResult.match(
			[&](const FileAst ast) {
				const Result<const ImportAndExportPaths, const Arr<const Diagnostic>> importsResult =
					resolveImportsAndExports(modelArena, astsArena, path, ast.imports, ast.exports);
				return importsResult.match(
					[&](const ImportAndExportPaths importsAndExports) {
						// Ensure all imports are added before adding this
						const Arr<const PathAndStorageKindAndRange> importsAndExportsArr =
							cat(astsArena, importsAndExports.imports, importsAndExports.exports);
						for (const PathAndStorageKindAndRange import : importsAndExportsArr) {
							const Opt<const Status> status =
								getAt_mut<const PathAndStorageKind, const Status, comparePathAndStorageKind>(
									statuses, import.path);
							if (has(status)) {
								switch (force(status)) {
									case Status::started: {
										const Diagnostic diag = Diagnostic{
											path,
											import.range,
											Diag{Diag::CircularImport{path, import.path}}};
										return some<const Arr<const Diagnostic>>(
											arrLiteral<const Diagnostic>(modelArena, { diag }));
									}
									case Status::finished:
										break;
									default:
										assert(0);
								}
							} else {
								const Opt<const Arr<const Diagnostic>> err = parseRecur(
									modelArena,
									astsArena,
									allSymbols,
									storages,
									lineAndColumnGetters,
									res,
									statuses,
									import.path);
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
						setInDict<const PathAndStorageKind, const Status, comparePathAndStorageKind>(
							astsArena,
							statuses,
							path,
							Status::finished);
						return none<const Arr<const Diagnostic>>();
					},
					[](const Arr<const Diagnostic> d) {
						return some<const Arr<const Diagnostic>>(d);
					});
			},
			[](const Arr<const Diagnostic> d) {
				return some<const Arr<const Diagnostic>>(d);
			});
	};

	using AllAsts = const Arr<const PathAndAstAndResolvedImports>;

	// Starts at 'main' and recursively parses all imports too.
	// Result will be in import order -- asts at lower indices are imported by asts at higher indices.
	// So, don't have to worry about circularity when checking.
	const Result<const AllAsts, const Arr<const Diagnostic>> parseEverythingWorker(
		Arena* modelArena,
		Arena* astsArena,
		AllSymbols* allSymbols,
		const PathAndStorageKind mainPath,
		ReadOnlyStorages storages,
		LineAndColumnGettersBuilder* lineAndColumnGetters
	) {
		ArrBuilder<const PathAndAstAndResolvedImports> res {};
		MutDict<const PathAndStorageKind, const Status, comparePathAndStorageKind> statuses {};
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
				mainPath,
			});
		for (const PathAndStorageKind path : rootPaths) {
			if (!hasKey_mut<const PathAndStorageKind, const Status, comparePathAndStorageKind>(&statuses, path)) {
				const Opt<const Arr<const Diagnostic>> err = parseRecur(
					modelArena,
					astsArena,
					allSymbols,
					storages,
					lineAndColumnGetters,
					&res,
					&statuses,
					path);
				if (has(err))
					return failure<const AllAsts, const Arr<const Diagnostic>>(force(err));
			}
		}
		return success<const Arr<const PathAndAstAndResolvedImports>, const Arr<const Diagnostic>>(finishArr(&res));
	}

	struct LcgsAndAllAsts {
		const LineAndColumnGetters lineAndColumnGetters;
		const Result<const AllAsts, const Arr<const Diagnostic>> allAsts;
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
		const Result<const Arr<const PathAndAstAndResolvedImports>, const Arr<const Diagnostic>> asts =
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
	const Result<const ModulesAndCommonTypes, const Arr<const Diagnostic>> getModules(
		Arena* modelArena,
		ProgramState* programState,
		const Arr<const PathAndAstAndResolvedImports> fileAsts
	) {
		Late<const CommonTypes> commonTypes {};

		Arena compiledArena {};
		MutDict<const PathAndStorageKind, const Module*, comparePathAndStorageKind> compiled {};
		const Result<const Arr<const Module*>, const Arr<const Diagnostic>> res =
			mapOrFail<const Module*, const Arr<const Diagnostic>>{}(
				modelArena,
				fileAsts,
				[&](const PathAndAstAndResolvedImports ast) {
					const Result<const Module*, const Arr<const Diagnostic>> res = [&]() {
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
							const Result<const BootstrapCheck, const Arr<const Diagnostic>> res =
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

	const Result<const Program, const Arr<const Diagnostic>> checkEverything(
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
	const Result<const Program, const Arr<const Diagnostic>> res =
		flatMapSuccess<const Program, const Arr<const Diagnostic>>{}(parsed.allAsts, [&](const AllAsts allAsts) {
			return checkEverything(modelArena, allAsts, main, parsed.lineAndColumnGetters);
		});
	return mapFailure<const Diagnostics>{}(res, [&](const Arr<const Diagnostic> diagnostics) {
		return Diagnostics{diagnostics, FilesInfo{storages.absolutePathsGetter(), parsed.lineAndColumnGetters}};
	});
}
