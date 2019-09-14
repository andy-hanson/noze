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
	const PathAndStorageKind includePath(Arena* arena) {
		return PathAndStorageKind{rootPath(arena, strLiteral("include.nz")), StorageKind::global};
	}

	const PathAndStorageKind gcPath(Arena* arena) {
		return PathAndStorageKind{rootPath(arena, strLiteral("gc.nz")), StorageKind::global};
	}

	const PathAndStorageKind runtimePath(Arena* arena) {
		return PathAndStorageKind{rootPath(arena, strLiteral("runtime.nz")), StorageKind::global};
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

	const Result<const Arr<const PathAndStorageKindAndRange>, const Arr<const Diagnostic>> resolveImports(
		Arena* modelArena,
		Arena* astsArena,
		const PathAndStorageKind path,
		const Arr<const ImportAst> imports
	) {
		return success<const Arr<const PathAndStorageKindAndRange>, const Arr<const Diagnostic>>(
			map<const PathAndStorageKindAndRange>{}(astsArena, imports, [&](const ImportAst i) {
				const Opt<const PathAndStorageKind> opDependencyPath = resolveImport(modelArena, path, i);
				if (!has(opDependencyPath))
					todo<void>("diagnostic: import resolution failed");
				return PathAndStorageKindAndRange{force(opDependencyPath), i.range};
			}));
	}

	struct PathAndAstAndResolvedImports {
		const PathAndStorageKind pathAndStorageKind;
		const FileAst ast;
		const Arr<const PathAndStorageKind> resolvedImports;

		inline PathAndAst pathAndAst() const {
			return PathAndAst{pathAndStorageKind, ast};
		}
	};

	const Opt<const Arr<const Diagnostic>> recur(
		Arena* modelArena,
		Arena* astsArena,
		AllSymbols* allSymbols,
		ReadOnlyStorages storages,
		LineAndColumnGettersBuilder* lineAndColumnGetters,
		ArrBuilder<const PathAndAstAndResolvedImports>* res,
		MutDict<const PathAndStorageKind, const Status, comparePathAndStorageKind>* statuses,
		const PathAndStorageKind path
	) {
		setInDict<const PathAndStorageKind, const Status, comparePathAndStorageKind>(astsArena, statuses, path, Status::started);

		// Parse it now
		const Result<const FileAst, const Arr<const Diagnostic>> parseResult =
			parseSingle(modelArena, astsArena, allSymbols, path, storages, lineAndColumnGetters);
		return parseResult.match(
			[&](const FileAst ast) {
				const Result<const Arr<const PathAndStorageKindAndRange>, const Arr<const Diagnostic>> importsResult =
					resolveImports(modelArena, astsArena, path, ast.imports);
				return importsResult.match(
					[&](const Arr<const PathAndStorageKindAndRange> imports) {
						// Ensure all imports are added before adding this
						for (const PathAndStorageKindAndRange import : imports) {
							const Opt<const Status> status =
								getAt_mut<const PathAndStorageKind, const Status, comparePathAndStorageKind>(statuses, import.path);
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
								const Opt<const Arr<const Diagnostic>> err = recur(
									modelArena, astsArena, allSymbols, storages, lineAndColumnGetters, res, statuses, import.path);
								if (has(err))
									return err;
							}
						}
						const PathAndAstAndResolvedImports pa = PathAndAstAndResolvedImports{
							path,
							ast,
							map<const PathAndStorageKind>{}(
								astsArena,
								imports,
								[](const PathAndStorageKindAndRange p) {
									return p.path;
								})};
						add<const PathAndAstAndResolvedImports>(astsArena, res, pa);
						setInDict<const PathAndStorageKind, const Status, comparePathAndStorageKind>(astsArena, statuses, path, Status::finished);
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

	// Starts at 'main' and recursively parses all imports too.
	// Result will be in import order -- asts at lower indices are imported by asts at higher indices.
	// So, don't have to worry about circularity when checking.
	// NOTE: this function does not return the 'include' module which is not imported directly.
	const Result<const Arr<const PathAndAstAndResolvedImports>, const Arr<const Diagnostic>> parseFromMain(
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
			{ mainPath, gcPath(modelArena), runtimePath(modelArena) });
		for (const PathAndStorageKind path : rootPaths) {
			if (!hasKey_mut<const PathAndStorageKind, const Status, comparePathAndStorageKind>(&statuses, path)) {
				const Opt<const Arr<const Diagnostic>> err = recur(modelArena, astsArena, allSymbols, storages, lineAndColumnGetters, &res, &statuses, path);
				if (has(err))
					return failure<const Arr<const PathAndAstAndResolvedImports>, const Arr<const Diagnostic>>(force(err));
			}
		}
		return success<const Arr<const PathAndAstAndResolvedImports>, const Arr<const Diagnostic>>(finishArr(&res));
	}

	//TODO:INLINE
	const Arr<const Module*> getImports(
		Arena* modelArena,
		const Arr<const PathAndStorageKind> imports,
		const MutDict<const PathAndStorageKind, const Module*, comparePathAndStorageKind>* compiled
	) {
		return map<const Module*>{}(modelArena, imports, [&](const PathAndStorageKind importPath) {
			return mustGetAt_mut<
				const PathAndStorageKind,
				const Module*,
				comparePathAndStorageKind
			>(compiled, importPath);
		});
	}

	// Result does not include the 'include' module.
	const Result<const Arr<const Module*>, const Arr<const Diagnostic>> getModules(
		Arena* modelArena,
		ProgramState* programState,
		const IncludeCheck includeCheck,
		const Arr<const PathAndAstAndResolvedImports> fileAsts
	) {
		Arena compiledArena {};
		MutDict<const PathAndStorageKind, const Module*, comparePathAndStorageKind> compiled {};
		return mapOrFail<const Module*, const Arr<const Diagnostic>>{}(
				modelArena,
				fileAsts,
				[&](const PathAndAstAndResolvedImports ast) {
					const Result<const Arr<const Module*>, const Arr<const Diagnostic>> resultImports =
						getImports(modelArena, ast.resolvedImports, &compiled);
					return flatMapSuccess<const Module*, const Arr<const Diagnostic>>{}(
						resultImports,
						[&](const Arr<const Module*> imports) {
							const Result<const Module*, const Arr<const Diagnostic>> res =
								check(modelArena, programState, imports, ast.pathAndAst(), includeCheck);
							if (res.isSuccess())
								addToDict<
									const PathAndStorageKind,
									const Module*,
									comparePathAndStorageKind
								>(&compiledArena, &compiled, ast.pathAndStorageKind, res.asSuccess());
							return res;
						});
				});
	}

	struct AllAsts {
		const PathAndAst includeAst;
		const Arr<const PathAndAstAndResolvedImports> otherAsts;
	};

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
		const PathAndStorageKind inclPath = includePath(modelArena);
		const Result<const FileAst, const Arr<const Diagnostic>> includeAst =
			parseSingle(modelArena, astsArena, allSymbols, inclPath, storages, &lineAndColumnGetters);
		const Result<const AllAsts, const Arr<const Diagnostic>> res = flatMapSuccess<
			const AllAsts,
			const Arr<const Diagnostic>
		>{}(
			includeAst,
			[&](const FileAst incl) {
				const Result<const Arr<const PathAndAstAndResolvedImports>, const Arr<const Diagnostic>> otherAsts =
					parseFromMain(modelArena, astsArena, allSymbols, mainPath, storages, &lineAndColumnGetters);
				return mapSuccess<const AllAsts>{}(otherAsts, [&](const Arr<const PathAndAstAndResolvedImports> oth) {
					return AllAsts{PathAndAst{inclPath, incl}, oth};
				});
			});

		const LineAndColumnGetters lc = finishDictShouldBeNoConflict<
			const PathAndStorageKind,
			const LineAndColumnGetter,
			comparePathAndStorageKind
		>(&lineAndColumnGetters);
		return LcgsAndAllAsts{lc, res};
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
		const Result<const IncludeCheck, const Arr<const Diagnostic>> include =
			checkIncludeNz(modelArena, &programState, allAsts.includeAst);
		return flatMapSuccess<
			const Program,
			const Arr<const Diagnostic>
		>{}(include, [&](const IncludeCheck includeCheck) {
			return mapSuccess<const Program>{}(
				getModules(modelArena, &programState, includeCheck, allAsts.otherAsts),
				[&](const Arr<const Module*> modules) {
					const StructDecl* ctxStructDecl = mustGetAt<const Sym, const StructOrAlias, compareSym>(
						includeCheck.module->structsAndAliasesMap,
						shortSymAlphaLiteral("ctx")
					).asStructDecl();
					const StructInst* ctxStructInst = instantiateNonTemplateStruct(modelArena, ctxStructDecl);

					return Program{
						.includeModule = includeCheck.module,
						.gcModule = findModule(gcPath(modelArena), modules),
						.runtimeModule = findModule(runtimePath(modelArena), modules),
						.mainModule = findModule(mainPath, modules),
						.allModules = prepend<const Module*>(modelArena, includeCheck.module, modules),
						.commonTypes = includeCheck.commonTypes,
						.ctxStructInst = ctxStructInst,
						.lineAndColumnGetters = lineAndColumnGetters
					};
				});
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
