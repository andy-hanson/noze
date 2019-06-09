#include "./check.h"

#include "./checkCtx.h"
#include "./typeFromAst.h"

namespace {
	const Opt<const StructDecl*> getCommonGenericType(
		const StructsAndAliasesMap& structsAndAliasesMap,
		const Str name,
		const size_t expectedTypeParams
	) {
		const Opt<const StructOrAlias> res = structsAndAliasesMap.get(name);
		if (res.has()) {
			// may fail -- builtin generic should not be an alias
			const StructDecl* decl = res.force().asDecl();
			if (decl->typeParams.size != expectedTypeParams)
				todo<void>("getCommonGenericType");
			return some<const StructDecl*>(decl);
		} else
			return none<const StructDecl*>();
	}

	const Opt<const StructInst*> getCommonNonGenericType(
		Arena& arena,
		const StructsAndAliasesMap& structsAndAliasesMap,
		const Str name,
		MutArr<StructInst*>& delayedStructInsts
	) {
		const Opt<const StructOrAlias> opStructOrAlias = structsAndAliasesMap.get(name);
		if (!opStructOrAlias.has())
			return none<const StructInst*>();
		else {
			const StructOrAlias structOrAlias = opStructOrAlias.force();
			assert(structOrAlias.typeParams().isEmpty());
			return some<const StructInst*>(structOrAlias.match(
				[](const StructAlias* a) { return a->target(); },
				[&](const StructDecl* s) {
					return instantiateStruct(
						arena,
						s,
						emptyArr<const Type>(),
						some<MutArr<StructInst*>&>(delayedStructInsts));
				}));
		}
	}

	const Result<const CommonTypes, const Diagnostics> getCommonTypes(
		const PathAndStorageKind path,
		CheckCtx& ctx,
		const StructsAndAliasesMap& structsAndAliasesMap,
		MutArr<StructInst*>& delayedStructInsts
	) {
		// non-generic types
		auto ng = [&](const char* s) -> const Opt<const StructInst*> {
			return getCommonNonGenericType(ctx.arena, structsAndAliasesMap, strLiteral(s), delayedStructInsts);
		};
		const Opt<const StructInst*>
			_bool = ng("bool"),
			int64 = ng("int64"),
			_char = ng("char"),
			str = ng("str"),
			_void = ng("void");

		// generic types
		auto com = [&](const char* name, const size_t nTypeParameters) -> const Opt<const StructDecl*> {
			return getCommonGenericType(structsAndAliasesMap, strLiteral(name), nTypeParameters);
		};
		const Opt<const StructDecl*>
			opt = com("opt", 1),
			some = com("some", 1),
			none = com("none", 0),
			byVal = com("by-val", 1),
			arr = com("arr", 1),
			mutArr = com("mut-arr", 1),
			fut = com("fut", 1),
			fun0 = com("fun0", 1),
			fun1 = com("fun1", 2),
			fun2 = com("fun2", 3),
			remoteFun0 = com("remote-fun0", 1),
			remoteFun1 = com("remote-fun1", 2),
			remoteFun2 = com("remote-fun2", 3);

		if (_bool.has() && _char.has() && int64.has() && str.has() && _void.has() &&
			opt.has() && some.has() && none.has() &&
			byVal.has() && arr.has() && fut.has() &&
			fun0.has() && fun1.has() && fun2.has() &&
			remoteFun0.has() && remoteFun1.has() && remoteFun2.has())
			return success<const CommonTypes, const Diagnostics>(
				CommonTypes{
					_bool.force(),
					_char.force(),
					int64.force(),
					str.force(),
					_void.force(),
					{opt.force(), some.force(), none.force()},
					byVal.force(),
					arr.force(),
					mutArr.force(),
					fut.force(),
					{fun0.force(), fun1.force(), fun2.force()},
					{remoteFun0.force(), remoteFun1.force(), remoteFun2.force()}
				});
		else {
			const Diagnostic diag = Diagnostic{path, SourceRange::empty(), Diag{Diag::CommonTypesMissing{}}};
			return failure<const CommonTypes, const Diagnostics>(arrLiteral<const Diagnostic>(ctx.arena, diag));
		}
	}

	Purity getPurityFromAst(const StructDeclAst it) {
		// Note: purity is taken for granted here, and verified later when we check the body.
		if (it.body.isIface()) {
			if (it.purity.has())
				todo<void>("iface should not have explicit purity, is always sendable");
			return Purity::sendable;
		} else if (it.purity.has())
			switch (it.purity.force()) {
				case PuritySpecifier::sendable:
					return Purity::sendable;
				case PuritySpecifier::nonSendable:
					return Purity::nonSendable;
				default:
					return unreachable<Purity>();
			}
		else
			return Purity::data;
	}

	const Arr<const TypeParam> checkTypeParams(CheckCtx& ctx, const Arr<const TypeParamAst> asts) {
		const Arr<const TypeParam> typeParams = mapWithIndex<const TypeParam>{}(ctx.arena, asts, [&](const TypeParamAst& ast, const size_t i) {
			return TypeParam{ast.range, ctx.copyStr(ast.name), i};
		});
		for (size_t i = 0; i < typeParams.size; i++)
			for (size_t prev_i = 0; prev_i < i; prev_i++)
				if (strEq(typeParams[prev_i].name, typeParams[i].name))
					ctx.diag(typeParams[i].range, Diag{Diag::ParamShadowsPrevious{Diag::ParamShadowsPrevious::Kind::typeParam}});
		return typeParams;
	}

	void collectTypeParamsInAst(Arena& arena, const TypeAst ast, ArrBuilder<const TypeParam>& res, const Arr<const TypeParam> outerTypeParams) {
		return ast.match(
			[&](const TypeAst::TypeParam tp) {
				if (!exists(outerTypeParams, [&](const TypeParam it) { return strEq(it.name, tp.name); })
					&& !exists(res.tempAsArr(), [&](const TypeParam it) { return strEq(it.name, tp.name); }))
					res.add(arena, TypeParam{tp.range, copyStr(arena, tp.name), res.size()});
			},
			[&](const TypeAst::InstStruct i) {
				for (const TypeAst arg : i.typeArgs)
					collectTypeParamsInAst(arena, arg, res, outerTypeParams);
			});
	}

	const Arr<const TypeParam> collectTypeParams(Arena& arena, const SigAst ast, const Arr<const TypeParam> outerTypeParams) {
		auto res = ArrBuilder<const TypeParam>{};
		collectTypeParamsInAst(arena, ast.returnType, res, outerTypeParams);
		for (const ParamAst p : ast.params)
			collectTypeParamsInAst(arena, p.type, res, outerTypeParams);
		return res.finish();
	}

	struct SigAndTypeParams {
		const Sig sig;
		const Arr<const TypeParam> typeParams;
	};

	const Arr<const Param> checkParams(
		CheckCtx& ctx,
		const Arr<const ParamAst> asts,
		const StructsAndAliasesMap structsAndAliasesMap,
		const TypeParamsScope& typeParamsScope,
		DelayStructInsts delayStructInsts
	) {
		const Arr<const Param> params = mapWithIndex<const Param>{}(ctx.arena, asts, [&](const ParamAst ast, const size_t index) {
			const Type type = typeFromAst(ctx, ast.type, structsAndAliasesMap, typeParamsScope, delayStructInsts);
			return Param{ast.range, ctx.copyStr(ast.name), type, index};
		});
		for (size_t i = 0; i < params.size; i++)
			for (size_t prev_i = 0; prev_i < i; prev_i++)
				if (strEq(params[prev_i].name, params[i].name))
					ctx.diag(params[i].range, Diag{Diag::ParamShadowsPrevious{Diag::ParamShadowsPrevious::Kind::param}});
		return params;
	}

	const SigAndTypeParams checkSig(
		CheckCtx& ctx,
		const Arr<const TypeParamAst> explicitTypeParams,
		const SigAst ast,
		const Arr<const TypeParam> outerTypeParams,
		const StructsAndAliasesMap& structsAndAliasesMap,
		Opt<MutArr<StructInst*>&> delayStructInsts
	) {
		const Arr<const TypeParam> typeParams = explicitTypeParams.isEmpty()
			? collectTypeParams(ctx.arena, ast, outerTypeParams)
			: checkTypeParams(ctx, explicitTypeParams);
		const TypeParamsScope typeParamsScope = TypeParamsScope{typeParams, outerTypeParams};
		const Arr<const Param> params = checkParams(ctx, ast.params, structsAndAliasesMap, typeParamsScope, delayStructInsts);
		const Type returnType = typeFromAst(ctx, ast.returnType, structsAndAliasesMap, typeParamsScope, delayStructInsts);
		return SigAndTypeParams{Sig{ast.range, ctx.copyStr(ast.name), returnType, params}, typeParams};
	}

	const Sig checkSigNoTypeParams(
		CheckCtx& ctx,
		const SigAst ast,
		const Arr<const TypeParam> outerTypeParams,
		const StructsAndAliasesMap& structsAndAliasesMap,
		MutArr<StructInst*>& delayStructInsts
	) {
		const SigAndTypeParams st = checkSig(
			ctx, emptyArr<const TypeParamAst>(), ast, outerTypeParams, structsAndAliasesMap, some<MutArr<StructInst*>&>(delayStructInsts));
		if (!st.typeParams.isEmpty())
			ctx.diag(st.sig.range, Diag{Diag::ShouldNotHaveTypeParamsInIface{}});
		return st.sig;
	}

	const Arr<const SpecDecl> checkSpecs(
		CheckCtx& ctx,
		const StructsAndAliasesMap& structsAndAliasesMap,
		const Arr<const SpecDeclAst> asts,
		MutArr<StructInst*>& delayStructInsts
	) {
		return map<const SpecDecl>{}(ctx.arena, asts, [&](const SpecDeclAst ast) {
			const Arr<const TypeParam> typeParams = checkTypeParams(ctx, ast.typeParams);
			const Arr<const Sig> sigs = map<const Sig>{}(ctx.arena, ast.sigs, [&](const SigAst it) {
				return checkSigNoTypeParams(ctx, it, typeParams, structsAndAliasesMap, delayStructInsts);
			});
			return SpecDecl{ast.range, ast.isPublic, ctx.copyStr(ast.name), typeParams, sigs};
		});
	}

	const Arr<StructAlias> checkStructAliasesInitial(CheckCtx& ctx, const Arr<const StructAliasAst> asts) {
		return map<StructAlias>{}(ctx.arena, asts, [&](const StructAliasAst ast) {
			return StructAlias{ast.range, ast.isPublic, ctx.copyStr(ast.name), checkTypeParams(ctx, ast.typeParams)};
		});
	}

	const Arr<StructDecl> checkStructsInitial(CheckCtx& ctx, const Arr<const StructDeclAst> asts) {
		return map<StructDecl>{}(ctx.arena, asts, [&](const StructDeclAst ast) {
			return StructDecl{ast.range, ast.isPublic, ctx.copyStr(ast.name), checkTypeParams(ctx, ast.typeParams), getPurityFromAst(ast)};
		});
	}

	void checkStructAliasTargets(
		CheckCtx& ctx,
		const StructsAndAliasesMap& structsAndAliasesMap,
		Arr<StructAlias> aliases,
		const Arr<const StructAliasAst> asts,
		MutArr<StructInst*>& delayStructInsts
	) {
		zip(aliases, asts, [&](StructAlias& structAlias, const StructAliasAst& ast) {
			const Opt<const StructInst*> inst = instStructFromAst(
				ctx, ast.target, structsAndAliasesMap, TypeParamsScope{structAlias.typeParams},
				some<MutArr<StructInst*>&>(delayStructInsts));
			if (!inst.has())
				todo<void>("handle invalid alias");
			structAlias.setTarget(inst.force());
		});
	}

	void checkSendable(CheckCtx& ctx, const SourceRange range, const Type type) {
		if (!typeIsPossiblySendable(type))
			ctx.diag(range, Diag{Diag::TypeNotSendable{}});
	}

	template <typename T, typename CbEqual>
	void checkNoEqual(const Arr<T> a, CbEqual eq) {
		for (size_t i = 0; i < a.size; i++)
			for (size_t j = 0; j < i; j++)
				if (eq(a[i], a[j]))
					todo<void>("checkNoEqual");
	}

	void checkStructBodies(
		CheckCtx& ctx,
		const CommonTypes& commonTypes,
		const StructsAndAliasesMap& structsAndAliasesMap,
		const Arr<StructDecl> structs,
		const Arr<const StructDeclAst> asts,
		MutArr<StructInst*>& delayStructInsts
	) {
		DelayStructInsts delay = some<MutArr<StructInst*>&>(delayStructInsts);
		zip(structs, asts, [&](StructDecl& strukt, const StructDeclAst& ast) {
			const StructBody body = ast.body.match(
				/*builtin*/ []() { return StructBody::builtin(); },
				[&](const StructDeclAst::Body::Fields f) {
					const Arr<const StructField> fields = mapWithIndex<const StructField>{}(
						ctx.arena,
						f.fields,
						[&](const StructDeclAst::Body::Fields::Field it, const size_t index) {
							const Type fieldType = typeFromAst(ctx, it.type, structsAndAliasesMap, TypeParamsScope{strukt.typeParams}, delay);
							if (isPurityWorse(fieldType.purity(), strukt.purity))
								todo<void>("field purity is worse than struct purity");
							return StructField{fieldType, ctx.copyStr(it.name), index};
						});
					checkNoEqual(fields, [](const StructField a, const StructField b) { return strEq(a.name, b.name); });
					return StructBody{StructBody::Fields{fields}};
				},
				[&](const StructDeclAst::Body::Union un) {
					const Arr<const StructInst*> members = map<const StructInst*>{}(ctx.arena, un.members, [&](const TypeAst::InstStruct it) {
						const Opt<const StructInst*> res = instStructFromAst(ctx, it, structsAndAliasesMap, TypeParamsScope{strukt.typeParams}, delay);
						if (!res.has())
							todo<void>("union member not found");
						if (isPurityWorse(res.force()->purity, strukt.purity))
							todo<void>("union member purity worse than union purity");
						return res.force();
					});
					checkNoEqual(members, [](const StructInst* a, const StructInst* b) { return ptrEquals(a->decl, b->decl); });
					return StructBody{StructBody::Union{members}};
				},
				[&](const StructDeclAst::Body::Iface i) {
					const Arr<const Message> messages = mapWithIndex<const Message>{}(
						ctx.arena,
						i.messages,
						[&](const SigAst it, const size_t idx) {
							const Sig sig = checkSigNoTypeParams(ctx, it, strukt.typeParams, structsAndAliasesMap, delayStructInsts);
							const Sig sig2 = Sig{sig.range, sig.name, makeFutType(ctx.arena, commonTypes, sig.returnType), sig.params};
							return Message{sig2, idx};
						});
					// TODO: check no two messages have the same name
					return StructBody{StructBody::Iface{messages}};
				});
			strukt.setBody(body);
		});

		for (const StructDecl& strukt : structs) {
			strukt.body().match(
				/*builtin*/ []() {},
				[](__attribute__((unused)) const StructBody::Fields& _) {},
				[](const StructBody::Union& u) {
					for (const StructInst* member : u.members)
						if (member->decl->body().isUnion())
							todo<void>("unions can't contain unions");
				},
				[&](const StructBody::Iface& i) {
					for (const Message& message : i.messages) {
						const Sig sig = message.sig;
						checkSendable(ctx, sig.range, sig.returnType);
						for (const Param param : sig.params)
							checkSendable(ctx, param.range, param.type);
					}
				});
		}
	}

	const StructsAndAliasesMap buildStructsAndAliasesDict(CheckCtx& ctx, const Arr<StructDecl> structs, const Arr<StructAlias> aliases) {
		auto d = DictBuilder<const Str, const StructOrAlias, strEq>{};
		for (const StructDecl* decl : ptrsRange(structs))
			d.add(ctx.arena, decl->name, StructOrAlias{decl});
		for (const StructAlias* a : ptrsRange(aliases))
			d.add(ctx.arena, a->name, StructOrAlias{a});
		return d.finish(ctx.arena, [&](const Str name, __attribute__((unused)) const StructOrAlias _, const StructOrAlias b) {
			ctx.diag(b.range(), Diag{Diag::DuplicateDeclaration{Diag::DuplicateDeclaration::Kind::structOrAlias, name}});
		});
	}

	template <typename T>
	const Dict<const Str, T*, strEq> buildDeclsDict(CheckCtx& ctx, const Arr<T> ts, const Diag::DuplicateDeclaration::Kind kind) {
		unused(ctx); unused(ts); unused(kind);
		return todo<Dict<const Str, T*, strEq>>("buildDeclsDict");
	}

	struct FunsAndMap {
		const Arr<const FunDecl> funs;
		const FunsMap funsMap;
	};

	const FunsAndMap checkFuns(
		CheckCtx& ctx,
		const CommonTypes& commonTypes,
		const SpecsMap& specsMap,
		const StructsAndAliasesMap& structsAndAliasesMap,
		const Arr<const FunDeclAst> asts
	) {
		unused(ctx);
		unused(commonTypes);
		unused(specsMap);
		unused(structsAndAliasesMap);
		unused(asts);
		return todo<const FunsAndMap>("FunsAndMap");
	}

	template <typename GetCommonTypes>
	const Result<const IncludeCheck, const Diagnostics> checkWorker(
		Arena& arena,
		const Opt<const Module*> include,
		const Arr<const Module*> imports,
		const FileAst ast,
		const PathAndStorageKind path,
		GetCommonTypes getCommonTypes
	) {
		CheckCtx ctx = CheckCtx{arena, path, include, imports};

		// Since structs may refer to each other, first get a structsAndAliasesMap, *then* fill in bodies
		const Arr<StructDecl> structs = checkStructsInitial(ctx, ast.structs);
		const Arr<StructAlias> structAliases = checkStructAliasesInitial(ctx, ast.structAliases);
		const StructsAndAliasesMap structsAndAliasesMap = buildStructsAndAliasesDict(ctx, structs, structAliases);

		// We need to create StructInsts when filling in struct bodies.
		// But when creating a StructInst, we usuallly want to fill in its body.
		// In case the decl body isn't available yet, we'll delay creating the StructInst body, which isn't needed until expr checking.
		MutArr<StructInst*> delayStructInsts = MutArr<StructInst*>{};

		checkStructAliasTargets(ctx, structsAndAliasesMap, structAliases, ast.structAliases, delayStructInsts);
		const Arr<const SpecDecl> specs = checkSpecs(ctx, structsAndAliasesMap, ast.specs, delayStructInsts);
		const SpecsMap specsMap = buildDeclsDict<const SpecDecl>(ctx, specs, Diag::DuplicateDeclaration::Kind::spec);

		if (ctx.hasDiags())
			return failure<const IncludeCheck, const Diagnostics>(ctx.diags());
		else {
			const Result<const CommonTypes, const Diagnostics> commonTypes = getCommonTypes(ctx, structsAndAliasesMap, delayStructInsts);
			return flatMapSuccess(commonTypes, [&](const CommonTypes commonTypes) {
				checkStructBodies(ctx, commonTypes, structsAndAliasesMap, structs, ast.structs, delayStructInsts);
				for (StructInst* i : delayStructInsts.freeze())
					i->setBody(instantiateStructBody(arena, i->decl, i->typeArgs));
				const FunsAndMap funsAndMap = checkFuns(ctx, commonTypes, specsMap, structsAndAliasesMap, ast.funs);
				if (ctx.hasDiags())
					return failure<const IncludeCheck, const Diagnostics>(ctx.diags());
				else {
					const Module* mod = arena.nu<const Module>()(
						path,
						imports,
						structs.asConst(),
						specs,
						funsAndMap.funs,
						structsAndAliasesMap,
						specsMap,
						funsAndMap.funsMap);
					return success<const IncludeCheck, const Diagnostics>(IncludeCheck{mod, commonTypes});
				}
			});
		}
	}
}

const Result<const IncludeCheck, const Diagnostics> checkIncludeNz(
	Arena& arena,
	const FileAst ast,
	const PathAndStorageKind path
) {
	return checkWorker(
		arena,
		none<const Module*>(),
		emptyArr<const Module*>(),
		ast,
		path,
		[&](CheckCtx& ctx, const StructsAndAliasesMap& structsAndAliasesMap, MutArr<StructInst*>& delayedStructInsts) {
			return getCommonTypes(path, ctx, structsAndAliasesMap, delayedStructInsts);
		});
}

const Result<const Module*, const Diagnostics> check(
	Arena& arena,
	const Arr<const Module*> imports,
	const FileAst ast,
	const PathAndStorageKind path,
	const IncludeCheck includeCheck
) {
	const Result<const IncludeCheck, const Diagnostics> res = checkWorker(
		arena,
		some<const Module*>(includeCheck.module),
		imports,
		ast,
		path,
		[&](
			__attribute__((unused)) CheckCtx& ctx,
			__attribute__((unused)) const StructsAndAliasesMap& _,
			__attribute__((unused)) const MutArr<StructInst*>& __
		) {
			return success<const CommonTypes, const Diagnostics>(includeCheck.commonTypes);
		});
	return mapSuccess(res, [](const IncludeCheck ic) { return ic.module; });
}

