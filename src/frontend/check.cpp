#include "./check.h"

#include "../util/arrUtil.h"
#include "../util/dictBuilder.h"
#include "../util/dictUtil.h"
#include "../util/resultUtil.h"
#include "./checkCtx.h"
#include "./checkExpr.h" // checkFunctionBody
#include "./typeFromAst.h"

namespace {
	const Opt<const StructDecl*> getCommonTemplateType(
		const StructsAndAliasesMap& structsAndAliasesMap,
		const Str name,
		const size_t expectedTypeParams
	) {
		const Opt<const StructOrAlias> res = structsAndAliasesMap.get(name);
		if (res.has()) {
			// may fail -- builtin Template should not be an alias
			const StructDecl* decl = res.force().asDecl();
			if (decl->typeParams.size != expectedTypeParams)
				todo<void>("getCommonTemplateType");
			return some<const StructDecl*>(decl);
		} else
			return none<const StructDecl*>();
	}

	const Opt<const StructInst*> getCommonNonTemplateType(
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
			assert(isEmpty(structOrAlias.typeParams()));
			return some<const StructInst*>(structOrAlias.match(
				[](const StructAlias* a) { return a->target(); },
				[&](const StructDecl* s) {
					return instantiateStruct(
						arena,
						s,
						emptyArr<const Type>(),
						some<MutArr<StructInst*>*>(&delayedStructInsts));
				}));
		}
	}

	const Result<const CommonTypes, const Arr<const Diagnostic>> getCommonTypes(
		const PathAndStorageKind path,
		CheckCtx& ctx,
		const StructsAndAliasesMap& structsAndAliasesMap,
		MutArr<StructInst*>& delayedStructInsts
	) {
		// non-template types
		auto ng = [&](const CStr s) -> const Opt<const StructInst*> {
			return getCommonNonTemplateType(ctx.arena, structsAndAliasesMap, strLiteral(s), delayedStructInsts);
		};
		const Opt<const StructInst*>
			_bool = ng("bool"),
			int64 = ng("int64"),
			_char = ng("char"),
			_ctx = ng("ctx"),
			str = ng("str"),
			_void = ng("void"),
			anyPtr = ng("any-ptr");

		// gemplate types
		auto com = [&](const CStr name, const size_t nTypeParameters) -> const Opt<const StructDecl*> {
			return getCommonTemplateType(structsAndAliasesMap, strLiteral(name), nTypeParameters);
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

		if (_bool.has() && _char.has() && int64.has() && str.has() && _void.has() && anyPtr.has() &&
			opt.has() && some.has() && none.has() &&
			byVal.has() && arr.has() && fut.has() &&
			fun0.has() && fun1.has() && fun2.has() &&
			remoteFun0.has() && remoteFun1.has() && remoteFun2.has())
			return success<const CommonTypes, const Arr<const Diagnostic>>(
				CommonTypes{
					_bool.force(),
					_char.force(),
					_ctx.force(),
					int64.force(),
					str.force(),
					_void.force(),
					anyPtr.force(),
					arrLiteral<const StructDecl*>(ctx.arena, opt.force(), some.force(), none.force()),
					byVal.force(),
					arr.force(),
					mutArr.force(),
					fut.force(),
					arrLiteral<const StructDecl*>(ctx.arena, fun0.force(), fun1.force(), fun2.force()),
					arrLiteral<const StructDecl*>(ctx.arena, remoteFun0.force(), remoteFun1.force(), remoteFun2.force())});
		else {
			const Diagnostic diag = Diagnostic{path, SourceRange::empty(), Diag{Diag::CommonTypesMissing{}}};
			return failure<const CommonTypes, const Arr<const Diagnostic>>(arrLiteral<const Diagnostic>(ctx.arena, diag));
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
		for (const size_t i : Range{typeParams.size})
			for (const size_t prev_i : Range{i})
				if (strEq(at(typeParams, prev_i).name, at(typeParams, i).name))
					ctx.diag(at(typeParams, i).range, Diag{Diag::ParamShadowsPrevious{Diag::ParamShadowsPrevious::Kind::typeParam}});
		return typeParams;
	}

	void collectTypeParamsInAst(Arena& arena, const TypeAst ast, ArrBuilder<const TypeParam>& res) {
		return ast.match(
			[&](const TypeAst::TypeParam tp) {
				if (!exists(res.tempAsArr(), [&](const TypeParam it) { return strEq(it.name, tp.name); }))
					res.add(arena, TypeParam{tp.range, copyStr(arena, tp.name), res.size()});
			},
			[&](const TypeAst::InstStruct i) {
				for (const TypeAst arg : i.typeArgs)
					collectTypeParamsInAst(arena, arg, res);
			});
	}

	const Arr<const TypeParam> collectTypeParams(Arena& arena, const SigAst ast) {
		ArrBuilder<const TypeParam> res {};
		collectTypeParamsInAst(arena, ast.returnType, res);
		for (const ParamAst p : ast.params)
			collectTypeParamsInAst(arena, p.type, res);
		return res.finish();
	}

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
		for (const size_t i : Range{params.size})
			for (const size_t prev_i : Range{i})
				if (strEq(at(params, prev_i).name, at(params, i).name))
					ctx.diag(at(params, i).range, Diag{Diag::ParamShadowsPrevious{Diag::ParamShadowsPrevious::Kind::param}});
		return params;
	}

	const Sig checkSig(
		CheckCtx& ctx,
		const SigAst ast,
		const Arr<const TypeParam> typeParams,
		const StructsAndAliasesMap& structsAndAliasesMap,
		DelayStructInsts delayStructInsts
	) {
		const TypeParamsScope typeParamsScope = TypeParamsScope{typeParams};
		const Arr<const Param> params = checkParams(ctx, ast.params, structsAndAliasesMap, typeParamsScope, delayStructInsts);
		const Type returnType = typeFromAst(ctx, ast.returnType, structsAndAliasesMap, typeParamsScope, delayStructInsts);
		return Sig{ast.range, ctx.copyStr(ast.name), returnType, params};
	}

	const Sig checkSigNoOwnTypeParams(
		CheckCtx& ctx,
		const SigAst ast,
		const Arr<const TypeParam> outerTypeParams,
		const StructsAndAliasesMap& structsAndAliasesMap,
		MutArr<StructInst*>& delayStructInsts
	) {
		return checkSig(
			ctx,
			ast,
			outerTypeParams,
			structsAndAliasesMap,
			some<MutArr<StructInst*>*>(&delayStructInsts));
	}

	const Arr<const SpecDecl> checkSpecDecls(
		CheckCtx& ctx,
		const StructsAndAliasesMap& structsAndAliasesMap,
		const Arr<const SpecDeclAst> asts,
		MutArr<StructInst*>& delayStructInsts
	) {
		return map<const SpecDecl>{}(ctx.arena, asts, [&](const SpecDeclAst ast) {
			const Arr<const TypeParam> typeParams = checkTypeParams(ctx, ast.typeParams);
			const Arr<const Sig> sigs = map<const Sig>{}(ctx.arena, ast.sigs, [&](const SigAst it) {
				return checkSigNoOwnTypeParams(ctx, it, typeParams, structsAndAliasesMap, delayStructInsts);
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
				ctx,
				ast.target,
				structsAndAliasesMap,
				TypeParamsScope{structAlias.typeParams},
				some<MutArr<StructInst*>*>(&delayStructInsts));
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
		for (const size_t i : Range{a.size})
			for (const size_t j : Range{i})
				if (eq(at(a, i), at(a, j)))
					todo<void>("checkNoEqual");
	}

	const StructBody checkFields(
		CheckCtx& ctx,
		const StructsAndAliasesMap& structsAndAliasesMap,
		const StructDecl& strukt,
		const StructDeclAst::Body::Fields f,
		MutArr<StructInst*>& delayStructInsts
	) {
		const Arr<const StructField> fields = mapWithIndex<const StructField>{}(
			ctx.arena,
			f.fields,
			[&](const StructDeclAst::Body::Fields::Field field, const size_t index) {
				const Type fieldType = typeFromAst(ctx, field.type, structsAndAliasesMap, TypeParamsScope{strukt.typeParams}, some<MutArr<StructInst*>*>(&delayStructInsts));
				if (isPurityWorse(fieldType.purity(), strukt.purity))
					ctx.diag(field.range, Diag{Diag::FieldPurityWorseThanStructPurity{fieldType.purity(), strukt.purity}});
				return StructField{field.isMutable, ctx.copyStr(field.name), fieldType, index};
			});
		checkNoEqual(fields, [](const StructField a, const StructField b) { return strEq(a.name, b.name); });
		return StructBody{StructBody::Fields{fields}};
	}

	void checkStructBodies(
		CheckCtx& ctx,
		const CommonTypes& commonTypes,
		const StructsAndAliasesMap& structsAndAliasesMap,
		const Arr<StructDecl> structs,
		const Arr<const StructDeclAst> asts,
		MutArr<StructInst*>& delayStructInsts
	) {
		DelayStructInsts delay = some<MutArr<StructInst*>*>(&delayStructInsts);
		zip(structs, asts, [&](StructDecl& strukt, const StructDeclAst& ast) {
			const StructBody body = ast.body.match(
				[](const StructDeclAst::Body::Builtin) {
					return StructBody{StructBody::Builtin{}};
				},
				[&](const StructDeclAst::Body::Fields f) {
					return checkFields(ctx, structsAndAliasesMap, strukt, f, delayStructInsts);
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
							const Sig sig = checkSigNoOwnTypeParams(ctx, it, strukt.typeParams, structsAndAliasesMap, delayStructInsts);
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
				[](const StructBody::Builtin) {},
				[](const StructBody::Fields) {},
				[](const StructBody::Union u) {
					for (const StructInst* member : u.members)
						if (member->decl->body().isUnion())
							todo<void>("unions can't contain unions");
				},
				[&](const StructBody::Iface i) {
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
		DictBuilder<const Str, const StructOrAlias, compareStr> d {};
		for (const StructDecl* decl : ptrsRange(structs))
			d.add(ctx.arena, decl->name, StructOrAlias{decl});
		for (const StructAlias* a : ptrsRange(aliases))
			d.add(ctx.arena, a->name, StructOrAlias{a});
		return d.finish(ctx.arena, [&](const Str name, const StructOrAlias, const StructOrAlias b) {
			ctx.diag(b.range(), Diag{Diag::DuplicateDeclaration{Diag::DuplicateDeclaration::Kind::structOrAlias, name}});
		});
	}

	template <typename T>
	const Dict<const Str, T*, compareStr> buildDeclsDict(CheckCtx& ctx, const Arr<T> ts, const Diag::DuplicateDeclaration::Kind kind) {
		return buildDict<const Str, T*, compareStr>{}(
			ctx.arena,
			ts,
			[](T& it) {
				return KeyValuePair<const Str, T*>{it.name, &it};
			},
			[&](const Str name, T*, T* t) {
				ctx.diag(t->range, Diag{Diag::DuplicateDeclaration{kind, name}});
			});
	}

	struct FunsAndMap {
		const Arr<const FunDecl> funs;
		const FunsMap funsMap;
	};

	const Arr<const SpecInst*> checkSpecUses(
		CheckCtx& ctx,
		const Arr<const SpecUseAst> asts,
		const StructsAndAliasesMap& structsAndAliasesMap,
		const SpecsMap& specsMap,
		const TypeParamsScope& typeParamsScope
	) {
		return mapOp<const SpecInst*>{}(ctx.arena, asts, [&](const SpecUseAst ast) {
			Opt<const SpecDecl*> opSpec = tryFindSpec(ctx, ast.spec, ast.range, specsMap);
			if (opSpec.has()) {
				const SpecDecl* spec = opSpec.force();
				const Arr<const Type> typeArgs = typeArgsFromAsts(
					ctx,
					ast.typeArgs,
					structsAndAliasesMap,
					typeParamsScope,
					none<MutArr<StructInst*>*>());
				if (typeArgs.size != spec->typeParams.size) {
					ctx.diag(ast.range, Diag{Diag::WrongNumberTypeArgsForSpec{spec, spec->typeParams.size, typeArgs.size}});
					return none<const SpecInst*>();
				} else
					return some<const SpecInst*>(instantiateSpec(ctx.arena, spec, typeArgs));
			} else {
				ctx.diag(ast.range, Diag{Diag::NameNotFound{ctx.copyStr(ast.spec), Diag::NameNotFound::Kind::spec}});
				return none<const SpecInst*>();
			}
		});
	}

	const FunsAndMap checkFuns(
		CheckCtx& ctx,
		const CommonTypes& commonTypes,
		const SpecsMap& specsMap,
		const StructsAndAliasesMap& structsAndAliasesMap,
		const Arr<const FunDeclAst> asts
	) {
		const Arr<FunDecl> funs = map<FunDecl>{}(ctx.arena, asts, [&](const FunDeclAst funAst) {
			const Arr<const TypeParam> typeParams = isEmpty(funAst.typeParams)
				? collectTypeParams(ctx.arena, funAst.sig)
				: checkTypeParams(ctx, funAst.typeParams);
			const Sig sig = checkSig(
				ctx,
				funAst.sig,
				typeParams,
				structsAndAliasesMap,
				none<MutArr<StructInst*>*>());
			const Arr<const SpecInst*> specUses = checkSpecUses(ctx, funAst.specUses, structsAndAliasesMap, specsMap, TypeParamsScope{typeParams});
			const FunFlags flags = FunFlags{funAst.noCtx, funAst.summon, funAst.unsafe, funAst.trusted};
			return FunDecl{funAst.isPublic, flags, sig, typeParams, specUses};
		});

		const FunsMap funsMap = buildMultiDict<const Str, const FunDecl*, compareStr>{}(
			ctx.arena,
			funs,
			[](const FunDecl& it) {
				return KeyValuePair<const Str, const FunDecl*>{it.name(), &it};
			});

		zip(funs, asts, [&](FunDecl& fun, const FunDeclAst funAst) {
			fun.setBody(funAst.body.match(
				[](const FunBodyAst::Builtin) {
					return FunBody{FunBody::Builtin{}};
				},
				[](const FunBodyAst::Extern) {
					return FunBody{FunBody::Extern{}};
				},
				[&](const ExprAst e) {
					return FunBody{checkFunctionBody(ctx, e, structsAndAliasesMap, funsMap, &fun, commonTypes)};
				}));
		});

		return FunsAndMap{asConst(funs), funsMap};
	}

	template <typename GetCommonTypes>
	const Result<const IncludeCheck, const Arr<const Diagnostic>> checkWorker(
		Arena& arena,
		const Opt<const Module*> include,
		const Arr<const Module*> imports,
		const FileAst ast,
		const PathAndStorageKind path,
		GetCommonTypes getCommonTypes
	) {
		CheckCtx ctx {arena, path, include, imports};

		// Since structs may refer to each other, first get a structsAndAliasesMap, *then* fill in bodies
		const Arr<StructDecl> structs = checkStructsInitial(ctx, ast.structs);
		const Arr<StructAlias> structAliases = checkStructAliasesInitial(ctx, ast.structAliases);
		const StructsAndAliasesMap structsAndAliasesMap = buildStructsAndAliasesDict(ctx, structs, structAliases);

		// We need to create StructInsts when filling in struct bodies.
		// But when creating a StructInst, we usuallly want to fill in its body.
		// In case the decl body isn't available yet, we'll delay creating the StructInst body, which isn't needed until expr checking.
		MutArr<StructInst*> delayStructInsts = MutArr<StructInst*>{};

		checkStructAliasTargets(ctx, structsAndAliasesMap, structAliases, ast.structAliases, delayStructInsts);
		const Arr<const SpecDecl> specs = checkSpecDecls(ctx, structsAndAliasesMap, ast.specs, delayStructInsts);
		const SpecsMap specsMap = buildDeclsDict<const SpecDecl>(ctx, specs, Diag::DuplicateDeclaration::Kind::spec);

		if (ctx.hasDiags())
			return failure<const IncludeCheck, const Arr<const Diagnostic>>(ctx.diags());
		else {
			const Result<const CommonTypes, const Arr<const Diagnostic>> commonTypes = getCommonTypes(ctx, structsAndAliasesMap, delayStructInsts);
			return flatMapSuccess<const IncludeCheck, const Arr<const Diagnostic>>{}(commonTypes, [&](const CommonTypes commonTypes) {
				checkStructBodies(ctx, commonTypes, structsAndAliasesMap, structs, ast.structs, delayStructInsts);
				for (StructInst* i : freeze(delayStructInsts))
					i->setBody(instantiateStructBody(arena, i->decl, i->typeArgs));
				const FunsAndMap funsAndMap = checkFuns(ctx, commonTypes, specsMap, structsAndAliasesMap, ast.funs);
				if (ctx.hasDiags())
					return failure<const IncludeCheck, const Arr<const Diagnostic>>(ctx.diags());
				else {
					const Module* mod = arena.nu<const Module>()(
						path,
						imports,
						asConst(structs),
						specs,
						funsAndMap.funs,
						structsAndAliasesMap,
						specsMap,
						funsAndMap.funsMap);
					return success<const IncludeCheck, const Arr<const Diagnostic>>(IncludeCheck{mod, commonTypes});
				}
			});
		}
	}
}

const Result<const IncludeCheck, const Arr<const Diagnostic>> checkIncludeNz(
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

const Result<const Module*, const Arr<const Diagnostic>> check(
	Arena& arena,
	const Arr<const Module*> imports,
	const FileAst ast,
	const PathAndStorageKind path,
	const IncludeCheck includeCheck
) {
	const Result<const IncludeCheck, const Arr<const Diagnostic>> res = checkWorker(
		arena,
		some<const Module*>(includeCheck.module),
		imports,
		ast,
		path,
		[&](CheckCtx&, const StructsAndAliasesMap&, const MutArr<StructInst*>&) {
			return success<const CommonTypes, const Arr<const Diagnostic>>(includeCheck.commonTypes);
		});
	return mapSuccess<const Module*>{}(res, [](const IncludeCheck ic) { return ic.module; });
}

