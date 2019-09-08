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
		const StructsAndAliasesMap structsAndAliasesMap,
		const Sym name,
		const size_t expectedTypeParams
	) {
		const Opt<const StructOrAlias> res =
			getAt<const Sym, const StructOrAlias, compareSym>(structsAndAliasesMap, name);
		if (has(res)) {
			// may fail -- builtin Template should not be an alias
			const StructDecl* decl = force(res).asDecl();
			if (size(decl->typeParams) != expectedTypeParams)
				todo<void>("getCommonTemplateType");
			return some<const StructDecl*>(decl);
		} else
			return none<const StructDecl*>();
	}

	const Opt<const StructInst*> getCommonNonTemplateType(
		Arena* arena,
		const StructsAndAliasesMap structsAndAliasesMap,
		const Sym name,
		MutArr<StructInst*>* delayedStructInsts
	) {
		const Opt<const StructOrAlias> opStructOrAlias =
			getAt<const Sym, const StructOrAlias, compareSym>(structsAndAliasesMap, name);
		if (!has(opStructOrAlias))
			return none<const StructInst*>();
		else {
			const StructOrAlias structOrAlias = force(opStructOrAlias);
			assert(isEmpty(structOrAlias.typeParams()));
			return structOrAlias.match(
				[](const StructAlias* a) {
					return a->target();
				},
				[&](const StructDecl* s) {
					return some<const StructInst*>(instantiateStruct(
						arena,
						s,
						emptyArr<const Type>(),
						some<MutArr<StructInst*>*>(delayedStructInsts)));
				});
		}
	}

	const Result<const CommonTypes, const Arr<const Diagnostic>> getCommonTypes(
		CheckCtx* ctx,
		const StructsAndAliasesMap structsAndAliasesMap,
		MutArr<StructInst*>* delayedStructInsts
	) {
		// non-template types
		auto nonTemplate = [&](const CStr s) -> const Opt<const StructInst*> {
			return getCommonNonTemplateType(
				ctx->arena,
				structsAndAliasesMap,
				shortSymAlphaLiteral(s),
				delayedStructInsts);
		};
		const Opt<const StructInst*>
			_bool = nonTemplate("bool"),
			int64 = nonTemplate("int"),
			_char = nonTemplate("char"),
			_ctx = nonTemplate("ctx"),
			str = nonTemplate("str"),
			_void = nonTemplate("void"),
			anyPtr = nonTemplate("any-ptr");

		// gemplate types
		auto com = [&](const CStr name, const size_t nTypeParameters) -> const Opt<const StructDecl*> {
			return getCommonTemplateType(structsAndAliasesMap, shortSymAlphaLiteral(name), nTypeParameters);
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
			sendFun0 = com("send-fun0", 1),
			sendFun1 = com("send-fun1", 2),
			sendFun2 = com("send-fun2", 3);

		if (has(_bool) && has(_char) && has(int64) && has(str) && has(_void) && has(anyPtr) &&
			has(opt) && has(some) && has(none) &&
			has(byVal) && has(arr) && has(fut) &&
			has(fun0) && has(fun1) && has(fun2) &&
			has(sendFun0) && has(sendFun1) && has(sendFun2))
			return success<const CommonTypes, const Arr<const Diagnostic>>(
				CommonTypes{
					force(_bool),
					force(_char),
					force(_ctx),
					force(int64),
					force(str),
					force(_void),
					force(anyPtr),
					arrLiteral<const StructDecl*>(ctx->arena, { force(opt), force(some), force(none) }),
					force(byVal),
					force(arr),
					force(mutArr),
					force(fut),
					arrLiteral<const StructDecl*>(ctx->arena, { force(fun0), force(fun1), force(fun2) }),
					arrLiteral<const StructDecl*>(ctx->arena, { force(sendFun0), force(sendFun1), force(sendFun2) })});
		else {
			const Diagnostic diag = Diagnostic{ctx->path, SourceRange::empty(), Diag{Diag::CommonTypesMissing{}}};
			return failure<
				const CommonTypes,
				const Arr<const Diagnostic>
			>(arrLiteral<const Diagnostic>(ctx->arena, { diag }));
		}
	}

	const Arr<const TypeParam> checkTypeParams(CheckCtx* ctx, const Arr<const TypeParamAst> asts) {
		const Arr<const TypeParam> typeParams = mapWithIndex<const TypeParam>{}(
			ctx->arena,
			asts,
			[&](const TypeParamAst& ast, const size_t i) {
				return TypeParam{ast.range, ast.name, i};
			});
		for (const size_t i : Range{size(typeParams)})
			for (const size_t prev_i : Range{i})
				if (symEq(at(typeParams, prev_i).name, at(typeParams, i).name))
					addDiag(
						ctx,
						at(typeParams, i).range,
						Diag{Diag::ParamShadowsPrevious{Diag::ParamShadowsPrevious::Kind::typeParam}});
		return typeParams;
	}

	void collectTypeParamsInAst(Arena* arena, const TypeAst ast, ArrBuilder<const TypeParam>* res) {
		return ast.match(
			[&](const TypeAst::TypeParam tp) {
				if (!exists(arrBuilderTempAsArr(res), [&](const TypeParam it) { return symEq(it.name, tp.name); }))
					add<const TypeParam>(arena, res, TypeParam{tp.range, tp.name, arrBuilderSize(res)});
			},
			[&](const TypeAst::InstStruct i) {
				for (const TypeAst arg : i.typeArgs)
					collectTypeParamsInAst(arena, arg, res);
			});
	}

	const Arr<const TypeParam> collectTypeParams(Arena* arena, const SigAst ast) {
		ArrBuilder<const TypeParam> res {};
		collectTypeParamsInAst(arena, ast.returnType, &res);
		for (const ParamAst p : ast.params)
			collectTypeParamsInAst(arena, p.type, &res);
		return finishArr(&res);
	}

	const Arr<const Param> checkParams(
		CheckCtx* ctx,
		const Arr<const ParamAst> asts,
		const StructsAndAliasesMap structsAndAliasesMap,
		const TypeParamsScope typeParamsScope,
		DelayStructInsts delayStructInsts
	) {
		const Arr<const Param> params = mapWithIndex<const Param>{}(
			ctx->arena,
			asts,
			[&](const ParamAst ast, const size_t index) {
				const Type type = typeFromAst(ctx, ast.type, structsAndAliasesMap, typeParamsScope, delayStructInsts);
				return Param{ast.range, ast.name, type, index};
			});
		for (const size_t i : Range{size(params)})
			for (const size_t prev_i : Range{i})
				if (symEq(at(params, prev_i).name, at(params, i).name))
					addDiag(
						ctx,
						at(params, i).range,
						Diag{Diag::ParamShadowsPrevious{Diag::ParamShadowsPrevious::Kind::param}});
		return params;
	}

	const Sig checkSig(
		CheckCtx* ctx,
		const SigAst ast,
		const Arr<const TypeParam> typeParams,
		const StructsAndAliasesMap structsAndAliasesMap,
		DelayStructInsts delayStructInsts
	) {
		const TypeParamsScope typeParamsScope = TypeParamsScope{typeParams};
		const Arr<const Param> params = checkParams(
			ctx,
			ast.params,
			structsAndAliasesMap,
			typeParamsScope,
			delayStructInsts);
		const Type returnType = typeFromAst(
			ctx,
			ast.returnType,
			structsAndAliasesMap,
			typeParamsScope,
			delayStructInsts);
		return Sig{ast.range, ast.name, returnType, params};
	}

	const Arr<const SpecDecl> checkSpecDecls(
		CheckCtx* ctx,
		const StructsAndAliasesMap structsAndAliasesMap,
		const Arr<const SpecDeclAst> asts
	) {
		return map<const SpecDecl>{}(ctx->arena, asts, [&](const SpecDeclAst ast) {
			const Arr<const TypeParam> typeParams = checkTypeParams(ctx, ast.typeParams);
			const Arr<const Sig> sigs = map<const Sig>{}(ctx->arena, ast.sigs, [&](const SigAst it) {
				return checkSig(ctx, it, typeParams, structsAndAliasesMap, none<MutArr<StructInst*>*>());
			});
			return SpecDecl{ast.range, ast.isPublic, ast.name, typeParams, sigs};
		});
	}

	const Arr<StructAlias> checkStructAliasesInitial(CheckCtx* ctx, const Arr<const StructAliasAst> asts) {
		return map<StructAlias>{}(ctx->arena, asts, [&](const StructAliasAst ast) {
			return StructAlias{ast.range, ast.isPublic, ast.name, checkTypeParams(ctx, ast.typeParams)};
		});
	}

	struct PurityAndForceSendable {
		const Purity purity;
		const Bool forceSendable;
	};

	const PurityAndForceSendable getPurityFromAst(const StructDeclAst ast) {
		// Note: purity is taken for granted here, and verified later when we check the body.
		if (ast.body.isIface()) {
			if (has(ast.purity))
				todo<void>("iface should not have explicit purity, is always sendable");
			return PurityAndForceSendable{Purity::sendable, False};
		} else if (has(ast.purity))
			switch (force(ast.purity)) {
				case PuritySpecifier::sendable:
					return PurityAndForceSendable{Purity::sendable, False};
				case PuritySpecifier::forceSendable:
					return PurityAndForceSendable{Purity::sendable, True};
				case PuritySpecifier::mut:
					return PurityAndForceSendable{Purity::mut, False};
				default:
					return unreachable<const PurityAndForceSendable>();
			}
		else
			return PurityAndForceSendable{Purity::data, False};
	}

	const Arr<StructDecl> checkStructsInitial(CheckCtx* ctx, const Arr<const StructDeclAst> asts) {
		return map<StructDecl>{}(ctx->arena, asts, [&](const StructDeclAst ast) {
			const PurityAndForceSendable p = getPurityFromAst(ast);
			return StructDecl{
				ast.range,
				ast.isPublic,
				ast.name,
				checkTypeParams(ctx, ast.typeParams),
				p.purity,
				p.forceSendable};
		});
	}

	void checkStructAliasTargets(
		CheckCtx* ctx,
		const StructsAndAliasesMap structsAndAliasesMap,
		Arr<StructAlias> aliases,
		const Arr<const StructAliasAst> asts,
		MutArr<StructInst*>* delayStructInsts
	) {
		zipPtrs(aliases, asts, [&](StructAlias* structAlias, const StructAliasAst* ast) {
			structAlias->setTarget(instStructFromAst(
				ctx,
				ast->target,
				structsAndAliasesMap,
				TypeParamsScope{structAlias->typeParams},
				some<MutArr<StructInst*>*>(delayStructInsts)));
		});
	}

	void checkSendable(CheckCtx* ctx, const SourceRange range, const Type type) {
		if (!typeIsPossiblySendable(type))
			addDiag(ctx, range, Diag{Diag::TypeNotSendable{}});
	}

	//TODO:MOVE
	template <typename T, typename Cb>
	void everyPairWithIndex(const Arr<T> a, Cb cb) {
		for (const size_t i : Range{size(a)})
			for (const size_t j : Range{i})
				cb(at(a, i), at(a, j), i, j);
	}

	//TODO:MOVE
	template <typename T, typename Cb>
	void everyPair(const Arr<T> a, Cb cb) {
		for (const size_t i : Range{size(a)})
			for (const size_t j : Range{i})
				cb(at(a, i), at(a, j));
	}

	const Opt<const ForcedByValOrRef> getForcedByValOrRef(const Opt<const ExplicitByValOrRef> e) {
		if (has(e))
			switch (force(e)) {
				case ExplicitByValOrRef::byVal:
					return some<const ForcedByValOrRef>(ForcedByValOrRef::byVal);
				case ExplicitByValOrRef::byRef:
					return some<const ForcedByValOrRef>(ForcedByValOrRef::byRef);
				default:
					assert(0);
			}
		else
			return none<const ForcedByValOrRef>();
	}

	const StructBody checkRecord(
		CheckCtx* ctx,
		const StructsAndAliasesMap structsAndAliasesMap,
		const StructDecl* strukt,
		const StructDeclAst::Body::Record r,
		MutArr<StructInst*>* delayStructInsts
	) {
		const Opt<const ForcedByValOrRef> forcedByValOrRef = getForcedByValOrRef(r.explicitByValOrRef);
		const Bool forcedByVal = _and(has(forcedByValOrRef), force(forcedByValOrRef) == ForcedByValOrRef::byVal);

		const Arr<const RecordField> fields = mapWithIndex<const RecordField>{}(
			ctx->arena,
			r.fields,
			[&](const StructDeclAst::Body::Record::Field field, const size_t index) {
				const Type fieldType = typeFromAst(
					ctx,
					field.type,
					structsAndAliasesMap,
					TypeParamsScope{strukt->typeParams},
					some<MutArr<StructInst*>*>(delayStructInsts));
				if (isPurityWorse(fieldType.purity(), strukt->purity) && !strukt->forceSendable)
					addDiag(ctx, field.range, Diag{Diag::PurityOfFieldWorseThanRecord{strukt, fieldType}});
				if (field.isMutable) {
					const Opt<const Diag::MutFieldNotAllowed::Reason> reason =
						strukt->purity != Purity::mut && !strukt->forceSendable
							? some<const Diag::MutFieldNotAllowed::Reason>(
								Diag::MutFieldNotAllowed::Reason::recordIsNotMut)
						: forcedByVal
							? some<const Diag::MutFieldNotAllowed::Reason>(
								Diag::MutFieldNotAllowed::Reason::recordIsForcedByVal)
						: none<const Diag::MutFieldNotAllowed::Reason>();
					if (has(reason))
						addDiag(ctx, field.range, Diag{Diag::MutFieldNotAllowed{force(reason)}});
				}
				return RecordField{field.range, field.isMutable, field.name, fieldType, index};
			});
		everyPair(fields, [&](const RecordField a, const RecordField b) {
			if (symEq(a.name, b.name))
				addDiag(
					ctx,
					b.range,
					Diag{Diag::DuplicateDeclaration{Diag::DuplicateDeclaration::Kind::field, a.name}});
		});

		//const Bool x = forcedByVal && some(fields, [&](const RecordField f) {
		//	const Bool res = f.isMutable;
		//	if (res)
		//		addDiag(ctx, f.range, Diag{Diag::})
		//	return res;
		//}));

		return StructBody{StructBody::Record{forcedByValOrRef, fields}};
	}

	void checkStructBodies(
		CheckCtx* ctx,
		const CommonTypes* commonTypes,
		const StructsAndAliasesMap structsAndAliasesMap,
		const Arr<StructDecl> structs,
		const Arr<const StructDeclAst> asts,
		MutArr<StructInst*>* delayStructInsts
	) {
		DelayStructInsts delay = some<MutArr<StructInst*>*>(delayStructInsts);
		zipPtrs(structs, asts, [&](StructDecl* strukt, const StructDeclAst* ast) {
			const StructBody body = ast->body.match(
				[](const StructDeclAst::Body::Builtin) {
					return StructBody{StructBody::Builtin{}};
				},
				[&](const StructDeclAst::Body::Record r) {
					return checkRecord(ctx, structsAndAliasesMap, strukt, r, delayStructInsts);
				},
				[&](const StructDeclAst::Body::Union un) {
					const Opt<const Arr<const StructInst*>> members = mapOrNone<const StructInst*>{}(
						ctx->arena,
						un.members,
						[&](const TypeAst::InstStruct it) {
							const Opt<const StructInst*> res = instStructFromAst(
								ctx,
								it,
								structsAndAliasesMap,
								TypeParamsScope{strukt->typeParams},
								delay);
							if (has(res) && isPurityWorse(force(res)->purity, strukt->purity))
								addDiag(ctx, it.range, Diag{Diag::PurityOfMemberWorseThanUnion{strukt, force(res)}});
							return res;
						});
					if (has(members)) {
						everyPairWithIndex(
							force(members),
							[&](const StructInst* a, const StructInst* b, const size_t, const size_t bIndex) {
								if (ptrEquals(a->decl, b->decl))
									addDiag(
										ctx,
										at(un.members, bIndex).range,
										Diag{Diag::DuplicateDeclaration{
											Diag::DuplicateDeclaration::Kind::unionMember,
											a->decl->name}});
							});
						return StructBody{StructBody::Union{force(members)}};
					} else
						return StructBody{StructBody::Bogus{}};
				},
				[&](const StructDeclAst::Body::Iface i) {
					const Arr<const Message> messages = mapWithIndex<const Message>{}(
						ctx->arena,
						i.messages,
						[&](const SigAst it, const size_t idx) {
							const Sig sig = checkSig(
								ctx,
								it,
								strukt->typeParams,
								structsAndAliasesMap,
								some<MutArr<StructInst*>*>(delayStructInsts));
							const Sig sig2 = Sig{
								sig.range,
								sig.name,
								makeFutType(ctx->arena, commonTypes, sig.returnType),
								sig.params};
							return Message{sig2, idx};
						});
					// TODO: check no two messages have the same name
					return StructBody{StructBody::Iface{messages}};
				});
			strukt->setBody(body);
		});

		for (const StructDecl* strukt : ptrsRange(structs)) {
			strukt->body().match(
				[](const StructBody::Bogus) {},
				[](const StructBody::Builtin) {},
				[](const StructBody::Record) {},
				[](const StructBody::Union u) {
					for (const StructInst* member : u.members)
						if (member->decl->body().isUnion())
							todo<void>("unions can't contain unions");
				},
				[&](const StructBody::Iface i) {
					for (const Message message : i.messages) {
						const Sig sig = message.sig;
						checkSendable(ctx, sig.range, sig.returnType);
						for (const Param param : sig.params)
							checkSendable(ctx, param.range, param.type);
					}
				});
		}
	}

	const StructsAndAliasesMap buildStructsAndAliasesDict(
		CheckCtx* ctx,
		const Arr<StructDecl> structs,
		const Arr<StructAlias> aliases
	) {
		DictBuilder<const Sym, const StructOrAlias, compareSym> d {};
		for (const StructDecl* decl : ptrsRange(structs))
			addToDict<const Sym, const StructOrAlias, compareSym>(ctx->arena, &d, decl->name, StructOrAlias{decl});
		for (const StructAlias* a : ptrsRange(aliases))
			addToDict<const Sym, const StructOrAlias, compareSym>(ctx->arena, &d, a->name, StructOrAlias{a});
		return finishDict<const Sym, const StructOrAlias, compareSym>(
			ctx->arena,
			&d,
			[&](const Sym name, const StructOrAlias, const StructOrAlias b) {
				addDiag(
					ctx,
					b.range(),
					Diag{Diag::DuplicateDeclaration{Diag::DuplicateDeclaration::Kind::structOrAlias, name}});
			});
	}

	struct FunsAndMap {
		const Arr<FunDecl> funs;
		const FunsMap funsMap;
	};

	const Arr<const SpecInst*> checkSpecUses(
		CheckCtx* ctx,
		const Arr<const SpecUseAst> asts,
		const StructsAndAliasesMap structsAndAliasesMap,
		const SpecsMap specsMap,
		const TypeParamsScope typeParamsScope
	) {
		return mapOp<const SpecInst*>{}(ctx->arena, asts, [&](const SpecUseAst ast) {
			Opt<const SpecDecl*> opSpec = tryFindSpec(ctx, ast.spec, ast.range, specsMap);
			if (has(opSpec)) {
				const SpecDecl* spec = force(opSpec);
				const Arr<const Type> typeArgs = typeArgsFromAsts(
					ctx,
					ast.typeArgs,
					structsAndAliasesMap,
					typeParamsScope,
					none<MutArr<StructInst*>*>());
				if (!sizeEq(typeArgs, spec->typeParams)) {
					addDiag(
						ctx,
						ast.range,
						Diag{Diag::WrongNumberTypeArgsForSpec{spec, size(spec->typeParams), size(typeArgs)}});
					return none<const SpecInst*>();
				} else
					return some<const SpecInst*>(instantiateSpec(ctx->arena, spec, typeArgs));
			} else {
				addDiag(ctx, ast.range, Diag{Diag::NameNotFound{ast.spec, Diag::NameNotFound::Kind::spec}});
				return none<const SpecInst*>();
			}
		});
	}

	const FunsAndMap checkFuns(
		CheckCtx* ctx,
		const CommonTypes* commonTypes,
		const SpecsMap specsMap,
		const StructsAndAliasesMap structsAndAliasesMap,
		const Arr<const FunDeclAst> asts
	) {
		const Arr<FunDecl> funs = map<FunDecl>{}(ctx->arena, asts, [&](const FunDeclAst funAst) {
			const Arr<const TypeParam> typeParams = isEmpty(funAst.typeParams)
				? collectTypeParams(ctx->arena, funAst.sig)
				: checkTypeParams(ctx, funAst.typeParams);
			const Sig sig = checkSig(
				ctx,
				funAst.sig,
				typeParams,
				structsAndAliasesMap,
				none<MutArr<StructInst*>*>());
			const Arr<const SpecInst*> specUses = checkSpecUses(
				ctx,
				funAst.specUses,
				structsAndAliasesMap,
				specsMap,
				TypeParamsScope{typeParams});
			const FunFlags flags = FunFlags{funAst.noCtx, funAst.summon, funAst.unsafe, funAst.trusted};
			return FunDecl{funAst.isPublic, flags, sig, typeParams, specUses};
		});

		const FunsMap funsMap = buildMultiDict<const Sym, const FunDecl*, compareSym>{}(
			ctx->arena,
			funs,
			[](const FunDecl* it) {
				return KeyValuePair<const Sym, const FunDecl*>{it->name(), it};
			});

		for (const FunDecl* f : ptrsRange(funs))
			addToMutSymSetOkIfPresent(ctx->arena, &ctx->programState->funNames, f->name());

		zipPtrs(funs, asts, [&](FunDecl* fun, const FunDeclAst* funAst) {
			fun->setBody(funAst->body.match(
				[](const FunBodyAst::Builtin) {
					return FunBody{FunBody::Builtin{}};
				},
				[](const FunBodyAst::Extern) {
					return FunBody{FunBody::Extern{}};
				},
				[&](const ExprAst e) {
					return FunBody{checkFunctionBody(ctx, e, structsAndAliasesMap, funsMap, fun, commonTypes)};
				}));
		});

		return FunsAndMap{funs, funsMap};
	}

	const Dict<const Sym, const SpecDecl*, compareSym> buildSpecsDict(CheckCtx* ctx, const Arr<const SpecDecl> specs) {
		return buildDict<const Sym, const SpecDecl*, compareSym>{}(
			ctx->arena,
			specs,
			[](const SpecDecl* it) {
				return KeyValuePair<const Sym, const SpecDecl*>{it->name, it};
			},
			[&](const Sym name, const SpecDecl*, const SpecDecl* s) {
				addDiag(ctx, s->range, Diag{Diag::DuplicateDeclaration{Diag::DuplicateDeclaration::Kind::spec, name}});
			});
	}


	const Module* checkWorkerAfterCommonTypes(
		CheckCtx* ctx,
		const CommonTypes* commonTypes,
		StructsAndAliasesMap structsAndAliasesMap,
		const Arr<StructDecl> structs,
		MutArr<StructInst*>* delayStructInsts,
		const Arr<const Module*> imports,
		const FileAst ast
	) {
		checkStructBodies(ctx, commonTypes, structsAndAliasesMap, structs, ast.structs, delayStructInsts);
		for (const StructDecl* s : ptrsRange(structs))
			if (s->body().isRecord())
				for (const RecordField f : s->body().asRecord().fields)
					addToMutSymSetOkIfPresent(ctx->arena, &ctx->programState->recordFieldNames, f.name);

		for (StructInst* i : freeze(delayStructInsts))
			i->setBody(instantiateStructBody(ctx->arena, i->decl, i->typeArgs));

		const Arr<const SpecDecl> specs = checkSpecDecls(ctx, structsAndAliasesMap, ast.specs);
		const SpecsMap specsMap = buildSpecsDict(ctx, specs);
		for (const SpecDecl* s : ptrsRange(specs))
			addToMutSymSetOkIfPresent(ctx->arena, &ctx->programState->specNames, s->name);

		const FunsAndMap funsAndMap = checkFuns(ctx, commonTypes, specsMap, structsAndAliasesMap, ast.funs);

		// Create a module unconditionally so every function will always have containingModule set, even in failure case
		const Module* mod = nu<const Module>{}(
			ctx->arena,
			ctx->path,
			imports,
			asConstArr<StructDecl>(structs),
			specs,
			asConstArr<FunDecl>(funsAndMap.funs),
			structsAndAliasesMap,
			specsMap,
			funsAndMap.funsMap);

		for (FunDecl* f : ptrsRange(funsAndMap.funs))
			f->setContainingModule(mod);

		return mod;
	}

	template <typename GetCommonTypes>
	const Result<const IncludeCheck, const Arr<const Diagnostic>> checkWorker(
		Arena* arena,
		ProgramState* programState,
		const Opt<const Module*> include,
		const Arr<const Module*> imports,
		const PathAndAst pathAndAst,
		GetCommonTypes getCommonTypes
	) {
		CheckCtx ctx {arena, programState, pathAndAst.pathAndStorageKind, include, imports};

		const FileAst ast = pathAndAst.ast;

		// Since structs may refer to each other, first get a structsAndAliasesMap, *then* fill in bodies
		const Arr<StructDecl> structs = checkStructsInitial(&ctx, ast.structs);
		for (const StructDecl* s : ptrsRange(structs))
			addToMutSymSetOkIfPresent(arena, &programState->structAndAliasNames, s->name);
		const Arr<StructAlias> structAliases = checkStructAliasesInitial(&ctx, ast.structAliases);
		for (const StructAlias* a : ptrsRange(structAliases))
			addToMutSymSetOkIfPresent(arena, &programState->structAndAliasNames, a->name);
		const StructsAndAliasesMap structsAndAliasesMap = buildStructsAndAliasesDict(&ctx, structs, structAliases);

		// We need to create StructInsts when filling in struct bodies.
		// But when creating a StructInst, we usuallly want to fill in its body.
		// In case the decl body isn't available yet,
		// we'll delay creating the StructInst body, which isn't needed until expr checking.
		MutArr<StructInst*> delayStructInsts = MutArr<StructInst*>{};

		checkStructAliasTargets(&ctx, structsAndAliasesMap, structAliases, ast.structAliases, &delayStructInsts);

		if (hasDiags(&ctx))
			return failure<const IncludeCheck, const Arr<const Diagnostic>>(diags(&ctx));
		else {
			const Result<const CommonTypes, const Arr<const Diagnostic>> commonTypesResult =
				getCommonTypes(&ctx, structsAndAliasesMap, &delayStructInsts);
			return flatMapSuccess<const IncludeCheck, const Arr<const Diagnostic>>{}(
				commonTypesResult,
				[&](const CommonTypes commonTypes) {
					const Module* mod = checkWorkerAfterCommonTypes(
						&ctx, &commonTypes, structsAndAliasesMap, structs, &delayStructInsts, imports, ast);
					return hasDiags(&ctx)
						? failure<const IncludeCheck, const Arr<const Diagnostic>>(diags(&ctx))
						: success<const IncludeCheck, const Arr<const Diagnostic>>(IncludeCheck{mod, commonTypes});
				});
		}
	}
}

const Result<const IncludeCheck, const Arr<const Diagnostic>> checkIncludeNz(
	Arena* arena,
	ProgramState* programState,
	const PathAndAst pathAndAst
) {
	return checkWorker(
		arena,
		programState,
		none<const Module*>(),
		emptyArr<const Module*>(),
		pathAndAst,
		[&](CheckCtx* ctx, const StructsAndAliasesMap structsAndAliasesMap, MutArr<StructInst*>* delayedStructInsts) {
			return getCommonTypes(ctx, structsAndAliasesMap, delayedStructInsts);
		});
}

const Result<const Module*, const Arr<const Diagnostic>> check(
	Arena* arena,
	ProgramState* programState,
	const Arr<const Module*> imports,
	const PathAndAst pathAndAst,
	const IncludeCheck includeCheck
) {
	const Result<const IncludeCheck, const Arr<const Diagnostic>> res = checkWorker(
		arena,
		programState,
		some<const Module*>(includeCheck.module),
		imports,
		pathAndAst,
		[&](CheckCtx*, const StructsAndAliasesMap, const MutArr<StructInst*>*) {
			return success<const CommonTypes, const Arr<const Diagnostic>>(includeCheck.commonTypes);
		});
	return mapSuccess<const Module*>{}(res, [](const IncludeCheck ic) { return ic.module; });
}

