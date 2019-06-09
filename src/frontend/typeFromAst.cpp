#include "./typeFromAst.h"

namespace {
	//TODO:MOVE
	template<typename T, typename Cb>
	const Opt<const T*> findInEither(const Arr<T> a, const Arr<T> b, Cb cb) {
		for (size_t i = 0; i < a.size; i++)
			if (cb(a[i]))
				return some<const T*>(a.getPtr(i));
		for (size_t i = 0; i < b.size; i++)
			if (cb(b[i]))
				return some<const T*>(b.getPtr(i));
		return none<const T*>();
	}

	template<typename TDecl, typename GetTMap>
	const Opt<TDecl> tryFindT(
		CheckCtx& ctx,
		const Str name,
		const SourceRange range,
		const Dict<const Str, TDecl, strEq> dict,
		Diag::NameNotFound::Kind kind,
		GetTMap getTMap
	) {
		Cell<const Opt<TDecl>> res = Cell<const Opt<TDecl>>{dict.get(name)};

		for (const Module* m : ctx.includeAndImportsRange()) {
			const Opt<TDecl> fromModule = getTMap(m).get(name);
			if (fromModule.has()) {
				if (res.get().has())
					todo<void>("Duplicate imports from different modules");
				else
					res.set(fromModule);
			}
		}

		const Opt<TDecl> r = res.get();
		if (!r.has())
			ctx.diag(range, Diag{Diag::NameNotFound{ctx.copyStr(name), kind}});
		return r;
	}
}

const Type instantiateType(Arena& arena, const Type type, const Arr<const TypeParam> typeParams, const Arr<const Type> typeArgs) {
	return type.match(
		/*bogus*/ []() { return Type::bogus(); },
		[&](const TypeParam* p) {
			const Opt<const Type*> op = tryGetTypeArg<const Type>(typeParams, typeArgs, p);
			return op.has() ? *op.force() : type;
		},
		[&](const StructInst* i) {
			return Type{instantiateStructInst(arena, i, typeParams, typeArgs)};
		});
}

const StructBody instantiateStructBody(Arena& arena, const StructDecl* decl, const Arr<const Type> typeArgs) {
	return decl->body().match(
		/*builtin*/ []() { return StructBody::builtin(); },
		[&](const StructBody::Fields f) {
			const Arr<const StructField> fields = map<const StructField>{}(arena, f.fields, [&](const StructField f) {
				return StructField{instantiateType(arena, f.type, decl->typeParams, typeArgs), f.name, f.index};
			});
			return StructBody{StructBody::Fields{fields}};
		},
		[&](const StructBody::Union u) {
			const Arr<const StructInst*> members = map<const StructInst*>{}(arena, u.members, [&](const StructInst* i) {
				return instantiateStructInst(arena, i, decl->typeParams, typeArgs);
			});
			return StructBody{StructBody::Union{members}};
		},
		[&](const StructBody::Iface i) {
			const Arr<const Message> messages = map<const Message>{}(arena, i.messages, [&](const Message m) {
				const Arr<const Param> params = map<const Param>{}(arena, m.sig.params, [&](const Param p) {
					return Param{p.range, p.name, instantiateType(arena, p.type, decl->typeParams, typeArgs), p.index};
				});
				const Sig sig = Sig{m.sig.range, m.sig.name, instantiateType(arena, m.sig.returnType, decl->typeParams, typeArgs), params};
				return Message{sig, m.index};
			});
			return StructBody{StructBody::Iface{messages}};
		});
}

const StructInst* instantiateStruct(
	Arena& arena,
	const StructDecl* decl,
	const Arr<const Type> typeArgs,
	DelayStructInsts delayStructInsts
) {
	for (const StructInst* si : decl->insts.tempAsArr())
		if (eachCorresponds(si->typeArgs, typeArgs, typeEquals))
			return si;

	const Purity purity = [&]() {
		Purity pur = decl->purity;
		for (const Type typeArg : typeArgs)
			pur = worsePurity(pur, typeArg.purity());
		return pur;
	}();

	StructInst* res = arena.nu<StructInst>()(decl, typeArgs, purity);
	if (decl->bodyIsSet())
		res->setBody(instantiateStructBody(arena, decl, typeArgs));
	else
		// We should only need to do this in the initial phase of settings struct bodies, which is when delayedStructInst is set.
		delayStructInsts.force().push(arena, res);
	decl->insts.push(arena, res);
	return res;
}

const StructInst* instantiateStructInst(Arena& arena, const StructInst* structInst, const Arr<const TypeParam> typeParams, const Arr<const Type> typeArgs) {
	// TODO:PERF don't create the array if we don't need it (`instantiate` could take the callback)
	const Arr<const Type> itsTypeArgs = map<const Type>{}(arena, structInst->typeArgs, [&](const Type t) {
		return instantiateType(arena, t, typeParams, typeArgs);
	});
	return instantiateStructNeverDelay(arena, structInst->decl, itsTypeArgs);
}

const Opt<const StructInst*> instStructFromAst(
	CheckCtx& ctx,
	const TypeAst::InstStruct ast,
	const StructsAndAliasesMap& structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts) {
	const Opt<const StructOrAlias> opDecl = tryFindT(
		ctx,
		ast.name,
		ast.range,
		structsAndAliasesMap,
		Diag::NameNotFound::Kind::strukt,
		[](const Module* m) { return m->structsAndAliasesMap; });
	if (!opDecl.has())
		return none<const StructInst*>();

	const StructOrAlias sOrA = opDecl.force();
	const size_t nExpectedTypeArgs = sOrA.typeParams().size;
	const Arr<const Type> typeArgs = [&]() {
		const size_t nActualTypeArgs = ast.typeArgs.size;
		if (nActualTypeArgs != nExpectedTypeArgs) {
			ctx.diag(ast.range, Diag{Diag::WrongNumberTypeArgsForStruct{sOrA, nExpectedTypeArgs, nActualTypeArgs}});
			return fillArr<const Type>{}(ctx.arena, nExpectedTypeArgs, [](__attribute__((unused)) const size_t _) { return Type::bogus(); });
		} else
			return typeArgsFromAsts(ctx, ast.typeArgs, structsAndAliasesMap, typeParamsScope, delayStructInsts);
	}();

	const StructInst* inst = sOrA.match(
		[&](const StructAlias* a) {
			if (nExpectedTypeArgs != 0)
				return todo<const StructInst*>("alias with type params");
			else
				return a->target();
		},
		[&](const StructDecl* decl) {
			return instantiateStruct(ctx.arena, decl, typeArgs, delayStructInsts);
		});
	return some<const StructInst*>(inst);
}

const Type typeFromAst(
	CheckCtx& ctx,
	const TypeAst ast,
	const StructsAndAliasesMap& structsAndAliasesMap,
	const TypeParamsScope& typeParamsScope,
	DelayStructInsts delayStructInsts
) {
	return ast.match(
		[&](const TypeAst::TypeParam p) {
			const Opt<const TypeParam*> found = findInEither(
				typeParamsScope.outer,
				typeParamsScope.innerTypeParams,
				[&](const TypeParam it) { return strEq(it.name, p.name); });
			if (found.has())
				return Type{found.force()};
			else {
				ctx.diag(p.range, Diag{Diag::NameNotFound{ctx.copyStr(p.name), Diag::NameNotFound::Kind::typeParam}});
				return Type::bogus();
			}
		},
		[&](const TypeAst::InstStruct iAst) {
			const Opt<const StructInst*> i = instStructFromAst(ctx, iAst, structsAndAliasesMap, typeParamsScope, delayStructInsts);
			return i.has() ? Type{i.force()} : Type::bogus();
		});
}

const Opt<const SpecDecl*> tryFindSpec(
	CheckCtx& ctx,
	const Str name,
	const SourceRange range,
	const SpecsMap specsMap) {
	return tryFindT(ctx, name, range, specsMap, Diag::NameNotFound::Kind::spec, [](const Module* m) {
		return m->specsMap;
	});
}

const Arr<const Type> typeArgsFromAsts(
	CheckCtx& ctx,
	const Arr<const TypeAst> typeAsts,
	const StructsAndAliasesMap structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts
) {
	return map<const Type>{}(ctx.arena, typeAsts, [&](const TypeAst it) { return typeFromAst(ctx, it, structsAndAliasesMap, typeParamsScope, delayStructInsts); });
}

bool typeIsPossiblySendable(const Type type) {
	return type.match(
		/*bogus*/ []() {
			return true;
		},
		[](__attribute__((unused)) const TypeParam* _) {
			// type param *might* have a sendable type arg. Issue errors when instantiating a generic iface, not declaring it.
			return true;
		},
		[](const StructInst* i) {
			return i->decl->purity != Purity::nonSendable &&
				every(i->typeArgs, typeIsPossiblySendable);
		});
}

const Type makeFutType(Arena& arena, const CommonTypes& commonTypes, const Type type) {
	return Type{instantiateStructNeverDelay(arena, commonTypes.fut, arrLiteral<const Type>(arena, type))};
}
