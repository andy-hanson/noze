#include "./typeFromAst.h"

#include "../util/arrUtil.h"

namespace {
	//TODO:MOVE
	template<typename T, typename Cb>
	const Opt<const T*> findInEither(const Arr<T> a, const Arr<T> b, Cb cb) {
		for (const size_t i : Range{a.size})
			if (cb(at(a, i)))
				return some<const T*>(ptrAt(a, i));
		for (const size_t i : Range{b.size})
			if (cb(at(b, i)))
				return some<const T*>(ptrAt(b, i));
		return none<const T*>();
	}

	template<typename TDecl, typename GetTMap>
	const Opt<TDecl> tryFindT(
		CheckCtx* ctx,
		const Sym name,
		const SourceRange range,
		const Dict<const Sym, TDecl, compareSym> dict,
		Diag::NameNotFound::Kind kind,
		GetTMap getTMap
	) {
		Cell<const Opt<TDecl>> res = Cell<const Opt<TDecl>>{getAt<const Sym, TDecl, compareSym>(dict, name)};

		for (const Module* m : includeAndImportsRange(ctx)) {
			const Opt<TDecl> fromModule = getAt<const Sym, TDecl, compareSym>(getTMap(m), name);
			if (has(fromModule)) {
				if (has(cellGet(&res)))
					todo<void>("Duplicate imports from different modules");
				else
					cellSet<const Opt<TDecl>>(&res, fromModule);
			}
		}

		const Opt<TDecl> r = cellGet(&res);
		if (!has(r))
			addDiag(ctx, range, Diag{Diag::NameNotFound{name, kind}});
		return r;
	}
}

const Opt<const StructInst*> instStructFromAst(
	CheckCtx* ctx,
	const TypeAst::InstStruct ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts) {
	const Opt<const StructOrAlias> opDecl = tryFindT(
		ctx,
		ast.name,
		ast.range,
		structsAndAliasesMap,
		Diag::NameNotFound::Kind::strukt,
		[](const Module* m) { return m->structsAndAliasesMap; });
	if (!has(opDecl))
		return none<const StructInst*>();

	const StructOrAlias sOrA = force(opDecl);
	const size_t nExpectedTypeArgs = size(sOrA.typeParams());
	const Arr<const Type> typeArgs = [&]() {
		const size_t nActualTypeArgs = size(ast.typeArgs);
		if (nActualTypeArgs != nExpectedTypeArgs) {
			addDiag(ctx, ast.range, Diag{Diag::WrongNumberTypeArgsForStruct{sOrA, nExpectedTypeArgs, nActualTypeArgs}});
			return fillArr<const Type>{}(ctx->arena, nExpectedTypeArgs, [](const size_t) {
				return Type{Type::Bogus{}};
			});
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
			return instantiateStruct(ctx->arena, decl, typeArgs, delayStructInsts);
		});
	return some<const StructInst*>(inst);
}

const Type typeFromAst(
	CheckCtx* ctx,
	const TypeAst ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts
) {
	return ast.match(
		[&](const TypeAst::TypeParam p) {
			const Opt<const TypeParam*> found = findPtr(typeParamsScope.innerTypeParams, [&](const TypeParam* it) {
				return symEq(it->name, p.name);
			});
			if (has(found))
				return Type{force(found)};
			else {
				addDiag(ctx, p.range, Diag{Diag::NameNotFound{p.name, Diag::NameNotFound::Kind::typeParam}});
				return Type{Type::Bogus{}};
			}
		},
		[&](const TypeAst::InstStruct iAst) {
			const Opt<const StructInst*> i =
				instStructFromAst(ctx, iAst, structsAndAliasesMap, typeParamsScope, delayStructInsts);
			return has(i) ? Type{force(i)} : Type{Type::Bogus{}};
		});
}

const Opt<const SpecDecl*> tryFindSpec(
	CheckCtx* ctx,
	const Sym name,
	const SourceRange range,
	const SpecsMap specsMap) {
	return tryFindT(ctx, name, range, specsMap, Diag::NameNotFound::Kind::spec, [](const Module* m) {
		return m->specsMap;
	});
}

const Arr<const Type> typeArgsFromAsts(
	CheckCtx* ctx,
	const Arr<const TypeAst> typeAsts,
	const StructsAndAliasesMap structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts
) {
	return map<const Type>{}(ctx->arena, typeAsts, [&](const TypeAst it) {
		return typeFromAst(ctx, it, structsAndAliasesMap, typeParamsScope, delayStructInsts);
	});
}

const Bool typeIsPossiblySendable(const Type type) {
	return type.match(
		[](const Type::Bogus) {
			return True;
		},
		[](const TypeParam*) {
			// type param *might* have a sendable type arg.
			// Issue errors when instantiating a template iface, not declaring it.
			return True;
		},
		[](const StructInst* i) {
			return _and(
				i->decl->purity != Purity::mut,
				every(i->typeArgs, typeIsPossiblySendable));
		});
}

const Type makeFutType(Arena* arena, const CommonTypes* commonTypes, const Type type) {
	return Type{instantiateStructNeverDelay(arena, commonTypes->fut, arrLiteral<const Type>(arena, { type }))};
}
