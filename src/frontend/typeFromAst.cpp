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

	template <typename TDecl>
	struct DeclAndModule {
		const TDecl decl;
		// none for this module (which isn't created yet)
		const Opt<const Module*> module;
	};

	template<typename TDecl, typename GetTMap>
	const Opt<TDecl> tryFindT(
		CheckCtx* ctx,
		const Sym name,
		const SourceRange range,
		const Dict<const Sym, TDecl, compareSym> dict,
		const Diag::DuplicateImports::Kind duplicateImportKind,
		Diag::NameNotFound::Kind nameNotFoundKind,
		GetTMap getTMap
	) {
		using DAndM = const DeclAndModule<const TDecl>;
		const Opt<TDecl> here = getAt<const Sym, TDecl, compareSym>(dict, name);
		Cell<const Opt<const DAndM>> res = Cell<const Opt<const DAndM>>{
			has(here) ? some<const DAndM>(DAndM{force(here), none<const Module*>()}) : none<const DAndM>()};

		for (const Module* m : ctx->allFlattenedImports) {
			const Opt<TDecl> fromModule = getAt<const Sym, TDecl, compareSym>(getTMap(m), name);
			if (has(fromModule)) {
				if (has(cellGet(&res))) {
					const DAndM already = force(cellGet(&res));
					//TODO: include both modules in the diag
					addDiag(ctx, range, Diag{Diag::DuplicateImports{duplicateImportKind, name}});
					return none<TDecl>();
				} else
					cellSet<const Opt<const DAndM>>(
						&res,
						some<const DAndM>(DAndM{force(fromModule), some<const Module*>(m)}));
			}
		}

		const Opt<const DAndM> r = cellGet(&res);
		if (has(r)) {
			return some<TDecl>(force(r).decl);
		} else {
			addDiag(ctx, range, Diag{Diag::NameNotFound{nameNotFoundKind, name}});
			return none<TDecl>();
		}
	}
}

const Opt<const StructInst*> instStructFromAst(
	CheckCtx* ctx,
	const TypeAst::InstStruct ast,
	const StructsAndAliasesMap structsAndAliasesMap,
	const TypeParamsScope typeParamsScope,
	DelayStructInsts delayStructInsts) {
	const Opt<const StructOrAlias> opDecl = tryFindT<const StructOrAlias>(
		ctx,
		ast.name,
		ast.range,
		structsAndAliasesMap,
		Diag::DuplicateImports::Kind::structOrAlias,
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

	return sOrA.match(
		[&](const StructAlias* a) {
			if (nExpectedTypeArgs != 0)
				return todo<const Opt<const StructInst*>>("alias with type params");
			else
				return a->target();
		},
		[&](const StructDecl* decl) {
			return some<const StructInst*>(instantiateStruct(ctx->arena, decl, typeArgs, delayStructInsts));
		});
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
				addDiag(ctx, p.range, Diag{Diag::NameNotFound{Diag::NameNotFound::Kind::typeParam, p.name}});
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
	return tryFindT<const SpecDecl*>(
		ctx,
		name,
		range,
		specsMap,
		Diag::DuplicateImports::Kind::spec,
		Diag::NameNotFound::Kind::spec,
		[](const Module* m) {
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

const Type makeFutType(Arena* arena, const CommonTypes* commonTypes, const Type type) {
	return Type{instantiateStructNeverDelay(arena, commonTypes->fut, arrLiteral<const Type>(arena, { type }))};
}
