#include "./instantiate.h"

const Type instantiateType(Arena* arena, const Type type, const TypeParamsAndArgs typeParamsAndArgs) {
	return type.match(
		[](const Type::Bogus) {
			return Type{Type::Bogus{}};
		},
		[&](const TypeParam* p) {
			const Opt<const Type> op = tryGetTypeArgFromTypeParamsAndArgs(typeParamsAndArgs, p);
			return has(op) ? force(op) : type;
		},
		[&](const StructInst* i) {
			return Type{instantiateStructInst(arena, i, typeParamsAndArgs)};
		});
}

namespace {
	const Bool calledEquals(const Called a, const Called b) {
		return a.match(
			[&](const FunInst* f) {
				return _and(b.isFunInst(), ptrEquals(f, b.asFunInst()));
			},
			[&](const SpecSig s) {
				if (b.isSpecSig()) {
					const SpecSig bs = b.asSpecSig();
					if (ptrEquals(s.specInst, bs.specInst)) {
						const Bool res = ptrEquals(s.sig, bs.sig);
						assert(res == eq(s.indexOverAllSpecUses, bs.indexOverAllSpecUses));
						return res;
					} else
						return False;
				} else
					return False;
			});
	}

	const Sig instantiateSig(Arena* arena, const Sig sig, const TypeParamsAndArgs typeParamsAndArgs) {
		const Type returnType = instantiateType(arena, sig.returnType, typeParamsAndArgs);
		const Arr<const Param> params = map<const Param>{}(arena, sig.params, [&](const Param p) {
			return p.withType(instantiateType(arena, p.type, typeParamsAndArgs));
		});
		return Sig{sig.range, sig.name, returnType, params};
	}
}

const FunInst* instantiateFun(
	Arena* arena,
	const FunDecl* decl,
	const Arr<const Type> typeArgs,
	const Arr<const Called> specImpls
) {
	for (const FunInst* fi : tempAsArr(&decl->insts))
		if (eachCorresponds(fi->typeArgs, typeArgs, typeEquals)
			&& eachCorresponds(fi->specImpls, specImpls, calledEquals))
			return fi;

	const FunInst* res = nu<const FunInst>{}(
		arena,
		decl,
		typeArgs,
		specImpls,
		instantiateSig(arena, decl->sig, TypeParamsAndArgs{decl->typeParams, typeArgs}));
	push<const FunInst*>(arena, &decl->insts, res);
	return res;
}

const StructBody instantiateStructBody(Arena* arena, const StructDecl* decl, const Arr<const Type> typeArgs) {
	const TypeParamsAndArgs typeParamsAndArgs = TypeParamsAndArgs{decl->typeParams, typeArgs};
	return decl->body().match(
		[](const StructBody::Bogus) {
			return StructBody{StructBody::Bogus{}};
		},
		[](const StructBody::Builtin) {
			return StructBody{StructBody::Builtin{}};
		},
		[&](const StructBody::Record r) {
			const Arr<const RecordField> fields = map<const RecordField>{}(arena, r.fields, [&](const RecordField f) {
				return f.withType(instantiateType(arena, f.type, typeParamsAndArgs));
			});
			return StructBody{StructBody::Record{r.forcedByValOrRef, fields}};
		},
		[&](const StructBody::Union u) {
			const Arr<const StructInst*> members = map<const StructInst*>{}(arena, u.members, [&](const StructInst* i) {
				return instantiateStructInst(arena, i, typeParamsAndArgs);
			});
			return StructBody{StructBody::Union{members}};
		});
}

const StructInst* instantiateStruct(
	Arena* arena,
	const StructDecl* decl,
	const Arr<const Type> typeArgs,
	DelayStructInsts delayStructInsts
) {
	for (const StructInst* si : tempAsArr(&decl->insts))
		if (eachCorresponds(si->typeArgs, typeArgs, typeEquals))
			return si;

	const Purity bestCasePurity = fold(decl->purity, typeArgs, [&](const Purity pur, const Type typeArg) {
		return worsePurity(pur, typeArg.bestCasePurity());
	});
	const Purity worstCasePurity = fold(decl->purity, typeArgs, [&](const Purity pur, const Type typeArg) {
		return worsePurity(pur, typeArg.worstCasePurity());
	});

	StructInst* res = nu<StructInst>{}(arena, decl, typeArgs, bestCasePurity, worstCasePurity);
	push<const StructInst*>(arena, &decl->insts, res);

	if (decl->bodyIsSet())
		res->setBody(instantiateStructBody(arena, decl, typeArgs));
	else
		// We should only need to do this in the initial phase of settings struct bodies,
		// which is when delayedStructInst is set.
		push(arena, force(delayStructInsts), res);
	return res;
}

const StructInst* instantiateNonTemplateStruct(Arena* arena, const StructDecl* decl) {
	return instantiateStruct(arena, decl, emptyArr<const Type>(), none<MutArr<StructInst*>*>());
}

const StructInst* instantiateStructInst(
	Arena* arena,
	const StructInst* structInst,
	const TypeParamsAndArgs typeParamsAndArgs
) {
	// TODO:PERF don't create the array if we don't need it (`instantiate` could take the callback)
	const Arr<const Type> itsTypeArgs = map<const Type>{}(arena, structInst->typeArgs, [&](const Type t) {
		return instantiateType(arena, t, typeParamsAndArgs);
	});
	return instantiateStructNeverDelay(arena, structInst->decl, itsTypeArgs);
}

const SpecInst* instantiateSpec(Arena* arena, const SpecDecl* decl, const Arr<const Type> typeArgs) {
	for (const SpecInst* si : tempAsArr(&decl->insts))
		if (eachCorresponds(si->typeArgs, typeArgs, typeEquals))
			return si;

	const SpecBody body = decl->body.match(
		[](const SpecBody::Builtin b) {
			return SpecBody{SpecBody::Builtin{b.kind}};
		},
		[&](const Arr<const Sig> sigs) {
			return SpecBody{map<const Sig>{}(arena, sigs, [&](const Sig sig) {
				return instantiateSig(arena, sig, TypeParamsAndArgs{decl->typeParams, typeArgs});
			})};
		});

	const SpecInst* res = nu<const SpecInst>{}(arena, decl, typeArgs, body);
	push<const SpecInst*>(arena, &decl->insts, res);
	return res;
}

const SpecInst* instantiateSpecInst(Arena* arena, const SpecInst* specInst, const TypeParamsAndArgs typeParamsAndArgs) {
	// TODO:PERF don't create the array if we don't need it (`instantiate` could take the callback)
	const Arr<const Type> itsTypeArgs = map<const Type>{}(arena, specInst->typeArgs, [&](const Type t) {
		return instantiateType(arena, t, typeParamsAndArgs);
	});
	return instantiateSpec(arena, specInst->decl, itsTypeArgs);
}
