#include "./instantiate.h"

const Type instantiateType(Arena& arena, const Type type, const TypeParamsAndArgs typeParamsAndArgs) {
	return type.match(
		[](const Type::Bogus) {
			return Type{Type::Bogus{}};
		},
		[&](const TypeParam* p) {
			const Opt<const Type> op = tryGetTypeArg(typeParamsAndArgs, p);
			return op.has() ? op.force() : type;
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

	const Sig instantiateSig(Arena& arena, const Sig sig, const TypeParamsAndArgs typeParamsAndArgs) {
		const Type returnType = instantiateType(arena, sig.returnType, typeParamsAndArgs);
		const Arr<const Param> params = map<const Param>{}(arena, sig.params, [&](const Param p) {
			return p.withType(instantiateType(arena, p.type, typeParamsAndArgs));
		});
		return Sig{sig.range, sig.name, returnType, params};
	}
}

const FunInst* instantiateFun(Arena& arena, const FunDecl* decl, const Arr<const Type> typeArgs, const Arr<const Called> specImpls) {
	for (const FunInst* fi : tempAsArr(decl->insts))
		if (eachCorresponds(fi->typeArgs, typeArgs, typeEquals) && eachCorresponds(fi->specImpls, specImpls, calledEquals))
			return fi;

	const FunInst* res = arena.nu<const FunInst>()(decl, typeArgs, specImpls, instantiateSig(arena, decl->sig, TypeParamsAndArgs{decl->typeParams, typeArgs}));
	push<const FunInst*>(arena, decl->insts, res);
	return res;
}

const StructBody instantiateStructBody(Arena& arena, const StructDecl* decl, const Arr<const Type> typeArgs) {
	const TypeParamsAndArgs typeParamsAndArgs = TypeParamsAndArgs{decl->typeParams, typeArgs};
	return decl->body().match(
		[](const StructBody::Bogus) {
			return StructBody{StructBody::Bogus{}};
		},
		[](const StructBody::Builtin) {
			return StructBody{StructBody::Builtin{}};
		},
		[&](const StructBody::Record r) {
			const Arr<const StructField> fields = map<const StructField>{}(arena, r.fields, [&](const StructField f) {
				return f.withType(instantiateType(arena, f.type, typeParamsAndArgs));
			});
			return StructBody{StructBody::Record{r.forcedByValOrRef, fields}};
		},
		[&](const StructBody::Union u) {
			const Arr<const StructInst*> members = map<const StructInst*>{}(arena, u.members, [&](const StructInst* i) {
				return instantiateStructInst(arena, i, typeParamsAndArgs);
			});
			return StructBody{StructBody::Union{members}};
		},
		[&](const StructBody::Iface i) {
			const Arr<const Message> messages = map<const Message>{}(arena, i.messages, [&](const Message m) {
				const Arr<const Param> params = map<const Param>{}(arena, m.sig.params, [&](const Param p) {
					return Param{p.range, p.name, instantiateType(arena, p.type, typeParamsAndArgs), p.index};
				});
				const Sig sig = Sig{m.sig.range, m.sig.name, instantiateType(arena, m.sig.returnType, typeParamsAndArgs), params};
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
	for (const StructInst* si : tempAsArr(decl->insts))
		if (eachCorresponds(si->typeArgs, typeArgs, typeEquals))
			return si;

	const Purity purity = [&]() {
		Purity pur = decl->purity;
		for (const Type typeArg : typeArgs)
			pur = worsePurity(pur, typeArg.purity());
		return pur;
	}();

	StructInst* res = arena.nu<StructInst>()(decl, typeArgs, purity);
	push<const StructInst*>(arena, decl->insts, res);

	if (decl->bodyIsSet())
		res->setBody(instantiateStructBody(arena, decl, typeArgs));
	else
		// We should only need to do this in the initial phase of settings struct bodies, which is when delayedStructInst is set.
		push(arena, *delayStructInsts.force(), res);
	return res;
}

const StructInst* instantiateStructInst(Arena& arena, const StructInst* structInst, const TypeParamsAndArgs typeParamsAndArgs) {
	// TODO:PERF don't create the array if we don't need it (`instantiate` could take the callback)
	const Arr<const Type> itsTypeArgs = map<const Type>{}(arena, structInst->typeArgs, [&](const Type t) {
		return instantiateType(arena, t, typeParamsAndArgs);
	});
	return instantiateStructNeverDelay(arena, structInst->decl, itsTypeArgs);
}

const SpecInst* instantiateSpec(Arena& arena, const SpecDecl* decl, const Arr<const Type> typeArgs) {
	for (const SpecInst* si : tempAsArr(decl->insts))
		if (eachCorresponds(si->typeArgs, typeArgs, typeEquals))
			return si;

	const Arr<const Sig> sigs = map<const Sig>{}(arena, decl->sigs, [&](const Sig sig) {
		return instantiateSig(arena, sig, TypeParamsAndArgs{decl->typeParams, typeArgs});
	});
	const SpecInst* res = arena.nu<const SpecInst>()(decl, typeArgs, sigs);
	push<const SpecInst*>(arena, decl->insts, res);
	return res;
}

const SpecInst* instantiateSpecInst(Arena& arena, const SpecInst* specInst, const TypeParamsAndArgs typeParamsAndArgs) {
	// TODO:PERF don't create the array if we don't need it (`instantiate` could take the callback)
	const Arr<const Type> itsTypeArgs = map<const Type>{}(arena, specInst->typeArgs, [&](const Type t) {
		return instantiateType(arena, t, typeParamsAndArgs);
	});
	return instantiateSpec(arena, specInst->decl, itsTypeArgs);
}
