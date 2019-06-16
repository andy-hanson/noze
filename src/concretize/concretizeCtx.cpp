#include "./concretizeCtx.h"

#include "../util/arrUtil.h"
#include "./builtinInfo.h"
#include "./concretizeBuiltin.h" // getBuiltinFunBody
#include "./concretizeExpr.h"
#include "./concretizeUtil.h"
#include "./mangleName.h"

namespace {
	Comparison compareConcreteTypeArr(const Arr<const ConcreteType> a, const Arr<const ConcreteType> b) {
		return compareArr<const ConcreteType, compareConcreteType>(a, b);
	}

	Comparison compareFunDeclAndTypeArgs(const FunDeclAndTypeArgs a, const FunDeclAndTypeArgs b) {
		const Comparison res = comparePointer(a.decl, b.decl);
		return res != Comparison::equal ? res : compareConcreteTypeArr(a.typeArgs, b.typeArgs);
	}

	Comparison compareFunDeclAndTypeArgsAndSpecImpls(const FunDeclAndTypeArgsAndSpecImpls a, const FunDeclAndTypeArgsAndSpecImpls b) {
		const Comparison res = compareFunDeclAndTypeArgs(a.funDeclAndTypeArgs, b.funDeclAndTypeArgs);
		return res != Comparison::equal ? res : compareArr<const FunDecl*, comparePointer<const FunDecl>>(a.specImpls, b.specImpls);
	}

	const Str getConcreteStructMangledName(Arena& arena, const Str declName, const Arr<const ConcreteType> typeArgs) {
		Writer writer { arena };
		writeMangledName(writer, declName);
		for (const ConcreteType ta : typeArgs)
			writeConcreteTypeForMangle(writer, ta);
		return writer.finish();
	}

	void writeSpecializeOnArgsForMangle(Writer& writer, const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs) {
		for (const size_t i : Range{specializeOnArgs.size})
			specializeOnArgs[i].match(
				[](const ConstantOrLambdaOrVariable::Variable) {},
				[&](const Constant* c) {
					writer.writeStatic("_arg");
					writer.writeUint(i);
					writer.writeStatic("_is_");
					writer.writeUint(c->id);
				},
				[&](const KnownLambdaBody* klb) {
					writer.writeStatic("_klb");
					writer.writeUint(i);
					writer.writeStatic("_is_");
					writer.writeStr(klb->mangledName);
				});
	}

	// Don't need to take typeArgs here, since we have the concrete return type and param types anyway.
	const Str getConcreteFunMangledName(
		Arena& arena,
		const Str declName,
		const ConcreteType returnType,
		const Arr<const ConcreteParam> params,
		const Arr<const FunDecl*> specImpls,
		const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs
	) {
		Writer writer { arena };
		writeMangledName(writer, declName);
		writeConcreteTypeForMangle(writer, returnType);
		for (const ConcreteParam param : params)
			writeConcreteTypeForMangle(writer, param.type);
		for (const FunDecl* si : specImpls) {
			writer.writeStatic("__");
			writeMangledName(writer, si->name());
		}
		writeSpecializeOnArgsForMangle(writer, specializeOnArgs);
		return writer.finish();
	}

	const Opt<const SpecialStructKind> getSpecialStructKind(const StructInst* i, const CommonTypes& commonTypes) {
		const StructDecl* decl = i->decl;
		if (ptrEquals(decl, commonTypes.arr))
			return some<const SpecialStructKind>(SpecialStructKind::arr);
		else if (ptrEquals(decl, commonTypes.mutArr))
			return some<const SpecialStructKind>(SpecialStructKind::mutArr);
		else
			return none<const SpecialStructKind>();
	}

	size_t sizeFromConcreteStructBody(const ConcreteStructBody body) {
		return body.match(
			[](const ConcreteStructBody::Builtin b) {
				return b.info.sizeBytes;
			},
			[](const ConcreteStructBody::Fields f) {
				// TODO: this is probably not 100% accurate. Luckily I use static asserts in the generated code to check this.
				size_t s = 0;
				for (const ConcreteField field : f.fields)
					s += field.type.sizeOrPointerSize();
				return max(s, 1);
			},
			[](const ConcreteStructBody::Union u) {
				size_t maxMember = 0;
				for (const ConcreteType ct : u.members)
					maxMember = max(maxMember, ct.sizeOrPointerSize());
				// Must factor in the 'kind' size. It seems that enums are int-sized.
				return roundUp(sizeof(int) + maxMember, sizeof(void*));
			},
			[](const ConcreteStructBody::Iface) {
				// Ifaces are all the same size
				return sizeof(void*) * 2;
			});
	}

	template <typename ForVariable>
	const Opt<const ConcreteType> getSpecializedParamType(const ConstantOrLambdaOrVariable clv, ForVariable forVariable) {
		return clv.match(
			[&](const ConstantOrLambdaOrVariable::Variable) {
				return some<const ConcreteType>(forVariable());
			},
			[&](const Constant*) {
				return none<const ConcreteType>();
			},
			[&](const KnownLambdaBody* klb) {
				return klb->closureType;
			});
	}

	const Arr<const ConcreteParam> concretizeParamsAndSpecialize(
		ConcretizeCtx& ctx,
		const Arr<const Param> params,
		const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs,
		const TypeArgsScope typeArgsScope
	) {
		assert(params.size == specializeOnArgs.size);
		// TODO: mapOpZip helper
		ArrBuilder<const ConcreteParam> res {};
		for (const size_t i : Range{params.size}) {
			const Param p = params[i];
			const Opt<const ConcreteType> t = getSpecializedParamType(specializeOnArgs[i], [&]() {
				return getConcreteType(ctx, p.type, typeArgsScope);
			});
			if (t.has())
				// TODO: probably need to mangle the param name
				res.add(ctx.arena, ConcreteParam{copyStr(ctx.arena, p.name), t.force()});
		}
		return res.finish();
	}

	const Bool isNonSpecializableBuiltin(const FunDecl* f) {
		return _and(f->body().isBuiltin(), getBuiltinFunInfo(f->sig).isNonSpecializable);
	}

	void initializeConcreteStruct(
		ConcretizeCtx& ctx,
		const Arr<const ConcreteType> typeArgs,
		const StructInst* i,
		ConcreteStruct* res,
		const TypeArgsScope typeArgsScope
	) {
		// Note: i.body has the struct's own type parameters replaced with its type arguments, unlike decl.bod
		res->setBody(i->body().match(
			[&](const StructBody::Builtin) {
				return ConcreteStructBody{ConcreteStructBody::Builtin{getBuiltinStructInfo(i->decl), typeArgs}};
			},
			[&](const StructBody::Fields f) {
				const Arr<const ConcreteField> fields = map<const ConcreteField>{}(ctx.arena, f.fields, [&](const StructField f) {
					return ConcreteField{mangleName(ctx.arena, f.name), getConcreteType(ctx, f.type, typeArgsScope)};
				});
				return ConcreteStructBody{ConcreteStructBody::Fields{fields}};
			},
			[&](const StructBody::Union u) {
				const Arr<const ConcreteType> members = map<const ConcreteType>{}(ctx.arena, u.members, [&](const StructInst* si) {
					return getConcreteType_forStructInst(ctx, si, typeArgsScope);
				});
				return ConcreteStructBody{ConcreteStructBody::Union{members}};
			},
			[&](const StructBody::Iface i) {
				const Arr<const ConcreteSig> messages = map<const ConcreteSig>{}(ctx.arena, i.messages, [&](const Message msg) {
					return ConcreteSig{
						mangleName(ctx.arena, msg.sig.name),
						getConcreteType(ctx, msg.sig.returnType, typeArgsScope),
						concretizeParamsNoSpecialize(ctx, msg.sig.params, typeArgsScope)};
				});
				return ConcreteStructBody{ConcreteStructBody::Iface{messages}};
			}));

		// Set this high so it will be referenced by pointer, so we don't recurse infinitely.
		res->_sizeBytes.set(99999);
		res->_sizeBytes.setOverwrite(sizeFromConcreteStructBody(res->body()));
	}

	// This is for concretefun from FunDecl, from lambda is below
	ConcreteFun* getConcreteFunFromKey(ConcretizeCtx& ctx, const ConcreteFunKey key) {
		const FunDecl* decl = key.decl();
		const TypeArgsScope typeScope = key.typeArgsScope();
		const ConcreteType returnType = getConcreteType(ctx, decl->returnType(), typeScope);
		const Arr<const ConcreteParam> params = concretizeParamsAndSpecialize(ctx, decl->params(), key.specializeOnArgs, typeScope);
		const Str mangledName = decl->isExtern()
			? decl->name()
			: getConcreteFunMangledName(ctx.arena, decl->name(), returnType, params, key.specImpls(), key.specializeOnArgs);
		const ConcreteSig sig = ConcreteSig{mangledName, returnType, params};
		// no closure for fun from decl
		ConcreteFun* res = ctx.arena.nu<ConcreteFun>()(_not(decl->noCtx()), none<const ConcreteType>(), sig, isCallFun(ctx, decl));

		if (isNonSpecializableBuiltin(decl))
			for (const ConstantOrLambdaOrVariable clv : key.specializeOnArgs)
				assert(clv.isVariable());

		const ConcreteFunSource source =  ConcreteFunSource{
			key.declAndTypeArgsAndSpecImpls,
			key.specializeOnArgs,
			decl->body(),
			none<const Arr<const ConstantOrLambdaOrVariable>>()};
		ctx.concreteFunToSource.add(ctx.arena, res, source);
		return res;
	}

	//TODO: only used for side effect
	const ConcreteFunBody fillInConcreteFunBody(ConcretizeCtx& ctx, ConcreteFun* cf) {
		// TODO: just assert it's not already set?
		if (!cf->_body.isSet()) {
			cf->_body.set(ConcreteFunBody{
				ctx.arena.nu<ConcreteExpr>()(SourceRange::empty(), none<const KnownLambdaBody*>(), ConcreteExpr::Bogus{})});
			const ConcreteFunSource source = ctx.concreteFunToSource.tryDeleteAndGet(cf).force();
			const ConcreteFunBody body = source.body.match(
				[&](const FunBody::Builtin) {
					return getBuiltinFunBody(ctx, source, cf);
				},
				[&](const FunBody::Extern) {
					return ConcreteFunBody{ConcreteFunBody::Extern{}};
				},
				[&](const Expr* e) {
					return toConcreteFunBody(doConcretizeExpr(ctx, source, cf, *e));
				});
			cf->_body.setOverwrite(body);
			return body;
		} else
			return cf->body();
	}

	const Str getLambdaInstanceMangledName(
		Arena& arena,
		const Str knownLambdaBodyMangledName,
		const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs,
		const Bool isForDynamic
	) {
		Writer writer { arena };
		writer.writeStr(knownLambdaBodyMangledName);
		writeSpecializeOnArgsForMangle(writer, specializeOnArgs);
		if (isForDynamic)
			writer.writeStatic("__dynamic");
		return writer.finish();
	}

	// This is for instantiating a KnownLambdaBody.
	const Arr<const ConcreteParam> specializeParamsForLambdaInstance(
		ConcretizeCtx& ctx,
		const Arr<const ConcreteParam> nonSpecializedParams,
		const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs
	) {
		ArrBuilder<const ConcreteParam> res {};
		//TODO: 'zip' helper
		for (const size_t i : Range{nonSpecializedParams.size}) {
			const ConcreteParam p = nonSpecializedParams[i];
			const Opt<const ConcreteType> t = getSpecializedParamType(specializeOnArgs[i], [&]() { return p.type; });
			if (t.has())
				res.add(ctx.arena, ConcreteParam{p.mangledName, t.force()});
		}
		return res.finish();
	}

	// Get a ConcreteFun for a particular instance of a KnownLambdaBody -- used by instantiateKnownLambdaBody
	ConcreteFun* getConcreteFunFromKnownLambdaBodyAndFill(
		ConcretizeCtx& ctx,
		const KnownLambdaBody* klb,
		const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs,
		// If True, this is used by a dynamic call.
		// If False, this is called directly.
		const Bool isForDynamic
	) {
		const Str mangledName = getLambdaInstanceMangledName(ctx.arena, klb->mangledName, specializeOnArgs, isForDynamic);
		const LambdaInfo info = ctx.knownLambdaBodyToInfo.mustGet(klb);
		const ConcreteType returnType = klb->nonSpecializedSig.returnType; // specialization never changes the return type
		const Arr<const ConcreteParam> params = specializeParamsForLambdaInstance(ctx, klb->nonSpecializedSig.params, specializeOnArgs);
		const ConcreteSig sig = ConcreteSig{mangledName, returnType, params};
		const Opt<const ConcreteType> closure = isForDynamic && !klb->hasClosure()
			? some<const ConcreteType>(ctx.voidPtrType())
			: klb->closureType;
		ConcreteFun* res = ctx.arena.nu<ConcreteFun>()(
			/*needsCtx*/ True, // TODO:PERF for some non-dynamic calls it does not need ctx
			closure,
			sig,
			/*isCallFun*/ False);

		const ConcreteFunSource source = ConcreteFunSource{
			info.containingFunDeclAndTypeArgsAndSpecImpls,
			specializeOnArgs,
			FunBody{info.body},
			some<const Arr<const ConstantOrLambdaOrVariable>>(klb->closureSpecialize)};
		ctx.concreteFunToSource.add(ctx.arena, res, source);

		fillInConcreteFunBody(ctx, res);

		return res;
	}

	ConcreteFun* getOrAddConcreteFunWithoutFillingBody(ConcretizeCtx& ctx, const ConcreteFunKey key) {
		return ctx.allConcreteFuns.getOrAdd(ctx.arena, key, [&]() {
			return getConcreteFunFromKey(ctx, key);
		});
	}

}

Comparison compareConcreteStructKey(const ConcreteStructKey a, const ConcreteStructKey b) {
	const Comparison res = comparePointer(a.decl, b.decl);
	return res != Comparison::equal ? res : compareConcreteTypeArr(a.typeArgs, b.typeArgs);
}

Comparison compareConcreteFunKey(const ConcreteFunKey a, const ConcreteFunKey b) {
	const Comparison res = compareFunDeclAndTypeArgsAndSpecImpls(a.declAndTypeArgsAndSpecImpls, b.declAndTypeArgsAndSpecImpls);
	return res != Comparison::equal
		? res
		: compareArr<const ConstantOrLambdaOrVariable, compareConstantOrLambdaOrVariable>(a.specializeOnArgs, b.specializeOnArgs);
}

const ConcreteType ConcretizeCtx::voidPtrType() {
	return getConcreteType_forStructInst(*this, commonTypes._void, TypeArgsScope::empty()).byRef();
}

const ConcreteFun* getOrAddNonGenericConcreteFunAndFillBody(ConcretizeCtx& ctx, const FunDecl* decl) {
	const ConcreteFunKey key = ConcreteFunKey{
		FunDeclAndTypeArgsAndSpecImpls{
			FunDeclAndTypeArgs{
				decl,
				emptyArr<const ConcreteType>()},
			emptyArr<const FunDecl*>()},
		allVariable(ctx.arena, decl->arity())};
	return getOrAddConcreteFunAndFillBody(ctx, key);
}

const ConcreteFun* getOrAddConcreteFunAndFillBody(ConcretizeCtx& ctx, const ConcreteFunKey key) {
	ConcreteFun* cf = getOrAddConcreteFunWithoutFillingBody(ctx, key);
	fillInConcreteFunBody(ctx, cf);
	return cf;
}

const ConcreteFun* instantiateKnownLambdaBodyForDirectCall(ConcretizeCtx& ctx, const KnownLambdaBody* klb, const Arr<const ConstantOrLambdaOrVariable> args) {
	return klb->directCallInstances.getOrAdd(ctx.arena, args, [&]() {
		return getConcreteFunFromKnownLambdaBodyAndFill(ctx, klb, args, /*isForDynamic*/ False);
	});
}

const ConcreteFun* instantiateKnownLambdaBodyForDynamic(ConcretizeCtx& ctx, const KnownLambdaBody* klb) {
	// When a lambda will be used dynamically:
	// - Do no specialization
	// - Add a dummy closure type, even if it won't be used.
	return lazilySet(klb->dynamicInstance, [&]() {
		return getConcreteFunFromKnownLambdaBodyAndFill(ctx, klb, allVariable(ctx.arena, klb->nonSpecializedSig.params.size), /*isForDynamic*/ True);
	});
}

const ConcreteType getConcreteType_forStructInst(ConcretizeCtx& ctx, const StructInst* i, const TypeArgsScope typeArgsScope) {
	const Arr<const ConcreteType> typeArgs = typesToConcreteTypes(ctx, i->typeArgs, typeArgsScope);
	if (ptrEquals(i->decl, ctx.commonTypes.byVal))
		return getConcreteType(ctx, only(i->typeArgs), typeArgsScope).byVal();
	else {
		const ConcreteStructKey key = ConcreteStructKey{i->decl, typeArgs};
		Cell<const Bool> didAdd { False };
		// Note: we can't do anything in this callback that would call getOrAddConcreteStruct again.
		ConcreteStruct* res = ctx.allConcreteStructs.getOrAdd(ctx.arena, key, [&]() {
			didAdd.set(True);
			return ctx.arena.nu<ConcreteStruct>()(
				getConcreteStructMangledName(ctx.arena, i->decl->name, key.typeArgs),
				getSpecialStructKind(i, ctx.commonTypes));
		});
		if (didAdd.get())
			initializeConcreteStruct(ctx, typeArgs, i, res, typeArgsScope);
		return ConcreteType::fromStruct(res);
	}
}

// TODO: 't' may contain type params, must pass in current context
const ConcreteType getConcreteType(ConcretizeCtx& ctx, const Type t, const TypeArgsScope typeArgsScope) {
	return t.match(
		[](const Type::Bogus) {
			return unreachable<const ConcreteType>();
		},
		[&](const TypeParam* p) {
			// Handle calledConcreteFun first
			assert(ptrEquals(p, typeArgsScope.typeParams.getPtr(p->index)));
			return typeArgsScope.typeArgs[p->index];
		},
		[&](const StructInst* i) {
			return getConcreteType_forStructInst(ctx, i, typeArgsScope);
		});
}

const Arr<const ConcreteType> typesToConcreteTypes(ConcretizeCtx& ctx, const Arr<const Type> types, const TypeArgsScope typeArgsScope) {
	return map<const ConcreteType>{}(ctx.arena, types, [&](const Type t) {
		return getConcreteType(ctx, t, typeArgsScope);
	});
}

const Bool isCallFun(ConcretizeCtx& ctx, const FunDecl* decl) {
	return exists(ctx.callFuns, [&](const FunDecl* d) {
		return ptrEquals(d, decl);
	});
}

namespace {
	const SpecializeOnArgs specializeOnAsMuchAsPossible(Arena& arena, const Arr<const ConstantOrExpr> args) {
		// TODO: helper to map to two different arrs
		ArrBuilder<const ConstantOrExpr> notSpecializedArgs {};
		const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs = map<const ConstantOrLambdaOrVariable>{}(arena, args, [&](const ConstantOrExpr ce) {
			return ce.match(
				[&](const Constant* c) {
					return ConstantOrLambdaOrVariable{c};
				},
				[&](const ConcreteExpr* e) {
					notSpecializedArgs.add(arena, ConstantOrExpr{e});
					return constantOrLambdaOrVariableFromConcreteExpr(*e);
				});
		});
		return SpecializeOnArgs{specializeOnArgs, notSpecializedArgs.finish()};
	}
}

const SpecializeOnArgs getSpecializeOnArgsForLambdaClosure(Arena& arena, const Arr<const ConstantOrExpr> args) {
	return specializeOnAsMuchAsPossible(arena, args);
}

const SpecializeOnArgs getSpecializeOnArgsForLambdaCall(Arena& arena, const Arr<const ConstantOrExpr> args, const Bool isSummon) {
	Cell<const Bool> allConstant { True };
	Cell<const Bool> someFun { False };
	for (const ConstantOrExpr c : args) {
		if (getKnownLambdaBodyFromConstantOrExpr(c).has())
			someFun.set(True);
		if (!c.isConstant())
			allConstant.set(False);
	}

	// Specialize on funs with all-constant parameters that are non-'summon' as these will likely evaluate to constants.
	// Always specialize on a fun, even if fun is summon
	return (allConstant.get() && !isSummon) || someFun.get()
		? specializeOnAsMuchAsPossible(arena, args)
		: SpecializeOnArgs{allVariable(arena, args.size), args};
}

const SpecializeOnArgs getSpecializeOnArgsForFun(ConcretizeCtx& ctx, const FunDecl* f, const Arr<const ConstantOrExpr> args) {
	// Don't specialize just because a single arg is a constant.
	// If *every* arg is a constant, we always specialize on all args.
	// Else, specialize if some arg has a known lambda body. (TODO: and that lambda is *only* called.)

	// Never specialize on 'call' -- though we do treat it specially in 'concretizeCall'
	return f->isExtern() || isNonSpecializableBuiltin(f) || isCallFun(ctx, f)
		? SpecializeOnArgs{allVariable(ctx.arena, args.size), args}
		: getSpecializeOnArgsForLambdaCall(ctx.arena, args, f->isSummon());
}

const Arr<const ConcreteField> concretizeClosureFieldsAndSpecialize(
	ConcretizeCtx& ctx,
	const Arr<const ClosureField*> closure,
	const Arr<const ConstantOrLambdaOrVariable> closureSpecialize,
	const TypeArgsScope typeArgsScope
) {
	ArrBuilder<const ConcreteField> res;
	for (const size_t i : Range{closure.size}) {
		const ClosureField* c = closure[i];
		const Opt<const ConcreteType> t = getSpecializedParamType(closureSpecialize[i], [&]() {
			return getConcreteType(ctx, c->type, typeArgsScope);
		});
		if (t.has())
			res.add(ctx.arena, ConcreteField{copyStr(ctx.arena, c->name), t.force()});
	}
	return res.finish();
}

const Arr<const ConcreteParam> concretizeParamsNoSpecialize(ConcretizeCtx& ctx, const Arr<const Param> params, const TypeArgsScope typeArgsScope) {
	return concretizeParamsAndSpecialize(ctx, params, allVariable(ctx.arena, params.size), typeArgsScope);
}
