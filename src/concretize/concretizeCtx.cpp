#include "./concretizeCtx.h"

#include "./builtinInfo.h"
#include "./concretizeUtil.h"
#include "./mangleName.h"

namespace {
	bool concreteTypeArrEq(const Arr<const ConcreteType> a, const Arr<const ConcreteType> b) {
		return arrEq<const ConcreteType, concreteTypeEq>(a, b);
	}

	bool funDeclAndTypeArgsEq(const FunDeclAndTypeArgs a, const FunDeclAndTypeArgs b) {
		return ptrEquals(a.decl, b.decl) && concreteTypeArrEq(a.typeArgs, b.typeArgs);
	}

	bool funDeclAndTypeArgsAndSpecImplsEq(const FunDeclAndTypeArgsAndSpecImpls a, const FunDeclAndTypeArgsAndSpecImpls b) {
		return funDeclAndTypeArgsEq(a.funDeclAndTypeArgs, b.funDeclAndTypeArgs) &&
			arrEq<const FunDecl*, ptrEquals<const FunDecl>>(a.specImpls, b.specImpls);
	}

	const Str getConcreteStructMangledName(Arena& arena, const Str declName, const Arr<const ConcreteType> typeArgs) {
		Writer writer { arena };
		writeMangledName(writer, declName);
		for (const ConcreteType ta : typeArgs)
			writeConcreteTypeForMangle(writer, ta);
		return writer.finish();
	}

	void writeSpecializeOnArgsForMangle(Writer& writer, const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs) {
		for (size_t i = 0 ; i < specializeOnArgs.size; i++)
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
		for (size_t i = 0; i < params.size; i++) {
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

	const Arr<const ConcreteParam> concretizeParamsNoSpecialize(ConcretizeCtx& ctx, const Arr<const Param> params, const TypeArgsScope typeArgsScope) {
		return concretizeParamsAndSpecialize(ctx, params, allVariable(ctx.arena, params.size), typeArgsScope);
	}

	bool isNonSpecializableBuiltin(const FunDecl* f) {
		return f->body().isBuiltin() && getBuiltinFunInfo(f->sig).isNonSpecializable;
	}

	const ConcreteType getConcreteType_forStructInst(ConcretizeCtx& ctx, const StructInst* i, const TypeArgsScope typeArgsScope);

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

	const ConcreteType getConcreteType_forStructInst(ConcretizeCtx& ctx, const StructInst* i, const TypeArgsScope typeArgsScope) {
		const Arr<const ConcreteType> typeArgs = typesToConcreteTypes(ctx, i->typeArgs, typeArgsScope);
		if (ptrEquals(i->decl, ctx.commonTypes.byVal))
			return getConcreteType(ctx, only(i->typeArgs), typeArgsScope).byVal();
		else {
			const ConcreteStructKey key = ConcreteStructKey{i->decl, typeArgs};
			bool didAdd = false;
			// Note: we can't do anything in this callback that would call getOrAddConcreteStruct again.
			ConcreteStruct* res = ctx.allConcreteStructs.getOrAdd(ctx.arena, key, [&]() {
				didAdd = true;
				return ctx.arena.nu<ConcreteStruct>()(
					getConcreteStructMangledName(ctx.arena, i->decl->name, key.typeArgs),
					getSpecialStructKind(i, ctx.commonTypes));
			});
			if (didAdd)
				initializeConcreteStruct(ctx, typeArgs, i, res, typeArgsScope);
			return ConcreteType::fromStruct(res);
		}
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
		ConcreteFun* res = ctx.arena.nu<ConcreteFun>()(!decl->noCtx(), none<const ConcreteType>(), sig, isCallFun(ctx, decl));

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

	ConcreteFun* getOrAddConcreteFunWithoutFillingBody(ConcretizeCtx& ctx, const ConcreteFunKey key) {
		return ctx.allConcreteFuns.getOrAdd(ctx.arena, key, [&]() {
			return getConcreteFunFromKey(ctx, key);
		});
	}

	//TODO: only used for side effect
	const ConcreteFunBody fillInConcreteFunBody(ConcretizeCtx& ctx, ConcreteFun* cf) {
		unused(ctx, cf);
		return todo<const ConcreteFunBody>("SSS");
	}
}

bool concreteStructKeyEq(const ConcreteStructKey a, const ConcreteStructKey b) {
	return ptrEquals(a.decl, b.decl) && concreteTypeArrEq(a.typeArgs, b.typeArgs);
}

bool concreteFunKeyEq(const ConcreteFunKey a, const ConcreteFunKey b) {
	return funDeclAndTypeArgsAndSpecImplsEq(a.declAndTypeArgsAndSpecImpls, b.declAndTypeArgsAndSpecImpls)
		&& arrEq<const ConstantOrLambdaOrVariable, constantOrLambdaOrVariableEq>(a.specializeOnArgs, b.specializeOnArgs);
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

// TODO: 't' may contain type params, must pass in current context
const ConcreteType getConcreteType(ConcretizeCtx& ctx, const Type t, const TypeArgsScope typeArgsScope) {
	return t.match(
		/*bogus*/ []() {
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

void writeConcreteTypeForMangle(Writer& writer, const ConcreteType t) {
	writer.writeStatic("__");
	if (t.isPointer)
		writer.writeStatic("ptr_");
	writer.writeStr(t.strukt->mangledName);
}

bool isCallFun(ConcretizeCtx& ctx, const FunDecl* decl) {
	return exists(ctx.callFuns, [&](const FunDecl* d) {
		return ptrEquals(d, decl);
	});
}
