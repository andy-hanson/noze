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

	const Str getConcreteStructMangledName(Arena& arena, const Str declName, const Arr<const ConcreteType> typeArgs) {
		Writer writer { arena };
		writeMangledName(writer, declName);
		for (const ConcreteType ta : typeArgs)
			writeConcreteTypeForMangle(writer, ta);
		return finishWriter(writer);
	}

	void writeSpecializeOnArgsForMangle(Writer& writer, const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs) {
		//TODO:EACHWITHINDEX
		for (const size_t i : Range{specializeOnArgs.size})
			at(specializeOnArgs, i).match(
				[](const ConstantOrLambdaOrVariable::Variable) {},
				[&](const Constant* c) {
					writeStatic(writer, "_arg");
					writeNat(writer, i);
					writeStatic(writer, "_is_");
					writeNat(writer, c->id);
				},
				[&](const KnownLambdaBody* klb) {
					writeStatic(writer, "_klb");
					writeNat(writer, i);
					writeStatic(writer, "_is_");
					writeStr(writer, klb->mangledName);
				});
	}

	// Don't need to take typeArgs here, since we have the concrete return type and param types anyway.
	const Str getConcreteFunMangledName(
		Arena& arena,
		const Str declName,
		const ConcreteType returnType,
		const Arr<const ConcreteParam> params,
		const Arr<const ConcreteFunInst> specImpls,
		const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs
	) {
		Writer writer { arena };
		writeMangledName(writer, declName);
		writeConcreteTypeForMangle(writer, returnType);
		for (const ConcreteParam param : params)
			writeConcreteTypeForMangle(writer, param.type);
		for (const ConcreteFunInst si : specImpls) {
			//TODO: include *its* type args!
			writeStatic(writer, "__");
			writeMangledName(writer, si.decl->name());
		}
		writeSpecializeOnArgsForMangle(writer, specializeOnArgs);
		return finishWriter(writer);
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

	size_t sizeFromConcreteFields(const Arr<const ConcreteField> fields) {
		// TODO: this is definitely not accurate. Luckily I use static asserts in the generated code to check this.
		size_t s = 0;
		for (const ConcreteField field : fields)
			s += max<const size_t>(sizeof(void*), field.type.sizeOrPointerSize());
		return max<const size_t>(s, 1);
	}

	size_t sizeFromConcreteStructBody(const ConcreteStructBody body) {
		return body.match(
			[](const ConcreteStructBody::Builtin b) {
				return b.info.sizeBytes;
			},
			[](const ConcreteStructBody::Fields f) {
				return sizeFromConcreteFields(f.fields);
			},
			[](const ConcreteStructBody::Union u) {
				size_t maxMember = 0;
				for (const ConcreteType ct : u.members)
					maxMember = max<const size_t>(maxMember, ct.sizeOrPointerSize());
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
				return klb->closureType();
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
			const Param p = at(params, i);
			const Opt<const ConcreteType> t = getSpecializedParamType(at(specializeOnArgs, i), [&]() {
				return getConcreteType(ctx, p.type, typeArgsScope);
			});
			if (t.has())
				res.add(ctx.arena, ConcreteParam{mangleName(ctx.arena, p.name), t.force()});
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
					return ConcreteField{f.isMutable, mangleName(ctx.arena, f.name), getConcreteType(ctx, f.type, typeArgsScope)};
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

	const bool PRINT_INSTANTIATION = false;

	static uint INSTANTIATING_INDENT = 0;

	void printInstantiatingIndent() {
		for (uint i = 0; i < INSTANTIATING_INDENT; i++)
			printf("\t");
	}

	// This is for concretefun from FunDecl, from lambda is below
	ConcreteFun* getConcreteFunFromKey(ConcretizeCtx& ctx, const ConcreteFunKey key) {
		const FunDecl* decl = key.decl();

		if (PRINT_INSTANTIATION) {
			printInstantiatingIndent();
			Arena arena {};
			Writer writer { arena };
			writeStatic(writer, "Instantiating ");
			writeStr(writer, decl->name());
			for (const ConstantOrLambdaOrVariable clv : key.specializeOnArgs) {
				//TODO: print type args too
				writeChar(writer, ' ');
				writeConstantOrLambdaOrVariable(writer, clv);
			}
			printf("%s\n", finishWriterToCStr(writer));
		}

		const TypeArgsScope typeScope = key.typeArgsScope();
		const ConcreteType returnType = getConcreteType(ctx, decl->returnType(), typeScope);
		const Arr<const ConcreteParam> params = concretizeParamsAndSpecialize(ctx, decl->params(), key.specializeOnArgs, typeScope);
		const Str mangledName = decl->isExtern()
			? decl->name()
			: getConcreteFunMangledName(
				ctx.arena,
				decl->name(),
				returnType,
				params,
				key.specImpls(),
				key.specializeOnArgs);
		const ConcreteSig sig = ConcreteSig{mangledName, returnType, params};
		// no closure for fun from decl
		ConcreteFun* res = ctx.arena.nu<ConcreteFun>()(_not(decl->noCtx()), none<const ConcreteParam>(), sig, isCallFun(ctx, decl));

		if (isNonSpecializableBuiltin(decl))
			for (const ConstantOrLambdaOrVariable clv : key.specializeOnArgs)
				assert(clv.isVariable());

		assert(key.decl()->params().size == key.specializeOnArgs.size);

		const ConcreteFunSource source = ConcreteFunSource{
			res,
			key.funInst,
			key.specializeOnArgs,
			decl->body(),
			none<const KnownLambdaBody*>()};
		ctx.concreteFunToSource.add(ctx.arena, res, source);
		return res;
	}

	//TODO: only used for side effect
	const ConcreteFunBody fillInConcreteFunBody(ConcretizeCtx& ctx, ConcreteFun* cf) {
		// TODO: just assert it's not already set?
		if (!cf->_body.isSet()) {
			if (PRINT_INSTANTIATION) {
				INSTANTIATING_INDENT++;
				printInstantiatingIndent();
				printf("FILLING IN BODY\n");
			}

			cf->_body.set(ConcreteFunBody{
				ctx.arena.nu<ConcreteExpr>()(cf->returnType(), SourceRange::empty(), none<const KnownLambdaBody*>(), ConcreteExpr::Bogus{})});
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

			if (PRINT_INSTANTIATION) {
				printInstantiatingIndent();
				printf("DONE FILLING BODY\n");

				assert(INSTANTIATING_INDENT != 0);
				INSTANTIATING_INDENT--;
			}

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
		writeStr(writer, knownLambdaBodyMangledName);
		writeSpecializeOnArgsForMangle(writer, specializeOnArgs);
		if (isForDynamic)
			writeStatic(writer, "__dynamic");
		return finishWriter(writer);
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
			const ConcreteParam p = at(nonSpecializedParams, i);
			const Opt<const ConcreteType> t = getSpecializedParamType(at(specializeOnArgs, i), [&]() { return p.type; });
			if (t.has())
				res.add(ctx.arena, ConcreteParam{p.mangledName, t.force()});
		}
		return res.finish();
	}

	const Bool shouldAllocateClosureForDynamicLambda(const ConcreteType closureType) {
		// TODO:PERF we could avoid the pointer for closures that don't exceed pointer size.
		return _not(closureType.isPointer);
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

		// For a dynamic lambda, the closure should always be a pointer.
		const Opt<const ConcreteParam> closure = isForDynamic
			? some<const ConcreteParam>([&]() {
				if (klb->hasClosure()) {
					const ConcreteParam closureParam = klb->closureParam.force();
					return shouldAllocateClosureForDynamicLambda(closureParam.type) ? closureParam.withType(closureParam.type.byRef()) : closureParam;
				} else
					return ConcreteParam{strLiteral(""), ctx.anyPtrType()};
			}())
			: klb->closureParam;

		ConcreteFun* res = ctx.arena.nu<ConcreteFun>()(
			/*needsCtx*/ True, // TODO:PERF for some non-dynamic calls it does not need ctx
			closure,
			sig,
			/*isCallFun*/ False);

		const ConcreteFunSource source = ConcreteFunSource{
			res,
			info.containingFunInst,
			specializeOnArgs,
			FunBody{info.body},
			some<const KnownLambdaBody*>(klb)};
		ctx.concreteFunToSource.add(ctx.arena, res, source);

		fillInConcreteFunBody(ctx, res);

		return res;
	}

	ConcreteFun* getOrAddConcreteFunWithoutFillingBody(ConcretizeCtx& ctx, const ConcreteFunKey key) {
		return ctx.allConcreteFuns.getOrAdd(ctx.arena, key, [&]() {
			return getConcreteFunFromKey(ctx, key);
		});
	}

	Comparison compareConcreteFunInst(const ConcreteFunInst a, const ConcreteFunInst b) {
		const Comparison cmpDecl = comparePtr(a.decl, b.decl);
		if (cmpDecl != Comparison::equal)
			return cmpDecl;
		else {
			const Comparison res = compareConcreteTypeArr(a.typeArgs, b.typeArgs);
			return res != Comparison::equal ? res : compareArr<const ConcreteFunInst, compareConcreteFunInst>(a.specImpls, b.specImpls);
		}
	}
}

Comparison compareConcreteStructKey(const ConcreteStructKey a, const ConcreteStructKey b) {
	const Comparison res = comparePtr(a.decl, b.decl);
	return res != Comparison::equal ? res : compareConcreteTypeArr(a.typeArgs, b.typeArgs);
}

Comparison compareConcreteFunKey(const ConcreteFunKey a, const ConcreteFunKey b) {
	const Comparison res = compareConcreteFunInst(a.funInst, b.funInst);
	return res != Comparison::equal
		? res
		: compareArr<const ConstantOrLambdaOrVariable, compareConstantOrLambdaOrVariable>(a.specializeOnArgs, b.specializeOnArgs);
}

const ConcreteType ConcretizeCtx::boolType() {
	return lazilySet(_boolType, [&]() {
		return getConcreteType_forStructInst(*this, commonTypes._bool, TypeArgsScope::empty());
	});
}

const ConcreteType ConcretizeCtx::charType() {
	return lazilySet(_charType, [&]() {
		return getConcreteType_forStructInst(*this, commonTypes._char, TypeArgsScope::empty());
	});
}

const ConcreteType ConcretizeCtx::voidType() {
	return lazilySet(_voidType, [&]() {
		return getConcreteType_forStructInst(*this, commonTypes._void, TypeArgsScope::empty());
	});
}

const ConcreteType ConcretizeCtx::anyPtrType() {
	return lazilySet(_anyPtrType, [&]() {
		return getConcreteType_forStructInst(*this, commonTypes.anyPtr, TypeArgsScope::empty());
	});
}

const ConcreteType ConcretizeCtx::ctxPtrType() {
	return lazilySet(_ctxPtrType, [&]() {
		const ConcreteType res = getConcreteType_forStructInst(*this, commonTypes.ctx, TypeArgsScope::empty());
		assert(res.isPointer);
		return res;
	});
}

const ConcreteFun* getOrAddNonTemplateConcreteFunAndFillBody(ConcretizeCtx& ctx, const FunDecl* decl) {
	const ConcreteFunKey key = ConcreteFunKey{
		ConcreteFunInst{
			decl,
			emptyArr<const ConcreteType>(),
			emptyArr<const ConcreteFunInst>()},
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
			assert(ptrEquals(p, getPtr(typeArgsScope.typeParams, p->index)));
			return at(typeArgsScope.typeArgs, p->index);
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

namespace {
	const Opt<const ConcreteType> concreteTypeFromFieldsCommon(Arena& arena, const Arr<const ConcreteField> fields, const Str mangledName, const Bool neverPointer) {
		if (isEmpty(fields))
			return none<const ConcreteType>();
		else {
			ConcreteStruct* cs = arena.nu<ConcreteStruct>()(
				mangledName,
				none<const SpecialStructKind>(),
				ConcreteStructBody{ConcreteStructBody::Fields{fields}});
			cs->_sizeBytes.set(sizeFromConcreteFields(fields));
			return some<const ConcreteType>(
				neverPointer ? ConcreteType::value(cs) : ConcreteType::fromStruct(cs));
		}
	}
}

const Opt<const ConcreteType> concreteTypeFromFields(Arena& arena, const Arr<const ConcreteField> fields, const Str mangledName) {
	return concreteTypeFromFieldsCommon(arena, fields, mangledName, False);
}

const Opt<const ConcreteType> concreteTypeFromFields_neverPointer(Arena& arena, const Arr<const ConcreteField> fields, const Str mangledName) {
	return concreteTypeFromFieldsCommon(arena, fields, mangledName, True);
}

const Bool isCallFun(ConcretizeCtx& ctx, const FunDecl* decl) {
	return exists(ctx.callFuns, [&](const FunDecl* d) {
		return ptrEquals(d, decl);
	});
}

namespace {
	const ConstantOrExpr makeLambdasDynamic_forConstant(ConcretizeCtx& ctx, const SourceRange range, const Constant* c) {
		if (c->kind.isLambda()) {
			const KnownLambdaBody* klb = c->kind.asLambda().knownLambdaBody;
			const ConcreteFun* fun = instantiateKnownLambdaBodyForDynamic(ctx, klb);
			const ConcreteType closureType = fun->closureType().force();
			const ConstantOrExpr closure = ConstantOrExpr{ctx.allConstants._null(ctx.arena, closureType)};
			const ConcreteExpr::LambdaToDynamic ld = ConcreteExpr::LambdaToDynamic{fun, closure};
			return nuExpr(ctx.arena, klb->dynamicType, range, ld);
		} else {
			return ConstantOrExpr{c};
		}
	}

	const ConstantOrExpr makeLambdasDynamic_forExpr(ConcretizeCtx& ctx, const SourceRange range, const ConcreteExpr* e) {
		if (e->knownLambdaBody().has()) {
			const KnownLambdaBody* klb = e->knownLambdaBody().force();
			const ConcreteFun* fun = instantiateKnownLambdaBodyForDynamic(ctx, klb);
			const ConcreteType closureType = klb->closureType().force();
			const ConstantOrExpr closure = shouldAllocateClosureForDynamicLambda(closureType)
				? nuExpr(
					ctx.arena,
					closureType.changeToByRef(),
					range,
					none<const KnownLambdaBody*>(),
					ConcreteExpr::Alloc{getAllocFun(ctx), e})
				: ConstantOrExpr{e};
			return nuExpr(ctx.arena, klb->dynamicType, range, ConcreteExpr::LambdaToDynamic{fun, closure});
		} else
			return ConstantOrExpr{e};
	}

	const SpecializeOnArgs yesSpecialize(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args, const Bool specializeOnAllConstants) {
		// TODO: helper to map to two different arrs
		ArrBuilder<const ConstantOrExpr> notSpecializedArgs {};
		const Arr<const ConstantOrLambdaOrVariable> specializeOnArgs = map<const ConstantOrLambdaOrVariable>{}(ctx.arena, args, [&](const ConstantOrExpr ce) {
			return ce.match(
				[&](const Constant* c) {
					if (specializeOnAllConstants || c->kind.isLambda())
						// Still specialize on lambda constants
						return ConstantOrLambdaOrVariable{c};
					else {
						notSpecializedArgs.add(ctx.arena, ConstantOrExpr{c});
						return ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}};
					}
				},
				[&](const ConcreteExpr* e) {
					if (e->knownLambdaBody().has()) {
						notSpecializedArgs.add(ctx.arena, ConstantOrExpr{e});
						return ConstantOrLambdaOrVariable{e->knownLambdaBody().force()};
					} else {
						notSpecializedArgs.add(ctx.arena, makeLambdasDynamic_forExpr(ctx, range, e));
						return ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}};
					}
				});
		});
		return SpecializeOnArgs{specializeOnArgs, notSpecializedArgs.finish()};
	}

	const SpecializeOnArgs dontSpecialize(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args) {
		return SpecializeOnArgs{allVariable(ctx.arena, args.size), makeLambdasDynamic_arr(ctx, range, args)};
	}
}

const SpecializeOnArgs getSpecializeOnArgsForLambdaClosure(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args) {
	return yesSpecialize(ctx, range, args, /*specializeOnAllConstants*/ False);
}

const SpecializeOnArgs getSpecializeOnArgsForLambdaCall(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> args, const Bool isSummon) {
	Cell<const Bool> allConstant { True };
	Cell<const Bool> someFun { False };
	for (const ConstantOrExpr c : args) {
		if (getKnownLambdaBodyFromConstantOrExpr(c).has())
			someFun.set(True);
		if (!c.isConstant())
			allConstant.set(False);
	}

	// Specialize on funs with all-constant parameters that are non-'summon' as these will likely evaluate to constants.
	// TODO: also require return type to be immutable
	const Bool isConstant = _and(allConstant.get(), !isSummon);

	// Always specialize on a fun, even if fun is summon
	return isConstant || someFun.get()
		? yesSpecialize(ctx, range, args, isConstant)
		: dontSpecialize(ctx, range, args);
}

const SpecializeOnArgs getSpecializeOnArgsForFun(ConcretizeCtx& ctx, const SourceRange range, const FunDecl* f, const Arr<const ConstantOrExpr> args) {
	// Don't specialize just because a single arg is a constant.
	// If *every* arg is a constant, we always specialize on all args.
	// Else, specialize if some arg has a known lambda body. (TODO: and that lambda is *only* called.)

	// Never specialize on 'call' -- though we do treat it specially in 'concretizeCall'
	return f->isExtern() || isNonSpecializableBuiltin(f) || isCallFun(ctx, f)
		? dontSpecialize(ctx, range, args)
		: getSpecializeOnArgsForLambdaCall(ctx, range, args, f->isSummon());
}

const Arr<const ConcreteField> concretizeClosureFieldsAndSpecialize(
	ConcretizeCtx& ctx,
	const Arr<const ClosureField*> closure,
	const Arr<const ConstantOrLambdaOrVariable> closureSpecialize,
	const TypeArgsScope typeArgsScope
) {
	ArrBuilder<const ConcreteField> res;
	for (const size_t i : Range{closure.size}) {
		const ClosureField* c = at(closure, i);
		const Opt<const ConcreteType> t = getSpecializedParamType(at(closureSpecialize, i), [&]() {
			return getConcreteType(ctx, c->type, typeArgsScope);
		});
		if (t.has())
			res.add(ctx.arena, ConcreteField{/*isMutable*/ False, copyStr(ctx.arena, c->name), t.force()});
	}
	return res.finish();
}

const Arr<const ConcreteParam> concretizeParamsNoSpecialize(ConcretizeCtx& ctx, const Arr<const Param> params, const TypeArgsScope typeArgsScope) {
	return concretizeParamsAndSpecialize(ctx, params, allVariable(ctx.arena, params.size), typeArgsScope);
}

const ConstantOrExpr makeLambdasDynamic(ConcretizeCtx& ctx, const SourceRange range, const ConstantOrExpr expr) {
	return expr.match(
		[&](const Constant* c) {
			return makeLambdasDynamic_forConstant(ctx, range, c);
		},
		[&](const ConcreteExpr* e) {
			return makeLambdasDynamic_forExpr(ctx, range, e);
		});
}

const Arr<const ConstantOrExpr> makeLambdasDynamic_arr(ConcretizeCtx& ctx, const SourceRange range, const Arr<const ConstantOrExpr> exprs) {
	return map<const ConstantOrExpr>{}(ctx.arena, exprs, [&](const ConstantOrExpr expr) {
		return makeLambdasDynamic(ctx, range, expr);
	});
}
