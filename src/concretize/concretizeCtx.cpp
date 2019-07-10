#include "./concretizeCtx.h"

#include "../util/arrUtil.h"
#include "./builtinInfo.h"
#include "./concretizeBuiltin.h" // getBuiltinFunBody
#include "./concretizeExpr.h"
#include "./concretizeUtil.h"
#include "./mangleName.h"
#include "./specialize.h" // concretizeParamsNoSpecialize

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
		size_t maxFieldAlign = 1;
		for (const ConcreteField field : fields) {
			const size_t itsSize = field.type.sizeOrPointerSizeBytes();
			const size_t itsAlign = min<const size_t>(itsSize, 8); //TODO: this is wrong!
			maxFieldAlign = max<const size_t>(maxFieldAlign, itsAlign);
			while (s % itsAlign != 0)
				s++;
			s += itsSize;
		}
		while (s % maxFieldAlign != 0)
			s++;
		return max<const size_t>(s, 1);
	}

	const Bool getDefaultIsPointerForFields(const Opt<const ForcedByValOrRef> forcedByValOrRef, const size_t sizeBytes, const Bool isSelfMutable) {
		if (forcedByValOrRef.has())
			switch (forcedByValOrRef.force()) {
				case ForcedByValOrRef::byVal:
					assert(!isSelfMutable);
					return False;
				case ForcedByValOrRef::byRef:
					return True;
				default:
					assert(0);
			}
		else
			return _or(isSelfMutable, gt(sizeBytes, sizeof(void*) * 2));
	}

	const ConcreteStructInfo getConcreteStructInfoForFields(const Opt<const ForcedByValOrRef> forcedByValOrRef, const Arr<const ConcreteField> fields) {
		const size_t sizeBytes = sizeFromConcreteFields(fields);
		const Bool isSelfMutable = /*isSelfMutable*/ exists(fields, [](const ConcreteField fld) {
			return fld.isMutable;
		});
		return ConcreteStructInfo{
			ConcreteStructBody{ConcreteStructBody::Record{fields}},
			sizeBytes,
			isSelfMutable,
			getDefaultIsPointerForFields(forcedByValOrRef, sizeBytes, isSelfMutable),
		};
	}

	void initializeConcreteStruct(
		ConcretizeCtx& ctx,
		const Arr<const ConcreteType> typeArgs,
		const StructInst* i,
		ConcreteStruct* res,
		const TypeArgsScope typeArgsScope
	) {
		// Initially make this a by-ref type, so we don't recurse infinitely when computing size
		// TODO: is this a bug? We compute the size based on assuming it's a pointer, then make it not be a pointer and that would change the size?
		res->_info.set(ConcreteStructInfo{
			ConcreteStructBody{ConcreteStructBody::Bogus{}},
			/*sizeBytes*/ 9999,
			// If we recurse, it should be by pointer.
			/*isSelfMutable*/ True,
			/*defaultIsPointer*/ True,
		});

		const ConcreteStructInfo info = i->body().match(
			[](const StructBody::Bogus) {
				return unreachable<const ConcreteStructInfo>();
			},
			[&](const StructBody::Builtin) {
				const BuiltinStructInfo b = getBuiltinStructInfo(i->decl);
				return ConcreteStructInfo{
					ConcreteStructBody{ConcreteStructBody::Builtin{b, typeArgs}},
					b.sizeBytes,
					/*isSelfMutable*/ False,
					/*defaultIsPointer*/ False
				};
			},
			[&](const StructBody::Record r) {
				const Arr<const ConcreteField> fields = map<const ConcreteField>{}(ctx.arena, r.fields, [&](const StructField f) {
					return ConcreteField{f.isMutable, mangleName(ctx.arena, f.name), getConcreteType(ctx, f.type, typeArgsScope)};
				});
				return getConcreteStructInfoForFields(r.forcedByValOrRef, fields);
			},
			[&](const StructBody::Union u) {
				const Arr<const ConcreteType> members = map<const ConcreteType>{}(ctx.arena, u.members, [&](const StructInst* si) {
					return getConcreteType_forStructInst(ctx, si, typeArgsScope);
				});

				size_t maxMember = 0;
				for (const ConcreteType ct : members)
					maxMember = max<const size_t>(maxMember, ct.sizeOrPointerSizeBytes());
				// Must factor in the 'kind' size. It seems that enums are int-sized.
				const size_t sizeBytes = roundUp(sizeof(int) + maxMember, sizeof(void*));

				return ConcreteStructInfo{
					ConcreteStructBody{ConcreteStructBody::Union{members}},
					sizeBytes,
					/*isSelfMutable*/ False,
					/*defaultIsPointer*/ False
				};
			},
			[&](const StructBody::Iface i) {
				const Arr<const ConcreteSig> messages = map<const ConcreteSig>{}(ctx.arena, i.messages, [&](const Message msg) {
					return ConcreteSig{
						mangleName(ctx.arena, msg.sig.name),
						getConcreteType(ctx, msg.sig.returnType, typeArgsScope),
						concretizeParamsNoSpecialize(ctx, msg.sig.params, typeArgsScope)};
				});
				return ConcreteStructInfo{
					ConcreteStructBody{ConcreteStructBody::Iface{messages}},
					sizeof(void*) * 2,
					/*isSelfMutable*/ False,
					/*defaultIsPoiner*/ False
				};
			});

		res->_info.setOverwrite(info);
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

	//TODO: only used for side effect?
	const ConcreteFunBody fillInConcreteFunBody(ConcretizeCtx& ctx, ConcreteFun* cf) {
		// TODO: just assert it's not already set?
		if (!cf->_body.isSet()) {
			if (PRINT_INSTANTIATION) {
				INSTANTIATING_INDENT++;
				printInstantiatingIndent();
				printf("FILLING IN BODY\n");
			}

			cf->_body.set(ConcreteFunBody{ConcreteFunBody::Bogus{}});

			const ConcreteFunSource source = ctx.concreteFunToSource.tryDeleteAndGet(cf).force();
			const ConcreteFunBody body = source.body.match(
				[&](const FunBody::Builtin) {
					return getBuiltinFunBody(ctx, source, cf);
				},
				[&](const FunBody::Extern) {
					return ConcreteFunBody{ConcreteFunBody::Extern{}};
				},
				[&](const Expr* e) {
					return doConcretizeExpr(ctx, source, cf, *e);
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
					return ConcreteParam{strLiteral("__unused"), ctx.anyPtrType()};
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
		allVariable(ctx.arena, arity(decl))};
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
				getConcreteStructInfoForFields(none<const ForcedByValOrRef>(), fields));
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
