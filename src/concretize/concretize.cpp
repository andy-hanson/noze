#include "./concretize.h"

#include "../util/arrUtil.h"
#include "./concretizeCtx.h"
#include "./getReferencedOnly.h"

namespace {
	const Bool isInt32(const CommonTypes* commonTypes, const Type type) {
		return typeEquals(type, Type{commonTypes->int32});
	}

	const Bool isStr(const CommonTypes* commonTypes, const Type type) {
		return typeEquals(type, Type{commonTypes->str});
	}

	const Bool isFutInt32(const CommonTypes* commonTypes, const Type type) {
		return _and3(
			type.isStructInst(),
			ptrEquals(type.asStructInst()->decl, commonTypes->fut),
			isInt32(commonTypes, only(type.asStructInst()->typeArgs)));
	}

	const Bool isArrStr(const CommonTypes* commonTypes, const Type type) {
		return _and3(
			type.isStructInst(),
			ptrEquals(type.asStructInst()->decl, commonTypes->arr),
			isStr(commonTypes, only(type.asStructInst()->typeArgs)));
	}

	void checkRtMainSignature(const CommonTypes* commonTypes, const FunDecl* mainFun) {
		if (!mainFun->noCtx())
			todo<void>("main must be noctx");
		if (mainFun->isTemplate())
			todo<void>("main is template?");

		// rt-main should take (int32 argc, char** argv)
		const Arr<const Param> params = mainFun->params();
		if (size(params) != 3)
			todo<void>("checkRtMainSignature wrong number params");

		if (!isInt32(commonTypes, at(params, 0).type))
			todo<void>("checkRtMainSignature doesn't take int");
		// TODO: check p1 type is ptr c-str
		// TODO: check p2 type is fun-ptr2 fut<int> ctx arr<str>)
		if (!isInt32(commonTypes, mainFun->returnType()))
			todo<void>("checkRtMainSignature doesn't return int");
	}

	void checkUserMainSignature(const CommonTypes* commonTypes, const FunDecl* mainFun) {
		if (mainFun->noCtx())
			todo<void>("main is noctx?");
		if (mainFun->isTemplate())
			todo<void>("main is template?");
		const Arr<const Param> params = mainFun->params();
		if (size(params) != 1)
			todo<void>("checkUserMainSignature should take 1 param");
		// Must take arr str
		const Type p0 = only(params).type;
		const Type ret = mainFun->returnType();
		if (!isArrStr(commonTypes, p0))
			todo<void>("checkUserMainSignature doesn't take arr str");
		if (!isFutInt32(commonTypes, ret))
			todo<void>("checkUserMainSignature doesn't return fut int");
	}

	const FunDecl* getRtMainFun(const Program program) {
		const Arr<const FunDecl*> mainFuns = multiDictGetAt<const Sym, const FunDecl*, compareSym>(
			program.runtimeModule->funsMap,
			shortSymAlphaLiteral("rt-main"));
		if (size(mainFuns) != 1) {
			printf("%lu\n", size(mainFuns));
			todo<void>("wrong number rt-main funs");
		}
		const FunDecl* mainFun = only(mainFuns);
		checkRtMainSignature(&program.commonTypes, mainFun);
		return mainFun;
	}

	const FunDecl* getUserMainFun(const Program program) {
		const Arr<const FunDecl*> mainFuns = multiDictGetAt<const Sym, const FunDecl*, compareSym>(
			program.mainModule->funsMap,
			shortSymAlphaLiteral("user-main"));
		if (size(mainFuns) != 1) {
			printf("%lu\n", size(mainFuns));
			todo<void>("wrong number user-main funs");
		}
		const FunDecl* mainFun = only(mainFuns);
		checkUserMainSignature(&program.commonTypes, mainFun);
		return mainFun;
	}

	const FunDecl* getAllocFun(const Program program) {
		const Arr<const FunDecl*> allocFuns = multiDictGetAt<
			const Sym,
			const FunDecl*,
			compareSym
		>(program.gcModule->funsMap, shortSymAlphaLiteral("alloc"));
		if (size(allocFuns) != 1)
			todo<void>("wrong number alloc funs");
		const FunDecl* allocFun = only(allocFuns);
		// TODO: check the signature!
		return allocFun;
	}

	const FunDecl* getGetVatAndActorFun(const Program program) {
		const Arr<const FunDecl*> funs = multiDictGetAt<
			const Sym,
			const FunDecl*,
			compareSym
		>(program.runtimeModule->funsMap, shortSymAlphaLiteral("cur-actor"));
		if (size(funs) != 1)
			todo<void>("wrong number cur-actor funs");
		return only(funs);
	}

	const Arr<const FunDecl*> getIfFuns(const Program program) {
		const Arr<const FunDecl*> ifFuns = multiDictGetAt<
			const Sym,
			const FunDecl*,
			compareSym
		>(program.bootstrapModule->funsMap, shortSymAlphaLiteral("if"));
		if (size(ifFuns) != 2)
			todo<void>("wrong number 'if' funs");
		return ifFuns;
	}

	// Gets 'call' for 'fun' and 'fun-mut'
	// 'call' for 'fun-ptr' is a builtin already so no need to handle that here
	// Don't need 'call' for 'fun-ref' here.
	const Arr<const FunDecl*> getCallFuns(Arena* arena, const Program program) {
		const Arr<const FunDecl*> allCallFuns = multiDictGetAt<
			const Sym,
			const FunDecl*,
			compareSym
		>(program.bootstrapModule->funsMap, shortSymAlphaLiteral("call"));
		const Arr<const FunDecl*> res = filter(arena, allCallFuns, [&](const FunDecl* f) {
			const StructDecl* decl = first(f->params()).type.asStructInst()->decl;
			const Opt<const FunKind> kind = program.commonTypes.getFunStructInfo(decl);
			if (has(kind))
				switch (force(kind)) {
					case FunKind::ptr:
					case FunKind::ref:
						return False;
					case FunKind::plain:
					case FunKind::mut:
						return True;
					default:
						assert(0);
				}
			else
				return False;
		});
		// fun0, fun1, fun2, fun3, same for funMut
		assert(size(res) == 8);
		return res;
	}
}

const ConcreteProgram concretize(Arena* arena, const Program program) {
	ConcretizeCtx ctx {
		arena,
		getAllocFun(program),
		getGetVatAndActorFun(program),
		getIfFuns(program),
		getCallFuns(arena, program),
		program.ctxStructInst,
		&program.commonTypes
	};
	const ConcreteFun* rtMainConcreteFun = getOrAddNonTemplateConcreteFunAndFillBody(&ctx, getRtMainFun(program));
	// We remove items from these dicts when we process them.
	assert(mutDictIsEmpty(&ctx.concreteFunToSource));
	const ConcreteFun* userMainConcreteFun = getOrAddNonTemplateConcreteFunAndFillBody(&ctx, getUserMainFun(program));
	// We remove items from these dicts when we process them.
	assert(mutDictIsEmpty(&ctx.concreteFunToSource));

	return getReferencedOnly(arena, rtMainConcreteFun, userMainConcreteFun, ctx.ctxType().strukt);
}
