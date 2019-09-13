#include "./concretize.h"

#include "../util/arrUtil.h"
#include "./concretizeCtx.h"
#include "./getReferencedOnly.h"

namespace {
	void checkMainSignature(const CommonTypes* commonTypes, const FunDecl* mainFun) {
		if (!mainFun->noCtx())
			todo<void>("main must be noctx");
		if (mainFun->isTemplate())
			todo<void>("main is Template?");
		const Arr<const Param> params = mainFun->params();
		if (size(params) == 0) {
			const Type ret = mainFun->returnType();
			if (!(ret.isStructInst() && ptrEquals(ret.asStructInst()->decl, commonTypes->int64->decl)))
				todo<void>("checkMainSignature -- doesn't return int64");
		} else
			// ALlow taking (int argc, char** argv)
			todo<void>("checkMainSignature");
	}

	const FunDecl* getMainFun(const Program program) {
		const Arr<const FunDecl*> mainFuns = multiDictGetAt<const Sym, const FunDecl*, compareSym>(
			program.mainModule->funsMap,
			shortSymAlphaLiteral("main"));
		if (size(mainFuns) != 1) {
			printf("%lu\n", size(mainFuns));
			todo<void>("wrong number main funs");
		}
		const FunDecl* mainFun = only(mainFuns);
		checkMainSignature(&program.commonTypes, mainFun);
		return mainFun;
	}

	const FunDecl* getAllocFun(const Program program) {
		const Arr<const FunDecl*> allocFuns = multiDictGetAt<
			const Sym,
			const FunDecl*,
			compareSym
		>(program.includeModule->funsMap, shortSymAlphaLiteral("alloc"));
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
		>(program.includeModule->funsMap, shortSymAlphaLiteral("cur-actor"));
		if (size(funs) != 1)
			todo<void>("wrong number get-vat-and-actor funs");
		return only(funs);
	}

	const Arr<const FunDecl*> getIfFuns(const Program program) {
		const Arr<const FunDecl*> ifFuns = multiDictGetAt<
			const Sym,
			const FunDecl*,
			compareSym
		>(program.includeModule->funsMap, shortSymAlphaLiteral("if"));
		if (size(ifFuns) != 2)
			todo<void>("wrong number 'if' funs");
		return ifFuns;
	}

	// Gets 'call' for 'fun'
	// 'call' for 'fun-ptr' is a builtin already so no need to handle that here
	const Arr<const FunDecl*> getCallFuns(Arena* arena, const Program program) {
		const Arr<const FunDecl*> allCallFuns = multiDictGetAt<
			const Sym,
			const FunDecl*,
			compareSym
		>(program.includeModule->funsMap, shortSymAlphaLiteral("call"));
		const Arr<const FunDecl*> res = filter(arena, allCallFuns, [&](const FunDecl* f) {
			const StructDecl* decl = first(f->params()).type.asStructInst()->decl;
			const Opt<const FunKind> kind = program.commonTypes.getFunStructInfo(decl);
			return has(kind) && force(kind) != FunKind::send;
		});
		assert(size(res) == 6);
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
		&program.commonTypes
	};
	const ConcreteFun* mainConcreteFun = getOrAddNonTemplateConcreteFunAndFillBody(&ctx, getMainFun(program));
	// We remove items from these dicts when we process them.
	assert(mutDictIsEmpty(&ctx.concreteFunToSource));
	return getReferencedOnly(arena, mainConcreteFun, ctx.ctxPtrType().strukt);
}
