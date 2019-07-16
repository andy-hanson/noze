#include "./concretize.h"

#include "../util/arrUtil.h"
#include "./concretizeCtx.h"
#include "./getReferencedOnly.h"

namespace {
	void checkMainSignature(const CommonTypes& commonTypes, const FunDecl* mainFun) {
		if (!mainFun->noCtx())
			todo<void>("main must be noctx");
		if (mainFun->isTemplate())
			todo<void>("main is Template?");
		const Arr<const Param> params = mainFun->params();
		if (size(params) == 0) {
			const Type ret = mainFun->returnType();
			if (!(ret.isStructInst() && ptrEquals(ret.asStructInst()->decl, commonTypes.int64->decl)))
				todo<void>("checkMainSignature -- doesn't return int64");
		} else
			// ALlow taking (int argc, char** argv)
			todo<void>("checkMainSignature");
	}

	const FunDecl* getMainFun(const Program program) {
		const Arr<const FunDecl*> mainFuns = multiDictGetAt<const Sym, const FunDecl*, compareSym>(program.mainModule->funsMap, shortSymAlphaLiteral("main"));
		if (size(mainFuns) != 1)
			todo<void>("wrong number main funs");
		const FunDecl* mainFun = only(mainFuns);
		checkMainSignature(program.commonTypes, mainFun);
		return mainFun;
	}

	const FunDecl* getAllocFun(const Program program) {
		const Arr<const FunDecl*> allocFuns = multiDictGetAt<const Sym, const FunDecl*, compareSym>(program.includeModule->funsMap, shortSymAlphaLiteral("alloc"));
		if (size(allocFuns) != 1)
			todo<void>("wrong number allocate-bytes funs");
		const FunDecl* allocFun = only(allocFuns);
		// TODO: check the signature!
		return allocFun;
	}

	// Gets 'call' for 'fun'
	// 'call' for 'fun-ptr' is a builtin already so no need to handle that here
	const Arr<const FunDecl*> getCallFuns(Arena* arena, const Program program) {
		const Arr<const FunDecl*> allCallFuns = multiDictGetAt<const Sym, const FunDecl*, compareSym>(program.includeModule->funsMap, shortSymAlphaLiteral("call"));
		const Arr<const FunDecl*> res = filter(arena, allCallFuns, [&](const FunDecl* f) {
			const StructDecl* decl = first(f->params()).type.asStructInst()->decl;
			return exists(program.commonTypes.funTypes, [&](const StructDecl* funStruct) {
				return ptrEquals(decl, funStruct);
			});
		});
		assert(size(res) == 3);
		return res;
	}
}

const ConcreteProgram concretize(Arena* arena, const Program program) {
	ConcretizeCtx ctx { arena, getAllocFun(program), getCallFuns(arena, program), program.commonTypes };
	const ConcreteFun* mainConcreteFun = getOrAddNonTemplateConcreteFunAndFillBody(&ctx, getMainFun(program));
	// We remove items from these dicts when we process them.
	assert(mutDictIsEmpty(&ctx.concreteFunToSource));
	return getReferencedOnly(arena, mainConcreteFun, ctx.ctxPtrType().strukt);
}
