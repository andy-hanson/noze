#include "./getReferencedOnly.h"

#include "../util/arrUtil.h"

namespace {
	template <typename T>
	void pushIfNotContained(Arena* arena, MutArr<const T*>* all, const T* t) {
		if (!contains<const T*, ptrEquals<const T>>(tempAsArr(all), t))
			push(arena, all, t);
	}

	struct SetReferencedCtx {
		Arena* arena;
		MutArr<const ConcreteStruct*> allReferencedStructs {};
		MutArr<const Constant*> allReferencedConstants {};
		MutArr<const ConcreteFun*> allReferencedFuns {};
		MutArr<const ConstantArrayBacking*> allArrayBackings {};
		SetReferencedCtx(const SetReferencedCtx*) = delete;
	};

	void addStruct(SetReferencedCtx* ctx, const ConcreteStruct* s) {
		pushIfNotContained(ctx->arena, &ctx->allReferencedStructs, s);
	}

	void addConstant(SetReferencedCtx* ctx, const Constant* c) {
		pushIfNotContained(ctx->arena, &ctx->allReferencedConstants, c);
	}

	void addFun(SetReferencedCtx* ctx, const ConcreteFun* f) {
		pushIfNotContained(ctx->arena, &ctx->allReferencedFuns, f);
	}

	void addArrayBacking(SetReferencedCtx* ctx, const ConstantArrayBacking* a) {
		pushIfNotContained(ctx->arena, &ctx->allArrayBackings, a);
	}

	void setReferencedInType(SetReferencedCtx* ctx, const ConcreteType t) {
		addStruct(ctx, t.strukt);
	}

	void setReferencedInSig(SetReferencedCtx* ctx, const ConcreteSig sig) {
		setReferencedInType(ctx, sig.returnType);
		for (const ConcreteParam p : sig.params)
			setReferencedInType(ctx, p.type);
	}

	void setReferencedInStruct(SetReferencedCtx* ctx, const ConcreteStruct* s) {
		s->body().match(
			[&](const ConcreteStructBody::Builtin b) {
				for (const ConcreteType s : b.typeArgs)
					setReferencedInType(ctx, s);
			},
			[&](const ConcreteStructBody::Record r) {
				for (const ConcreteField field : r.fields)
					setReferencedInType(ctx, field.type);
			},
			[&](const ConcreteStructBody::Union u) {
				for (const ConcreteType member : u.members)
					setReferencedInType(ctx, member);
			});
	}

	void setReferencedInExpr(SetReferencedCtx* ctx, const ConcreteExpr ce);

	void setReferencedInConstantOrExpr(SetReferencedCtx* ctx, const ConstantOrExpr ce) {
		ce.match(
			[&](const Constant* c) {
				addConstant(ctx, c);
			},
			[&](const ConcreteExpr* ce) {
				setReferencedInExpr(ctx, *ce);
			});
	}

	void setReferencedInConstantOrExprs(SetReferencedCtx* ctx, const Arr<const ConstantOrExpr> args) {
		for (const ConstantOrExpr arg : args)
			setReferencedInConstantOrExpr(ctx, arg);
	}

	void setReferencedInExpr(SetReferencedCtx* ctx, const ConcreteExpr ce) {
		ce.match(
			[](const ConcreteExpr::Bogus) {
				unreachable<void>();
			},
			[&](const ConcreteExpr::Alloc e) {
				addFun(ctx, e.alloc);
				setReferencedInExpr(ctx, *e.inner);
			},
			[&](const ConcreteExpr::Call e) {
				addFun(ctx, e.called);
				setReferencedInConstantOrExprs(ctx, e.args);
			},
			[&](const ConcreteExpr::Cond e) {
				setReferencedInExpr(ctx, *e.cond);
				setReferencedInConstantOrExpr(ctx, e.then);
				setReferencedInConstantOrExpr(ctx, e.elze);
			},
			[&](const ConcreteExpr::CreateArr e) {
				addStruct(ctx, e.arrType);
				addFun(ctx, e.alloc);
				setReferencedInConstantOrExprs(ctx, e.args);
			},
			[&](const ConcreteExpr::CreateRecord e) {
				const ConcreteType type = force(ce.typeWithKnownLambdaBody());
				setReferencedInType(ctx, type);
				setReferencedInConstantOrExprs(ctx, e.args);
			},
			[&](const ConcreteExpr::ImplicitConvertToUnion e) {
				setReferencedInConstantOrExpr(ctx, e.arg);
			},
			[&](const ConcreteExpr::Lambda e) {
				setReferencedInConstantOrExprs(ctx, e.closureInit);
			},
			[&](const ConcreteExpr::LambdaToDynamic e) {
				addFun(ctx, e.fun);
				setReferencedInConstantOrExpr(ctx, e.closure);
			},
			[&](const ConcreteExpr::Let e) {
				setReferencedInExpr(ctx, *e.value);
				setReferencedInConstantOrExpr(ctx, e.then);
			},
			[](const ConcreteExpr::LocalRef) {},
			[&](const ConcreteExpr::Match e) {
				setReferencedInExpr(ctx, *e.matchedValue);
				for (const ConcreteExpr::Match::Case kase : e.cases)
					setReferencedInConstantOrExpr(ctx, kase.then);
			},
			[](const ConcreteExpr::ParamRef) {},
			[&](const ConcreteExpr::RecordFieldAccess e) {
				setReferencedInConstantOrExpr(ctx, e.target);
			},
			[&](const ConcreteExpr::RecordFieldSet e) {
				setReferencedInExpr(ctx, *e.target);
				setReferencedInConstantOrExpr(ctx, e.value);
			},
			[&](const ConcreteExpr::Seq e) {
				setReferencedInExpr(ctx, *e.first);
				setReferencedInConstantOrExpr(ctx, e.then);
			},
			[&](const ConcreteExpr::SpecialUnary e) {
				setReferencedInExpr(ctx, *e.arg);
			},
			[&](const ConcreteExpr::SpecialBinary e) {
				setReferencedInConstantOrExpr(ctx, e.left);
				setReferencedInConstantOrExpr(ctx, e.right);
			});
	}

	void setReferencedInFun(SetReferencedCtx* ctx, const ConcreteFun* f) {
		if (has(f->closureParam))
			setReferencedInType(ctx, force(f->closureParam).type);
		setReferencedInSig(ctx, f->sig);
		f->body().match(
			[](const ConcreteFunBody::Bogus) {
				unreachable<void>();
			},
			[](const ConcreteFunBody::Builtin) {},
			[](const ConcreteFunBody::Extern) {},
			[&](const Constant* c) {
				addConstant(ctx, c);
			},
			[&](const ConcreteFunExprBody e) {
				for (const ConcreteLocal* l : e.allLocals) {
					setReferencedInType(ctx, l->type);
				}
				setReferencedInExpr(ctx, *e.expr);
			});
	}

	void setReferencedInConstant(SetReferencedCtx* ctx, const Constant* c) {
		c->kind.match(
			[](const Bool) {},
			[](const char) {},
			[&](const ConstantKind::FunPtr f) {
				addFun(ctx, f.fun);
			},
			[](const Int16) {},
			[](const Int32) {},
			[](const Int64) {},
			// When we *call* this or convert to fn, then we'll mark its fun as referenced
			[](const ConstantKind::Lambda) {},
			[](const Nat16) {},
			[](const Nat32) {},
			[](const Nat64) {},
			[](const ConstantKind::Null) {},
			[&](const ConstantKind::Ptr p) {
				addArrayBacking(ctx, p.array);
			},
			[&](const ConstantKind::Record r) {
				for (const Constant* arg : r.args)
					addConstant(ctx, arg);
			},
			[&](const ConstantKind::Union u) {
				addConstant(ctx, u.member);
			},
			[](const ConstantKind::Void) {});
	}

	void setReferencedInConstantArrayBacking(SetReferencedCtx* ctx, const ConstantArrayBacking* a) {
		for (const Constant* c : a->elements)
			addConstant(ctx, c);
	}
}

const ConcreteProgram getReferencedOnly(
	Arena* arena,
	const ConcreteFun* rtMain,
	const ConcreteFun* userMain,
	const ConcreteStruct* ctxStruct
) {
	SetReferencedCtx ctx { arena };
	addFun(&ctx, rtMain);
	addFun(&ctx, userMain);

	// For each of these -- we are collecting an array of all of them, and advancing an index of the ones we've scanned.
	// When the index reaches the array's length we are done (at least until another is pushed)
	size_t nextStructIndexToScan = 0;
	size_t nextConstantIndexToScan = 0;
	size_t nextFunIndexToScan = 0;
	size_t nextArrayBackingToScan = 0;

	for (;;) {
		if (nextStructIndexToScan < mutArrSize(&ctx.allReferencedStructs))
			setReferencedInStruct(&ctx, mutArrAt(&ctx.allReferencedStructs, nextStructIndexToScan++));
		else if (nextConstantIndexToScan < mutArrSize(&ctx.allReferencedConstants))
			setReferencedInConstant(&ctx, mutArrAt(&ctx.allReferencedConstants, nextConstantIndexToScan++));
		else if (nextFunIndexToScan < mutArrSize(&ctx.allReferencedFuns))
			setReferencedInFun(&ctx, mutArrAt(&ctx.allReferencedFuns, nextFunIndexToScan++));
		else if (nextArrayBackingToScan < mutArrSize(&ctx.allArrayBackings))
			setReferencedInConstantArrayBacking(&ctx, mutArrAt(&ctx.allArrayBackings, nextArrayBackingToScan++));
		else
			break;
	}

	return ConcreteProgram{
		freeze(&ctx.allReferencedStructs),
		freeze(&ctx.allReferencedConstants),
		freeze(&ctx.allReferencedFuns),
		freeze(&ctx.allArrayBackings),
		rtMain,
		userMain,
		ctxStruct};
}
