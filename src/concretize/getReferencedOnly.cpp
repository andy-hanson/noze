#include "./getReferencedOnly.h"

#include "../util/arrUtil.h"

namespace {
	template <typename T>
	void pushIfNotContained(Arena& arena, MutArr<const T*>& all, const T* t) {
		if (!contains<const T*, ptrEquals<const T>>(tempAsArr(all), t))
			push(arena, all, t);
	}

	struct SetReferencedCtx {
		Arena& arena;

		// For each of these -- we are collecting an array of all of them, and advancing an index of the ones we've scanned.
		// When the index reaches the array's length we are done (at least until another is pushed)

		MutArr<const ConcreteStruct*> allReferencedStructs {};
		size_t nextStructIndexToScan = 0;

		MutArr<const Constant*> allReferencedConstants {};
		size_t nextConstantIndexToScan = 0;

		MutArr<const ConcreteFun*> allReferencedFuns {};
		size_t nextFunIndexToScan = 0;

		// TODO: these should just be several ConcreteFuns
		MutArr<const ConcreteExpr::NewIfaceImpl> allReferencedNewIfaceImpls {};
		size_t nextNewIfaceImplIndexToScan = 0;

		SetReferencedCtx(const SetReferencedCtx&) = delete;

		void addStruct(const ConcreteStruct* s) {
			pushIfNotContained(arena, allReferencedStructs, s);
		}

		void addConstant(const Constant* c) {
			pushIfNotContained(arena, allReferencedConstants, c);
		}

		void addFun(const ConcreteFun* f) {
			pushIfNotContained(arena, allReferencedFuns, f);
		}

		void addNewIfaceImpl(const ConcreteExpr::NewIfaceImpl impl) {
			// These don't have isReferenced, but we'll only walk the function containing them once.
			push<const ConcreteExpr::NewIfaceImpl>(arena, allReferencedNewIfaceImpls, impl);
		}
	};

	void setReferencedInType(SetReferencedCtx& ctx, const ConcreteType t) {
		ctx.addStruct(t.strukt);
	}

	void setReferencedInSig(SetReferencedCtx& ctx, const ConcreteSig sig) {
		setReferencedInType(ctx, sig.returnType);
		for (const ConcreteParam p : sig.params)
			setReferencedInType(ctx, p.type);
	}

	void setReferencedInStruct(SetReferencedCtx& ctx, const ConcreteStruct* s) {
		return s->body().match(
			[&](const ConcreteStructBody::Builtin b) {
				for (const ConcreteType s : b.typeArgs)
					setReferencedInType(ctx, s);
			},
			[&](const ConcreteStructBody::Fields f) {
				for (const ConcreteField field : f.fields)
					setReferencedInType(ctx, field.type);
			},
			[&](const ConcreteStructBody::Union u) {
				for (const ConcreteType member : u.members)
					setReferencedInType(ctx, member);
			},
			[&](const ConcreteStructBody::Iface i) {
				for (const ConcreteSig message : i.messages)
					setReferencedInSig(ctx, message);
			});
	}

	void setReferencedInExpr(SetReferencedCtx& ctx, const ConcreteExpr ce);

	void setReferencedInConstantOrExpr(SetReferencedCtx& ctx, const ConstantOrExpr ce) {
		ce.match(
			[&](const Constant* c) {
				ctx.addConstant(c);
			},
			[&](const ConcreteExpr* ce) {
				setReferencedInExpr(ctx, *ce);
			});
	}

	void setReferencedInConstantOrExprs(SetReferencedCtx& ctx, const Arr<const ConstantOrExpr> args) {
		for (const ConstantOrExpr arg : args)
			setReferencedInConstantOrExpr(ctx, arg);
	}

	void setReferencedInExpr(SetReferencedCtx& ctx, const ConcreteExpr ce) {
		ce.match(
			[](const ConcreteExpr::Bogus) {
				unreachable<void>();
			},
			[&](const ConcreteExpr::Alloc e) {
				ctx.addFun(e.alloc);
				setReferencedInExpr(ctx, *e.inner);
			},
			[&](const ConcreteExpr::Call e) {
				ctx.addFun(e.called);
				setReferencedInConstantOrExprs(ctx, e.args);
			},
			[&](const ConcreteExpr::Cond e) {
				setReferencedInExpr(ctx, *e.cond);
				setReferencedInConstantOrExpr(ctx, e.then);
				setReferencedInConstantOrExpr(ctx, e.elze);
			},
			[&](const ConcreteExpr::CreateArr e) {
				ctx.addStruct(e.arrType);
				ctx.addFun(e.alloc);
				setReferencedInConstantOrExprs(ctx, e.args);
			},
			[&](const ConcreteExpr::CreateRecord e) {
				const ConcreteType type = ce.typeWithKnownLambdaBody().force();
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
				ctx.addFun(e.fun);
				setReferencedInConstantOrExpr(ctx, e.closure);
			},
			[&](const ConcreteExpr::Let e) {
				setReferencedInExpr(ctx, *e.value);
				setReferencedInConstantOrExpr(ctx, e.then);
			},
			[](const ConcreteExpr::LocalRef) {},
			[&](const ConcreteExpr::Match e) {
				setReferencedInExpr(ctx, *e.matched);
				for (const ConcreteExpr::Match::Case kase : e.cases)
					setReferencedInConstantOrExpr(ctx, kase.then);
			},
			[&](const ConcreteExpr::MessageSend e) {
				setReferencedInConstantOrExpr(ctx, e.target);
				setReferencedInConstantOrExprs(ctx, e.args);
			},
			[&](const ConcreteExpr::NewIfaceImpl e) {
				ctx.addNewIfaceImpl(e);
			},
			[](const ConcreteExpr::ParamRef) {},
			[&](const ConcreteExpr::Seq e) {
				setReferencedInExpr(ctx, *e.first);
				setReferencedInConstantOrExpr(ctx, e.then);
			},
			[&](const ConcreteExpr::SpecialBinary e) {
				setReferencedInConstantOrExpr(ctx, e.left);
				setReferencedInConstantOrExpr(ctx, e.right);
			},
			[&](const ConcreteExpr::StructFieldAccess e) {
				setReferencedInConstantOrExpr(ctx, e.target);
			},
			[&](const ConcreteExpr::StructFieldSet e) {
				setReferencedInExpr(ctx, *e.target);
				setReferencedInConstantOrExpr(ctx, e.value);
			});
	}

	void setReferencedInFun(SetReferencedCtx& ctx, const ConcreteFun* f) {
		if (f->closureParam.has())
			setReferencedInType(ctx, f->closureParam.force().type);
		setReferencedInSig(ctx, f->sig);
		f->body().match(
			[](const ConcreteFunBody::Builtin) {},
			[](const ConcreteFunBody::Extern) {},
			[&](const Constant* c) {
				ctx.addConstant(c);
			},
			[&](const ConcreteExpr* e) {
				setReferencedInExpr(ctx, *e);
			});
	}

	void setReferencedInNewIfaceImpl(SetReferencedCtx& ctx, const ConcreteExpr::NewIfaceImpl impl) {
		ctx.addStruct(impl.iface);
		if (impl.fieldsStruct.has())
			setReferencedInType(ctx, impl.fieldsStruct.force());
		for (const ConstantOrExpr ce : impl.fieldInitializers)
			setReferencedInConstantOrExpr(ctx, ce);
		for (const ConcreteExpr::NewIfaceImpl::MessageImpl m : impl.messageImpls)
			setReferencedInConstantOrExpr(ctx, m.body);
	}

	void setReferencedInConstant(SetReferencedCtx& ctx, const Constant* c) {
		c->kind.match(
			[&](const ConstantKind::Array a) {
				setReferencedInType(ctx, c->type());
				for (const Constant* element : a.elements())
					ctx.addConstant(element);
			},
			[](const Bool) {},
			[](const char) {},
			[&](const ConstantKind::FunPtr f) {
				ctx.addFun(f.fun);
			},
			[](const Int64) {},
			// When we *call* this or convert to fn, then we'll mark its fun as referenced
			[](const ConstantKind::Lambda) {},
			[](const Nat64) {},
			[](const ConstantKind::Null) {},
			[&](const ConstantKind::Ptr p) {
				ctx.addConstant(p.array);
			},
			[&](const ConstantKind::Record r) {
				for (const Constant* arg : r.args)
					ctx.addConstant(arg);
			},
			[&](const ConstantKind::Union u) {
				ctx.addConstant(u.member);
			},
			[](const ConstantKind::Void) {});
	}
}

const ConcreteProgram getReferencedOnly(Arena& arena, const ConcreteFun* mainFun, const ConcreteStruct* ctxStruct) {
	SetReferencedCtx ctx { arena };
	ctx.addFun(mainFun);
	for (;;) {
		if (ctx.nextStructIndexToScan < ctx.allReferencedStructs.size())
			setReferencedInStruct(ctx, at(ctx.allReferencedStructs, ctx.nextStructIndexToScan++));
		else if (ctx.nextConstantIndexToScan < ctx.allReferencedConstants.size())
			setReferencedInConstant(ctx, at(ctx.allReferencedConstants, ctx.nextConstantIndexToScan++));
		else if (ctx.nextFunIndexToScan < ctx.allReferencedFuns.size())
			setReferencedInFun(ctx, at(ctx.allReferencedFuns, ctx.nextFunIndexToScan++));
		else if (ctx.nextNewIfaceImplIndexToScan < ctx.allReferencedNewIfaceImpls.size())
			setReferencedInNewIfaceImpl(ctx, at(ctx.allReferencedNewIfaceImpls, ctx.nextNewIfaceImplIndexToScan++));
		else
			break;
	}

	return ConcreteProgram{
		freeze(ctx.allReferencedStructs),
		freeze(ctx.allReferencedConstants),
		freeze(ctx.allReferencedFuns),
		freeze(ctx.allReferencedNewIfaceImpls),
		ctxStruct};
}
