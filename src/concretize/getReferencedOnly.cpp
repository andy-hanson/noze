#include "./getReferencedOnly.h"

namespace {
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
			if (!s->isStructReferenced) {
				s->isStructReferenced = true;
				allReferencedStructs.push(arena, s);
			}
		}

		void addConstant(const Constant* c) {
			if (!c->isConstantReferenced) {
				c->isConstantReferenced = true;
				allReferencedConstants.push(arena, c);
			}
		}

		void addFun(const ConcreteFun* f) {
			if (!f->isFunReferenced) {
				f->isFunReferenced = true;
				allReferencedFuns.push(arena, f);
			}
		}

		void addNewIfaceImpl(ConcreteExpr::NewIfaceImpl impl) {
			// These don't have isReferenced, but we'll only walk the function containing them once.
			allReferencedNewIfaceImpls.push(arena, impl);
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
		assert(s->isStructReferenced);
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
			[&](const ConcreteExpr::CallConcreteFun e) {
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
				setReferencedInType(ctx, e.type);
				if (e.alloc.has())
					ctx.addFun(e.alloc.force());
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
				setReferencedInExpr(ctx, *e.inner);
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
			});
	}

	void setReferencedInFun(SetReferencedCtx& ctx, const ConcreteFun* f) {
		assert(f->isFunReferenced);
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
		assert(c->isConstantReferenced);
		c->kind.match(
			[&](const ConstantKind::Array a) {
				ctx.addStruct(a.arrayType);
				for (const Constant* element : a.elements())
					ctx.addConstant(element);
			},
			[](const bool) {},
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

const ConcreteProgram getReferencedOnly(Arena& arena, const ConcreteFun* mainFun) {
	SetReferencedCtx ctx { arena };
	ctx.addFun(mainFun);
	while (true) {
		if (ctx.nextStructIndexToScan < ctx.allReferencedStructs.size())
			setReferencedInStruct(ctx, ctx.allReferencedStructs[ctx.nextStructIndexToScan++]);
		else if (ctx.nextConstantIndexToScan < ctx.allReferencedConstants.size())
			setReferencedInConstant(ctx, ctx.allReferencedConstants[ctx.nextConstantIndexToScan++]);
		else if (ctx.nextFunIndexToScan < ctx.allReferencedFuns.size())
			setReferencedInFun(ctx, ctx.allReferencedFuns[ctx.nextFunIndexToScan++]);
		else if (ctx.nextNewIfaceImplIndexToScan < ctx.allReferencedNewIfaceImpls.size())
			setReferencedInNewIfaceImpl(ctx, ctx.allReferencedNewIfaceImpls[ctx.nextNewIfaceImplIndexToScan++]);
		else
			break;
	}

	return ConcreteProgram{
		ctx.allReferencedStructs.freeze(),
		ctx.allReferencedConstants.freeze(),
		ctx.allReferencedFuns.freeze(),
		ctx.allReferencedNewIfaceImpls.freeze()};
}
