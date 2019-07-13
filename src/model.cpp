#include "./model.h"

#include "./util/arrUtil.h"

const Bool Type::containsUnresolvedTypeParams() const {
	return match(
		[](const Type::Bogus) {
			return True;
		},
		[](const TypeParam*) {
			return True;
		},
		[](const StructInst* i) {
			return exists(i->typeArgs, [](const Type t) {
				return t.containsUnresolvedTypeParams();
			});
		});
}

const Bool Type::typeEquals(const Type other) const {
	return match(
		[&](const Type::Bogus) {
			return other.isBogus();
		},
		[&](const TypeParam* p) {
			return _and(other.isTypeParam(), ptrEquals(p, other.asTypeParam()));
		},
		[&](const StructInst* s) {
			return _and(other.isStructInst(), ptrEquals(s, other.asStructInst()));
		});
}

Purity Type::purity() const {
	return match(
		[](const Type::Bogus) {
			return Purity::data;
		},
		[](const TypeParam*) {
			return Purity::data;
		},
		[](const StructInst* s) {
			return s->purity;
		});
}

const Opt<const CommonTypes::LambdaInfo> CommonTypes::getFunStructInfo(const StructDecl* s) const {
	for (const StructDecl* p : funTypes)
		if (ptrEquals(p, s))
			return some<const CommonTypes::LambdaInfo>(LambdaInfo{False, s});

	for (const size_t i : Range{sendFunTypes.size})
		if (ptrEquals(at(sendFunTypes, i), s))
			return some<const CommonTypes::LambdaInfo>(LambdaInfo{True, at(funTypes, i)});

	return none<const CommonTypes::LambdaInfo>();
}

const Bool Expr::typeIsBogus(Arena* arena) const {
	return match(
		[](const Expr::Bogus) {
			return True;
		},
		[](const Expr::Call e) {
			return e.called.returnType().isBogus();
		},
		[](const Expr::ClosureFieldRef e) {
			return e.field->type.isBogus();
		},
		[](const Expr::Cond) {
			return todo<const Bool>("typeIsBogus cond");
		},
		[](const Expr::CreateArr) {
			return False;
		},
		[](const Expr::CreateRecord) {
			return False;
		},
		[](const Expr::FunAsLambda) {
			return False;
		},
		[](const Expr::IfaceImplFieldRef) {
			return todo<const Bool>("typeIsBogus ifaceimplfieldref");
		},
		[](const Expr::ImplicitConvertToUnion) {
			return False;
		},
		[](const Expr::Lambda) {
			return False;
		},
		[&](const Expr::Let e) {
			return e.then->typeIsBogus(arena);
		},
		[](const Expr::LocalRef e) {
			return e.local->type.isBogus();
		},
		[](const Expr::Match) {
			return todo<const Bool>("typeIsBogus match");
		},
		[](const Expr::MessageSend e) {
			return e.getType().isBogus();
		},
		[](const Expr::NewIfaceImpl) {
			return todo<const Bool>("typeIsBogus newIfaceImpl");
		},
		[](const Expr::ParamRef e) {
			return e.param->type.isBogus();
		},
		[&](const Expr::Seq e) {
			return e.then->typeIsBogus(arena);
		},
		[](const Expr::StringLiteral) {
			return False;
		},
		[](const Expr::StructFieldAccess e) {
			return e.accessedFieldType().isBogus();
		},
		[](const Expr::StructFieldSet) {
			return False; // always void
		});
}

const Type Expr::getType(Arena* arena, const CommonTypes& commonTypes) const {
	return match(
		[](const Expr::Bogus) {
			return Type{Type::Bogus{}};
		},
		[](const Expr::Call e) {
			return e.called.returnType();
		},
		[](const Expr::ClosureFieldRef e) {
			return e.field->type;
		},
		[](const Expr::Cond) {
			return todo<const Type>("getType cond");
		},
		[](const Expr::CreateArr e) {
			return Type(e.arrType);
		},
		[](const Expr::CreateRecord e) {
			return Type(e.structInst);
		},
		[](const Expr::FunAsLambda) {
			return todo<const Type>("getType funAsLambda");
		},
		[](const Expr::IfaceImplFieldRef) {
			return todo<const Type>("getType ifaceImplFieldRef");
		},
		[](const Expr::ImplicitConvertToUnion e) {
			return Type(e.unionType);
		},
		[](const Expr::Lambda e) {
			return Type(e.type);
		},
		[&](const Expr::Let e) {
			return e.then->getType(arena, commonTypes);
		},
		[](const Expr::LocalRef e) {
			return e.local->type;
		},
		[](const Expr::Match) {
			return todo<const Type>("getType match");
		},
		[](const Expr::MessageSend e) {
			return e.getType();
		},
		[](const Expr::NewIfaceImpl) {
			return todo<const Type>("getType newifaceimpl");
		},
		[](const Expr::ParamRef e) {
			return e.param->type;
		},
		[&](const Expr::Seq e) {
			return e.then->getType(arena, commonTypes);
		},
		[&](const Expr::StringLiteral) {
			return Type(commonTypes.str);
		},
		[](const Expr::StructFieldAccess e) {
			return e.accessedFieldType();
		},
		[&](const Expr::StructFieldSet) {
			return Type{commonTypes._void};
		});
}

void writeStructInst(Writer& writer, const StructInst* s) {
	writeSym(writer, s->decl->name);
	if (!isEmpty(s->typeArgs)) {
		Cell<const Bool> first { True };
		for (const Type t : s->typeArgs) {
			writeChar(writer, cellGet(&first) ? '<' : ' ');
			writeType(writer, t);
			cellSet<const Bool>(&first, False);
		}
		writeChar(writer, '>');
	}
}

void writeType(Writer& writer, const Type type) {
	type.match(
		[&](const Type::Bogus) {
			writeStatic(writer, "<<bogus>>");
		},
		[&](const TypeParam* p) {
			writeChar(writer, '?');
			writeSym(writer, p->name);
		},
		[&](const StructInst* s) {
			writeStructInst(writer, s);
		});
}

namespace {
	const Sexpr structInstToSexpr(Arena* arena, const StructInst* si) {
		return Sexpr{SexprRecord{
			shortSymAlphaLiteral("structinst"),
			arrLiteral<const Sexpr>(
				arena,
				Sexpr{si->decl->name},
				arrToSexpr<const Type>(arena, si->typeArgs, [&](const Type t) {
					return typeToSexpr(arena, t);
				}))
		}};
	}
}

const Sexpr typeToSexpr(Arena* arena, const Type type) {
	unused(arena);
	return type.match(
		[&](const Type::Bogus) {
			return Sexpr{shortSymAlphaLiteral("bogus")};
		},
		[&](const TypeParam* p) {
			return Sexpr{p->name};
		},
		[&](const StructInst*) {
			return todo<const Sexpr>("!!!!typetosexpr");
		});
}

namespace {
	const Sexpr calledToSexpr(Arena* arena, const Called c) {
		unused(arena);
		return c.match(
			[&](const FunInst*) {
				return todo<const Sexpr>("funinst to sexpr");
			},
			[&](const SpecSig) {
				return todo<const Sexpr>("specsig to sexpr");
			});
	}
}

const Sexpr exprToSexpr(Arena* arena, const Expr expr) {
	return expr.match(
		[&](const Expr::Bogus) {
			return Sexpr{strLiteral("bogus")};
		},
		[&](const Expr::Call e) {
			return Sexpr{SexprRecord{
				shortSymAlphaLiteral("call"),
				arrLiteral<const Sexpr>(arena, {
					calledToSexpr(arena, e.called),
					arrToSexpr<const Expr>(arena, e.args, [&](const Expr arg) {
						return exprToSexpr(arena, arg);
					})
				})}};
		},
		[&](const Expr::ClosureFieldRef) {
			return todo<const Sexpr>("closurefieldref");
		},
		[&](const Expr::Cond) {
			return todo<const Sexpr>("cond");
		},
		[&](const Expr::CreateArr) {
			return todo<const Sexpr>("createarr");
		},
		[&](const Expr::CreateRecord e) {
			return Sexpr{SexprRecord{
				shortSymAlphaLiteral("record"),
				arrLiteral<const Sexpr>(arena, {
					structInstToSexpr(arena, e.structInst),
					arrToSexpr<const Expr>(arena, e.args, [&](const Expr arg) {
						return exprToSexpr(arena, arg);
					})})}};
		},
		[&](const Expr::FunAsLambda) {
			return todo<const Sexpr>("funaslambda");
		},
		[&](const Expr::IfaceImplFieldRef) {
			return todo<const Sexpr>("ifaceimplfieldref");
		},
		[&](const Expr::ImplicitConvertToUnion) {
			return todo<const Sexpr>("implicitconverttounion");
		},
		[&](const Expr::Lambda) {
			return todo<const Sexpr>("lambda");
		},
		[&](const Expr::Let) {
			return todo<const Sexpr>("let");
		},
		[&](const Expr::LocalRef) {
			return todo<const Sexpr>("localref");
		},
		[&](const Expr::Match) {
			return todo<const Sexpr>("match");
		},
		[&](const Expr::MessageSend) {
			return todo<const Sexpr>("messagesend");
		},
		[&](const Expr::NewIfaceImpl) {
			return todo<const Sexpr>("newifaceimpl");
		},
		[&](const Expr::ParamRef) {
			return todo<const Sexpr>("paramref");
		},
		[&](const Expr::Seq) {
			return todo<const Sexpr>("seq");
		},
		[&](const Expr::StringLiteral) {
			return todo<const Sexpr>("stringliteral");
		},
		[&](const Expr::StructFieldAccess) {
			return todo<const Sexpr>("structfieldaccess");
		},
		[](const Expr::StructFieldSet) {
			return todo<const Sexpr>("structfieldset");
		});
}

void writeExpr(Writer& writer, const Expr expr) {
	Arena arena {};
	writeSexpr(writer, exprToSexpr(&arena, expr));
}
