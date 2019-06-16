#include "./model.h"

bool Type::containsUnresolvedTypeParams() const {
	return match(
		[](const Type::Bogus) {
			return true;
		},
		[](const TypeParam*) {
			return true;
		},
		[](const StructInst* i) {
			return exists(i->typeArgs, [](const Type t) {
				return t.containsUnresolvedTypeParams();
			});
		});
}

bool Type::typeEquals(const Type other) const {
	return match(
		[&](const Type::Bogus) {
			return other.isBogus();
		},
		[&](const TypeParam* p) {
			return other.isTypeParam() && ptrEquals(p, other.asTypeParam());
		},
		[&](const StructInst* s) {
			return other.isStructInst() && ptrEquals(s, other.asStructInst());
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
			return some<const CommonTypes::LambdaInfo>(LambdaInfo{false, s});

	for (const size_t i : Range{remoteFunTypes.size})
		if (ptrEquals(remoteFunTypes[i], s))
			return some<const CommonTypes::LambdaInfo>(LambdaInfo{true, funTypes[i]});

	return none<const CommonTypes::LambdaInfo>();
}

bool Expr::typeIsBogus(Arena& arena) const {
	return match(
		[](const Expr::Bogus) {
			return true;
		},
		[](const Expr::Call e) {
			return e.concreteReturnType.isBogus();
		},
		[](const Expr::ClosureFieldRef e) {
			return e.field->type.isBogus();
		},
		[](const Expr::Cond) {
			return todo<bool>("typeIsBogus cond");
		},
		[](const Expr::CreateArr) {
			return false;
		},
		[](const Expr::CreateRecord) {
			return false;
		},
		[](const Expr::FunAsLambda) {
			return false;
		},
		[](const Expr::IfaceImplFieldRef) {
			return todo<bool>("typeIsBogus ifaceimplfieldref");
		},
		[](const Expr::ImplicitConvertToUnion) {
			return todo<bool>("typeIsBogus implicitConvertToUnion");
		},
		[](const Expr::Lambda) {
			return false;
		},
		[&](const Expr::Let e) {
			return e.then->typeIsBogus(arena);
		},
		[](const Expr::LocalRef e) {
			return e.local->type.isBogus();
		},
		[](const Expr::Match) {
			return todo<bool>("typeIsBogus match");
		},
		[](const Expr::MessageSend) {
			return todo<bool>("typeIsBogus messageSend");
		},
		[](const Expr::NewIfaceImpl) {
			return todo<bool>("typeIsBogus newIfaceImpl");
		},
		[](const Expr::ParamRef e) {
			return e.param->type.isBogus();
		},
		[&](const Expr::Seq e) {
			return e.then->typeIsBogus(arena);
		},
		[](const Expr::StringLiteral) {
			return false;
		},
		[](const Expr::StructFieldAccess e) {
			return e.accessedFieldType().isBogus();
		});
}

const Type Expr::getType(Arena& arena, const CommonTypes& commonTypes) const {
	return match(
		[](const Expr::Bogus) {
			return Type{Type::Bogus{}};
		},
		[](const Expr::Call e) {
			return e.concreteReturnType;
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
		[](const Expr::MessageSend) {
			return todo<const Type>("getType messageSend");
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
		});
}
