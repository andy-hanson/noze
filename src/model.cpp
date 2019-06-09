#include "./model.h"

bool Type::containsUnresolvedTypeParams() const {
	return match(
		/*bogus*/ []() { return true; },
		[](__attribute__((unused)) const TypeParam* _) { return true; },
		[](const StructInst* i) {
			return exists(i->typeArgs, [](const Type t) {
				return t.containsUnresolvedTypeParams();
			});
		});
}

bool Type::typeEquals(const Type other) const {
	return match(
		/*bogus*/ [=]() {
			return other.isBogus();
		},
		[=](const TypeParam* p) {
			return other.isTypeParam() && ptrEquals(p, other.asTypeParam());
		},
		[=](const StructInst* s) {
			return other.isStructInst() && ptrEquals(s, other.asStructInst());
		});
}

Purity Type::purity() const {
	return match(
		/*bogus*/ []() {
			return Purity::data;
		},
		[](__attribute__((unused)) const TypeParam* p) {
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

	for (size_t i = 0; i < remoteFunTypes.size(); i++)
		if (ptrEquals(remoteFunTypes[i], s))
			return some<const CommonTypes::LambdaInfo>(LambdaInfo{true, funTypes[i]});

	return none<const CommonTypes::LambdaInfo>();
}

bool Expr::typeIsBogus(Arena& arena) const {
	return match(
		/*bogus*/ []() {
			return true;
		},
		[](const Expr::Call e) {
			return e.concreteReturnType.isBogus();
		},
		[](const Expr::ClosureFieldRef e) {
			return e.field->type.isBogus();
		},
		[](__attribute__((unused)) const Expr::Cond cond) {
			return todo<bool>("typeIsBogus cond");
		},
		[](__attribute__((unused)) const Expr::CreateArr _) {
			return false;
		},
		[](__attribute__((unused)) const Expr::CreateRecord _) {
			return false;
		},
		[](__attribute__((unused)) const Expr::FunAsLambda _) {
			return false;
		},
		[](__attribute__((unused)) const Expr::IfaceImplFieldRef _) {
			return todo<bool>("typeIsBogus ifaceimplfieldref");
		},
		[](__attribute__((unused)) const Expr::ImplicitConvertToUnion _) {
			return todo<bool>("typeIsBogus implicitConvertToUnion");
		},
		[](__attribute__((unused)) const Expr::Lambda _) {
			return false;
		},
		[&](const Expr::Let e) {
			return e.then->typeIsBogus(arena);
		},
		[](const Expr::LocalRef e) {
			return e.local->type.isBogus();
		},
		[](__attribute__((unused)) const Expr::Match _) {
			return todo<bool>("typeIsBogus match");
		},
		[](__attribute__((unused)) const Expr::MessageSend _) {
			return todo<bool>("typeIsBogus messageSend");
		},
		[](__attribute__((unused)) const Expr::NewIfaceImpl _) {
			return todo<bool>("typeIsBogus newIfaceImpl");
		},
		[](const Expr::ParamRef e) {
			return e.param->type.isBogus();
		},
		[&](const Expr::Seq e) {
			return e.then->typeIsBogus(arena);
		},
		[](__attribute__((unused)) const Expr::StringLiteral _) {
			return false;
		},
		[](const Expr::StructFieldAccess e) {
			return e.accessedFieldType().isBogus();
		});
}

const Type Expr::getType(Arena& arena, const CommonTypes& commonTypes) const {
	return match(
		/*bogus*/ []() { return Type::bogus(); },
		[](const Expr::Call e) {
			return e.concreteReturnType;
		},
		[](const Expr::ClosureFieldRef e) {
			return e.field->type;
		},
		[](__attribute__((unused)) const Expr::Cond e) {
			return todo<const Type>("getType cond");
		},
		[](const Expr::CreateArr e) {
			return Type(e.arrType);
		},
		[](const Expr::CreateRecord e) {
			return Type(e.structInst);
		},
		[](__attribute__((unused)) const Expr::FunAsLambda _) {
			return todo<const Type>("getType funAsLambda");
		},
		[](__attribute__((unused)) const Expr::IfaceImplFieldRef _) {
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
		[](__attribute__((unused)) const Expr::Match _) {
			return todo<const Type>("getType match");
		},
		[](__attribute__((unused)) const Expr::MessageSend _) {
			return todo<const Type>("getType messageSend");
		},
		[](__attribute__((unused)) const Expr::NewIfaceImpl _) {
			return todo<const Type>("getType newifaceimpl");
		},
		[](const Expr::ParamRef e) {
			return e.param->type;
		},
		[&](const Expr::Seq e) {
			return e.then->getType(arena, commonTypes);
		},
		[&](__attribute__((unused)) const Expr::StringLiteral _) {
			return Type(commonTypes.str);
		},
		[](const Expr::StructFieldAccess e) {
			return e.accessedFieldType();
		});
}
