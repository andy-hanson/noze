#pragma once

#include "../concreteModel.h"

struct ConstantsForRecord;
struct ConstantsForUnion;

struct AllConstants {
private:
	// element type * args -> id
	// When we're done, we'll invert this dict so they can be emitted.
	size_t nextArrId;
	MutDict<const ConstantArrKey, const Constant*, compareConstantArrKey> arrays;
	// Dict from array to an array of each pointer inside it
	MutDict<const Constant*, Arr<const Constant*>, comparePtr<const Constant>> arrayToPtrs;
	size_t nextPtrId;
	Late<const Constant*> _false;
	Late<const Constant*> _true;
	Late<const Constant*> __void;
	MutDict<const char, const Constant*, comparePrimitive<const char>> chars;
	MutDict<const Int64, const Constant*, comparePrimitive<const Int64>> int64s;
	MutDict<const Nat64, const Constant*, comparePrimitive<const Nat64>> nat64s;
	size_t nextLambdaId;
	// No dict for lambdas?
	MutDict<const ConcreteType, ConstantsForRecord*, compareConcreteType> records;
	MutDict<const ConcreteStruct*, ConstantsForUnion*, comparePtr<const ConcreteStruct>> unions;
	MutDict<const ConcreteStruct*, const Constant*, comparePtr<const ConcreteStruct>> nulls;
	ArrBuilder<const Constant*> all;

	const Constant* _nuConstant(Arena& arena, const ConcreteType type, const ConstantKind kind, const size_t id);
public:
	AllConstants(const AllConstants&) = delete;
	inline AllConstants() {}

	const Constant* arr(Arena& arena, const ConcreteStruct* arrayType, const ConcreteType elementType, const Arr<const Constant*> elements);

	const Constant* ptr(Arena& arena, const ConcreteType pointerType, const Constant* array, const size_t index);

	const Constant* _null(Arena& arena, const ConcreteType pointerType);

	const Constant* _bool(Arena& arena, const ConcreteType boolType, const Bool value);
	const Constant* _void(Arena& arena, const ConcreteType voidType);
	const Constant* _char(Arena& arena, const ConcreteType charType, const char value);
	const Constant* int64(Arena& arena, const ConcreteType int64Type, const Int64 value);
	const Constant* nat64(Arena& arena, const ConcreteType nat64Type, const Nat64 value);
	const Constant* funPtr(Arena& arena, const ConcreteType funPtrType, const ConcreteFun* fun);
	const Constant* lambda(Arena& arena, const KnownLambdaBody* klb);
	const Constant* record(Arena& arena, const ConcreteType recordType, const Arr<const Constant*> args);
	const Constant* _union(Arena& arena, const ConcreteType unionType, const size_t memberIndex, const Constant* member);
};
