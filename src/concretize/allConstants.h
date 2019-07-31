#pragma once

#include "../concreteModel.h"

struct ConstantsForRecord;
struct ConstantsForUnion;

struct AllConstants {
	// element type * args -> id
	// When we're done, we'll invert this dict so they can be emitted.
	size_t nextArrId = 0;
	MutDict<const ConstantArrKey, const Constant*, compareConstantArrKey> arrays {};
	// Dict from array to an array of each pointer inside it
	MutDict<const Constant*, Arr<const Constant*>, comparePtr<const Constant>> arrayToPtrs {};
	size_t nextPtrId = 0;
	Late<const Constant*> _false {};
	Late<const Constant*> _true {};
	Late<const Constant*> __void {};
	MutDict<const char, const Constant*, comparePrimitive<const char>> chars {};
	MutDict<const Int64, const Constant*, comparePrimitive<const Int64>> int64s {};
	MutDict<const Nat64, const Constant*, comparePrimitive<const Nat64>> nat64s {};
	size_t nextLambdaId = 0;
	// No dict for lambdas?
	MutDict<const ConcreteType, ConstantsForRecord*, compareConcreteType> records {};
	MutDict<const ConcreteStruct*, ConstantsForUnion*, comparePtr<const ConcreteStruct>> unions {};
	MutDict<const ConcreteStruct*, const Constant*, comparePtr<const ConcreteStruct>> nulls {};
	ArrBuilder<const Constant*> all {};

	AllConstants(const AllConstants&) = delete;
	inline AllConstants() {}
};

const Constant* constantArr(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteStruct* arrayType,
	const ConcreteType elementType,
	const Arr<const Constant*> elements);
const Constant* constantPtr(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType pointerType,
	const Constant* array,
	const size_t index);
const Constant* constantNull(Arena* arena, AllConstants* allConstants, const ConcreteType pointerType);
const Constant* constantBool(Arena* arena, AllConstants* allConstants, const ConcreteType boolType, const Bool value);
const Constant* constantVoid(Arena* arena, AllConstants* allConstants, const ConcreteType voidType);
const Constant* constantChar(Arena* arena, AllConstants* allConstants, const ConcreteType charType, const char value);
const Constant* constantInt64(Arena* arena, AllConstants* allConstants, const ConcreteType int64Type, const Int64 value);
const Constant* constantNat64(Arena* arena, AllConstants* allConstants, const ConcreteType nat64Type, const Nat64 value);
const Constant* constantFunPtr(Arena* arena, AllConstants* allConstants, const ConcreteType funPtrType, const ConcreteFun* fun);
const Constant* constantLambda(Arena* arena, AllConstants* allConstants, const KnownLambdaBody* klb);
const Constant* constantRecord(Arena* arena, AllConstants* allConstants, const ConcreteType recordType, const Arr<const Constant*> args);
const Constant* constantUnion(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType unionType,
	const size_t memberIndex,
	const Constant* member);
