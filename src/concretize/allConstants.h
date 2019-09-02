#pragma once

#include "../concreteModel.h"

struct AllConstants;

AllConstants* newAllConstants(Arena* arena);

const Constant* constantArr(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteStruct* arrayType,
	const ConcreteType elementType,
	const Arr<const Constant*> elements);
const Constant* constantBool(Arena* arena, AllConstants* allConstants, const ConcreteType boolType, const Bool value);
const Constant* constantChar(Arena* arena, AllConstants* allConstants, const ConcreteType charType, const char value);
const Constant* constantFunPtr(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType funPtrType,
	const ConcreteFun* fun);
const Constant* constantInt64(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType int64Type,
	const Int64 value);
const Constant* constantLambda(Arena* arena, AllConstants* allConstants, const KnownLambdaBody* klb);
const Constant* constantNat64(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType nat64Type,
	const Nat64 value);
const Constant* constantNull(Arena* arena, AllConstants* allConstants, const ConcreteType pointerType);
const Constant* constantPtr(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType pointerType,
	const Constant* array,
	const size_t index);
const Constant* constantRecord(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType recordType,
	const Arr<const Constant*> args);
const Constant* constantUnion(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType unionType,
	const size_t memberIndex,
	const Constant* member);
const Constant* constantVoid(Arena* arena, AllConstants* allConstants, const ConcreteType voidType);
