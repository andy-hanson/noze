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
	MutDict<const Constant*, Arr<const Constant*>, comparePointer<const Constant>> arrayToPtrs;
	size_t nextPtrId;
	const Constant* _false;
	const Constant* _true;
	MutDict<const char, const Constant*, compareChar> chars;
	MutDict<const Int64, const Constant*, compareInt64> int64s;
	MutDict<const Nat64, const Constant*, compareNat64> nat64s;
	size_t nextLambdaId;
	// No dict for lambdas?
	MutDict<const ConcreteType, ConstantsForRecord*, compareConcreteType> records;
	MutDict<const ConcreteStruct*, ConstantsForUnion*, comparePointer<const ConcreteStruct>> unions;
	ArrBuilder<const Constant*> all;

	const Constant* _nuConstant(Arena& arena, const ConstantKind kind, const size_t id);
public:
	const Constant* _void;
	const Constant* _null;

	AllConstants(const AllConstants&) = delete;
	AllConstants(Arena& arena);

	const Constant* arr(Arena& arena, const ConcreteStruct* arrayType, const ConcreteType elementType, const Arr<const Constant*> elements);

	const Constant* ptr(Arena& arena, const Constant* array, const size_t index);

	inline const Constant* _bool(const bool value) {
		return value ? _true : _false;
	}

	const Constant* _char(Arena& arena, const char value);
	const Constant* int64(Arena& arena, const Int64 value);
	const Constant* nat64(Arena& arena, const Nat64 value);
	const Constant* funPtr(Arena& arena, const ConcreteFun* fun);
	const Constant* lambda(Arena& arena, const KnownLambdaBody* klb);
	const Constant* record(Arena& arena, const ConcreteType type, const Arr<const Constant*> args);
	const Constant* _union(Arena& arena, const ConcreteType unionType, const size_t memberIndex, const Constant* member);
};
