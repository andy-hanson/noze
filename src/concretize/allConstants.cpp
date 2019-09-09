#include "./allConstants.h"

#include "../util/arrUtil.h"

namespace {
	Comparison compareArrConstants(const Arr<const Constant*> a, const Arr<const Constant*> b) {
		return compareArr<const Constant*, comparePtr<const Constant>>(a, b);
	}

	struct ConstantsForRecord {
		size_t nextId;
		MutDict<const Arr<const Constant*>, const Constant*, compareArrConstants> values;
		ConstantsForRecord(const ConstantsForRecord&) = delete;
	};

	struct ConstantsForUnion {
		size_t nextId;
		// Maps a struct to that struct as a member of the union
		MutDict<const Constant*, const Constant*, comparePtr<const Constant>> values;
		ConstantsForUnion(const ConstantsForUnion&) = delete;
	};
}

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
	MutDict<const Int16, const Constant*, compareInt16> int16s {};
	MutDict<const Int32, const Constant*, compareInt32> int32s {};
	MutDict<const Int64, const Constant*, compareInt64> int64s {};
	MutDict<const Nat16, const Constant*, compareNat16> nat16s {};
	MutDict<const Nat32, const Constant*, compareNat32> nat32s {};
	MutDict<const Nat64, const Constant*, compareNat64> nat64s {};
	MutDict<const ConcreteFun*, const Constant*, comparePtr<const ConcreteFun>> funPtrs {};
	size_t nextLambdaId = 0;
	// No dict for lambdas?
	MutDict<const ConcreteType, ConstantsForRecord*, compareConcreteType> records {};
	MutDict<const ConcreteStruct*, ConstantsForUnion*, comparePtr<const ConcreteStruct>> unions {};
	MutDict<const ConcreteStruct*, const Constant*, comparePtr<const ConcreteStruct>> nulls {};
	ArrBuilder<const Constant*> all {};

	AllConstants(const AllConstants&) = delete;
	inline AllConstants() {}
};

AllConstants* newAllConstants(Arena* arena) {
	return nu<AllConstants>{}(arena);
}

namespace {
	const Constant* newConstant(
		Arena* arena,
		AllConstants* allConstants,
		const ConcreteType type,
		const ConstantKind kind,
		const Nat64 id
	) {
		const Constant* res = nu<const Constant>{}(arena, type, kind, id);
		add(arena, &allConstants->all, res);
		return res;
	}

	void assertIsPointer(const ConcreteType pointerType) {
		assert(strEqLiteral(slice(pointerType.strukt->mangledName, 0, 3), "ptr"));
		assert(!pointerType.isPointer);
	}
}

const Constant* constantArr(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteStruct* arrayType,
	const ConcreteType elementType,
	const Arr<const Constant*> elements
) {
	const ConstantArrKey key = ConstantArrKey{elementType, elements};
	return getOrAdd<const ConstantArrKey, const Constant*, compareConstantArrKey>{}(
		arena,
		&allConstants->arrays,
		key,
		[&]() {
			return newConstant(
				arena,
				allConstants,
				concreteType_fromStruct(arrayType),
				ConstantKind{ConstantKind::Array{key}},
				Nat64{allConstants->nextArrId++});
		});
}

const Constant* constantBool(Arena* arena, AllConstants* allConstants, const ConcreteType boolType, const Bool value)  {
	if (value)
		return lazilySet(&allConstants->_true, [&]() {
			return newConstant(arena, allConstants, boolType, ConstantKind{True}, Nat64{0});
		});
	else
		return lazilySet(&allConstants->_false, [&]() {
			return newConstant(arena, allConstants, boolType, ConstantKind{False}, Nat64{1});
		});
}

const Constant* constantChar(Arena* arena, AllConstants* allConstants, const ConcreteType charType, const char value) {
	return getOrAdd<const char, const Constant*, comparePrimitive<const char>>{}(
		arena,
		&allConstants->chars,
		value,
		[&]() {
			return newConstant(arena, allConstants, charType, ConstantKind{value}, Nat64{static_cast<size_t>(value)});
		});
}

const Constant* constantFunPtr(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType funPtrType,
	const ConcreteFun* fun
) {
	const Constant* res = getOrAdd<const ConcreteFun*, const Constant*, comparePtr<const ConcreteFun>>{}(
		arena,
		&allConstants->funPtrs,
		fun,
		[&]() {
			return newConstant(
				arena,
				allConstants,
				funPtrType,
				ConstantKind{ConstantKind::FunPtr{fun}},
				Nat64{allConstants->nextLambdaId++});
		});
	assert(res->type().eq(funPtrType));
	return res;
}

const Constant* constantInt16(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType int16Type,
	const Int16 value
) {
	return getOrAdd<const Int16, const Constant*, compareInt16>{}(
		arena,
		&allConstants->int16s,
		value,
		[&]() {
			return newConstant(
				arena, allConstants, int16Type, ConstantKind{value}, Nat64{static_cast<size_t>(value.value)});
		});
}

const Constant* constantInt32(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType int32Type,
	const Int32 value
) {
	return getOrAdd<const Int32, const Constant*, compareInt32>{}(
		arena,
		&allConstants->int32s,
		value,
		[&]() {
			return newConstant(
				arena, allConstants, int32Type, ConstantKind{value}, Nat64{static_cast<size_t>(value.value)});
		});
}

const Constant* constantInt64(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType int64Type,
	const Int64 value
) {
	return getOrAdd<const Int64, const Constant*, compareInt64>{}(
		arena,
		&allConstants->int64s,
		value,
		[&]() {
			// Id matters for mangling. Go ahead and wrap to nat64.
			return newConstant(
				arena, allConstants, int64Type, ConstantKind{value}, Nat64{static_cast<size_t>(value.value)});
		});
}

const Constant* constantNat64(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType nat64Type,
	const Nat64 value
) {
	return getOrAdd<const Nat64, const Constant*, compareNat64>{}(
		arena,
		&allConstants->nat64s,
		value,
		[&]() {
			return newConstant(
				arena,
				allConstants,
				nat64Type,
				ConstantKind{value},
				value);
		});
}

const Constant* constantNat32(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType nat32Type,
	const Nat32 value
) {
	return getOrAdd<const Nat32, const Constant*, compareNat32>{}(
		arena,
		&allConstants->nat32s,
		value,
		[&]() {
			return newConstant(
				arena,
				allConstants,
				nat32Type,
				ConstantKind{value},
				nat64FromNat32(value));
		});
}

const Constant* constantNat16(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType nat16Type,
	const Nat16 value
) {
	return getOrAdd<const Nat16, const Constant*, compareNat16>{}(
		arena,
		&allConstants->nat16s,
		value,
		[&]() {
			return newConstant(
				arena,
				allConstants,
				nat16Type,
				ConstantKind{value},
				nat64FromNat16(value));
		});
}

const Constant* constantNull(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType pointerType
) {
	assertIsPointer(pointerType);
	return getOrAdd<const ConcreteStruct*, const Constant*, comparePtr<const ConcreteStruct>>{}(
		arena,
		&allConstants->nulls,
		pointerType.strukt,
		[&]() {
			return newConstant(arena, allConstants, pointerType, ConstantKind{ConstantKind::Null{}}, Nat64{0});
		});
}

const Constant* constantLambda(Arena* arena, AllConstants* allConstants, const KnownLambdaBody* klb) {
	return newConstant(
		arena,
		allConstants,
		klb->dynamicType,
		ConstantKind{ConstantKind::Lambda{klb}},
		Nat64{allConstants->nextLambdaId++});
}

const Constant* constantPtr(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType pointerType,
	const Constant* array,
	const Nat64 index
) {
	assertIsPointer(pointerType);
	const ConstantKind::Array a = array->kind.asArray();
	// Pointer may point to the end of the array
	const size_t nPtrs = a.size() + 1;
	assert(index.value < nPtrs);
	const Arr<const Constant*> ptrs = getOrAdd<
		const Constant*,
		Arr<const Constant*>,
		comparePtr<const Constant>
	>{}(arena, &allConstants->arrayToPtrs, array, [&]() {
		return fillArr<const Constant*>{}(arena, nPtrs, [&](const size_t idx) {
			return newConstant(
				arena,
				allConstants,
				pointerType,
				ConstantKind{ConstantKind::Ptr{array, Nat64{idx}}},
				Nat64{allConstants->nextPtrId++});
		});
	});
	return at(ptrs, index.value);
}

const Constant* constantRecord(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType recordType,
	const Arr<const Constant*> args
) {
	assert(recordType.strukt->isRecord());
	ConstantsForRecord* cr = getOrAdd<const ConcreteType, ConstantsForRecord*, compareConcreteType>{}(
		arena,
		&allConstants->records,
		recordType,
		[&]() {
			return nu<ConstantsForRecord>{}(arena);
		});
	return getOrAdd<const Arr<const Constant*>, const Constant*, compareArrConstants>{}(
		arena,
		&cr->values,
		args,
		[&]() {
			return newConstant(
				arena,
				allConstants,
				recordType,
				ConstantKind{ConstantKind::Record{recordType, args}},
				Nat64{cr->nextId++});
		});
}

const Constant* constantUnion(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteType unionType,
	const size_t memberIndex,
	const Constant* member
) {
	assert(unionType.strukt->isUnion());
	ConstantsForUnion* cu = getOrAdd<
		const ConcreteStruct*,
		ConstantsForUnion*,
		comparePtr<const ConcreteStruct>
	>{}(arena, &allConstants->unions, unionType.strukt, [&]() {
		return nu<ConstantsForUnion>{}(arena);
	});
	const Constant* res = getOrAdd<const Constant*, const Constant*, comparePtr<const Constant>>{}(
		arena,
		&cu->values,
		member,
		[&]() {
			return newConstant(
				arena,
				allConstants,
				unionType,
				ConstantKind{ConstantKind::Union{unionType.strukt, memberIndex, member}},
				Nat64{cu->nextId++});
		});
	assert(res->kind.asUnion().memberIndex == memberIndex);
	return res;
}

const Constant* constantVoid(Arena* arena, AllConstants* allConstants, const ConcreteType voidType) {
	return lazilySet(&allConstants->__void, [&]() {
		return newConstant(arena, allConstants, voidType, ConstantKind{ConstantKind::Void{}}, Nat64{0});
	});
}
