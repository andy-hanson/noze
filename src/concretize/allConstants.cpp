#include "./allConstants.h"

#include "../util/arrUtil.h"

namespace {
	const Constant* newConstant(Arena* arena, AllConstants* allConstants, const ConcreteType type, const ConstantKind kind, const size_t id) {
		const Constant* res = nu<const Constant>{}(arena, type, kind, id);
		add(arena, &allConstants->all, res);
		return res;
	}

	Comparison compareArrConstants(const Arr<const Constant*> a, const Arr<const Constant*> b) {
		return compareArr<const Constant*, comparePtr<const Constant>>(a, b);
	}
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

const Constant* constantArr(
	Arena* arena,
	AllConstants* allConstants,
	const ConcreteStruct* arrayType,
	const ConcreteType elementType,
	const Arr<const Constant*> elements
) {
	const ConstantArrKey key = ConstantArrKey{elementType, elements};
	return getOrAdd<const ConstantArrKey, const Constant*, compareConstantArrKey>{}(arena, &allConstants->arrays, key, [&]() {
		return newConstant(arena, allConstants, ConcreteType::fromStruct(arrayType), ConstantKind{ConstantKind::Array{key}}, allConstants->nextArrId++);
	});
}

namespace {
	void assertIsPointer(const ConcreteType pointerType) {
		assert(strEqLiteral(slice(pointerType.strukt->mangledName, 0, 3), "ptr"));
		assert(!pointerType.isPointer);
	}
}

const Constant* constantPtr(Arena* arena, AllConstants* allConstants, const ConcreteType pointerType, const Constant* array, const size_t index) {
	assertIsPointer(pointerType);
	const ConstantKind::Array a = array->kind.asArray();
	// Pointer may point to the end of the array
	const size_t nPtrs = a.size() + 1;
	assert(index < nPtrs);
	const Arr<const Constant*> ptrs = getOrAdd<
		const Constant*,
		Arr<const Constant*>,
		comparePtr<const Constant>
	>{}(arena, &allConstants->arrayToPtrs, array, [&]() {
		return fillArr<const Constant*>{}(arena, nPtrs, [&](const size_t idx) {
			return newConstant(arena, allConstants, pointerType, ConstantKind{ConstantKind::Ptr{array, idx}}, allConstants->nextPtrId++);
		});
	});
	return at(ptrs, index);
}

const Constant* constantNull(Arena* arena, AllConstants* allConstants, const ConcreteType pointerType) {
	assertIsPointer(pointerType);
	return getOrAdd<const ConcreteStruct*, const Constant*, comparePtr<const ConcreteStruct>>{}(arena, &allConstants->nulls, pointerType.strukt, [&]() {
		return newConstant(arena, allConstants, pointerType, ConstantKind{ConstantKind::Null{}}, 0);
	});
}

const Constant* constantBool(Arena* arena, AllConstants* allConstants, const ConcreteType boolType, const Bool value)  {
	if (value)
		return lazilySet(&allConstants->_true, [&]() {
			return newConstant(arena, allConstants, boolType, ConstantKind{True}, 0);
		});
	else
		return lazilySet(&allConstants->_false, [&]() {
			return newConstant(arena, allConstants, boolType, ConstantKind{False}, 1);
		});
}

const Constant* constantVoid(Arena* arena, AllConstants* allConstants, const ConcreteType voidType) {
	return lazilySet(&allConstants->__void, [&]() {
		return newConstant(arena, allConstants, voidType, ConstantKind{ConstantKind::Void{}}, 0);
	});
}

const Constant* constantChar(Arena* arena, AllConstants* allConstants, const ConcreteType charType, const char value) {
	return getOrAdd<const char, const Constant*, comparePrimitive<const char>>{}(arena, &allConstants->chars, value, [&]() {
		return newConstant(arena, allConstants, charType, ConstantKind{value}, value);
	});
}

const Constant* constantInt64(Arena* arena, AllConstants* allConstants, const ConcreteType int64Type, const Int64 value) {
	return getOrAdd<const Int64, const Constant*, comparePrimitive<const Int64>>{}(arena, &allConstants->int64s, value, [&]() {
		// Id matters for mangling. Go ahead and wrap to nat64.
		return newConstant(arena, allConstants, int64Type, ConstantKind{value}, static_cast<Nat64>(value));
	});
}

const Constant* constantNat64(Arena* arena, AllConstants* allConstants, const ConcreteType nat64Type, const Nat64 value) {
	return getOrAdd<const Nat64, const Constant*, comparePrimitive<const Nat64>>{}(arena, &allConstants->nat64s, value, [&]() {
		return newConstant(arena, allConstants, nat64Type, ConstantKind{value}, value);
	});
}

const Constant* constantFunPtr(Arena* arena, AllConstants* allConstants, const ConcreteType funPtrType, const ConcreteFun* fun) {
	//TODO: cache this!
	return newConstant(arena, allConstants, funPtrType, ConstantKind{ConstantKind::FunPtr{fun}}, allConstants->nextLambdaId++);
}

const Constant* constantLambda(Arena* arena, AllConstants* allConstants, const KnownLambdaBody* klb) {
	return newConstant(arena, allConstants, klb->dynamicType, ConstantKind{ConstantKind::Lambda{klb}}, allConstants->nextLambdaId++);
}

const Constant* constantRecord(Arena* arena, AllConstants* allConstants, const ConcreteType recordType, const Arr<const Constant*> args) {
	assert(recordType.strukt->isRecord());
	ConstantsForRecord* cr = getOrAdd<const ConcreteType, ConstantsForRecord*, compareConcreteType>{}(arena, &allConstants->records, recordType, [&]() {
		return nu<ConstantsForRecord>{}(arena);
	});
	return getOrAdd<const Arr<const Constant*>, const Constant*, compareArrConstants> {}(arena, &cr->values, args, [&]() {
		return newConstant(arena, allConstants, recordType, ConstantKind{ConstantKind::Record{recordType, args}}, cr->nextId++);
	});
}

const Constant* constantUnion(Arena* arena, AllConstants* allConstants, const ConcreteType unionType, const size_t memberIndex, const Constant* member) {
	assert(unionType.strukt->isUnion());
	ConstantsForUnion* cu = getOrAdd<
		const ConcreteStruct*,
		ConstantsForUnion*,
		comparePtr<const ConcreteStruct>
	>{}(arena, &allConstants->unions, unionType.strukt, [&]() {
		return nu<ConstantsForUnion>{}(arena);
	});
	const Constant* res = getOrAdd<const Constant*, const Constant*, comparePtr<const Constant>>{}(arena, &cu->values, member, [&]() {
		return newConstant(arena, allConstants, unionType, ConstantKind{ConstantKind::Union{unionType.strukt, memberIndex, member}}, cu->nextId++);
	});
	assert(res->kind.asUnion().memberIndex == memberIndex);
	return res;
}
