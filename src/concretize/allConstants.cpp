#include "./allConstants.h"

#include "../util/arrUtil.h"

namespace {
	Comparison compareArrConstants(const Arr<const Constant*> a, const Arr<const Constant*> b) {
		return compareArr<const Constant*, comparePointer<const Constant>>(a, b);
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
	MutDict<const Constant*, const Constant*, comparePointer<const Constant>> values;
	ConstantsForUnion(const ConstantsForUnion&) = delete;
};

const Constant* AllConstants::_nuConstant(Arena& arena, const ConcreteType type, const ConstantKind kind, const size_t id) {
	const Constant* res = arena.nu<const Constant>()(type, kind, id);
	all.add(arena, res);
	return res;
}

const Constant* AllConstants::arr(Arena& arena, const ConcreteStruct* arrayType, const ConcreteType elementType, const Arr<const Constant*> elements) {
	const ConstantArrKey key = ConstantArrKey{elementType, elements};
	return arrays.getOrAdd(arena, key, [&]() {
		return _nuConstant(arena, ConcreteType::fromStruct(arrayType), ConstantKind{ConstantKind::Array{key}}, nextArrId++);
	});
}

namespace {
	void assertIsPointer(const ConcreteType pointerType) {
		assert(strEqLiteral(slice(pointerType.strukt->mangledName, 0, 3), "ptr"));
		assert(!pointerType.isPointer);
	}
}

const Constant* AllConstants::ptr(Arena& arena, const ConcreteType pointerType, const Constant* array, const size_t index) {
	assertIsPointer(pointerType);
	const ConstantKind::Array a = array->kind.asArray();
	assert(index < a.size());
	const Arr<const Constant*> ptrs = arrayToPtrs.getOrAdd(arena, array, [&]() {
		return fillArr<const Constant*>{}(arena, a.size(), [&](const size_t idx) {
			return _nuConstant(arena, pointerType, ConstantKind{ConstantKind::Ptr{array, idx}}, nextPtrId++);
		});
	});
	return ptrs[index];
}

const Constant* AllConstants::_null(Arena& arena, const ConcreteType pointerType) {
	assertIsPointer(pointerType);
	return nulls.getOrAdd(arena, pointerType.strukt, [&]() {
		return _nuConstant(arena, pointerType, ConstantKind{ConstantKind::Null{}}, 0);
	});
}

const Constant* AllConstants::_bool(Arena& arena, const ConcreteType boolType, const Bool value)  {
	if (value)
		return lazilySet(_true, [&]() {
			return _nuConstant(arena, boolType, ConstantKind{True}, 0);
		});
	else
		return lazilySet(_false, [&]() {
			return _nuConstant(arena, boolType, ConstantKind{False}, 1);
		});
}

const Constant* AllConstants::_void(Arena& arena, const ConcreteType voidType) {
	return lazilySet(__void, [&]() {
		return _nuConstant(arena, voidType, ConstantKind{ConstantKind::Void{}}, 0);
	});
}

const Constant* AllConstants::_char(Arena& arena, const ConcreteType charType, const char value) {
	return chars.getOrAdd(arena, value, [&]() {
		return _nuConstant(arena, charType, ConstantKind{value}, value);
	});
}

const Constant* AllConstants::int64(Arena& arena, const ConcreteType int64Type, const Int64 value) {
	return int64s.getOrAdd(arena, value, [&]() {
		// Id matters for mangling. Go ahead and wrap to nat64.
		return _nuConstant(arena, int64Type, ConstantKind{value}, static_cast<Nat64>(value));
	});
}

const Constant* AllConstants::nat64(Arena& arena, const ConcreteType nat64Type, const Nat64 value) {
	return nat64s.getOrAdd(arena, value, [&]() {
		return _nuConstant(arena, nat64Type, ConstantKind{value}, value);
	});
}

const Constant* AllConstants::funPtr(Arena& arena, const ConcreteType funPtrType, const ConcreteFun* fun) {
	//TODO: cache this!
	return _nuConstant(arena, funPtrType, ConstantKind{ConstantKind::FunPtr{fun}}, nextLambdaId++);
}

const Constant* AllConstants::lambda(Arena& arena, const KnownLambdaBody* klb) {
	return _nuConstant(arena, klb->dynamicType, ConstantKind{ConstantKind::Lambda{klb}}, nextLambdaId++);
}

const Constant* AllConstants::record(Arena& arena, const ConcreteType recordType, const Arr<const Constant*> args) {
	assert(recordType.strukt->isRecord());
	ConstantsForRecord* cr = records.getOrAdd(arena, recordType, [&]() {
		return arena.nu<ConstantsForRecord>()();
	});
	return cr->values.getOrAdd(arena, args, [&]() {
		return _nuConstant(arena, recordType, ConstantKind{ConstantKind::Record{recordType, args}}, cr->nextId++);
	});
}

const Constant* AllConstants::_union(Arena& arena, const ConcreteType unionType, const size_t memberIndex, const Constant* member) {
	assert(unionType.strukt->isUnion());
	ConstantsForUnion* cu = unions.getOrAdd(arena, unionType.strukt, [&]() {
		return arena.nu<ConstantsForUnion>()();
	});
	const Constant* res = cu->values.getOrAdd(arena, member, [&]() {
		return _nuConstant(arena, unionType, ConstantKind{ConstantKind::Union{unionType.strukt, memberIndex, member}}, cu->nextId++);
	});
	assert(res->kind.asUnion().memberIndex == memberIndex);
	return res;
}
