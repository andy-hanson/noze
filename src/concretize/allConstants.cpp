#include "./allConstants.h"

namespace {
	bool arrConstantsEq(const Arr<const Constant*> a, const Arr<const Constant*> b) {
		return arrEq<const Constant*, ptrEquals<const Constant>>(a, b);
	}
}

struct ConstantsForRecord {
	size_t nextId;
	MutDict<const Arr<const Constant*>, const Constant*, arrConstantsEq> values;
	ConstantsForRecord(const ConstantsForRecord&) = delete;
};

struct ConstantsForUnion {
	size_t nextId;
	// Maps a struct to that struct as a member of the union
	MutDict<const Constant*, const Constant*, ptrEquals<const Constant>> values;
	ConstantsForUnion(const ConstantsForUnion&) = delete;
};

AllConstants::AllConstants(Arena& arena) {
	_false = _nuConstant(arena, ConstantKind{false}, 0);
	_true = _nuConstant(arena, ConstantKind{true}, 1);
	_void = _nuConstant(arena, ConstantKind{ConstantKind::Void{}}, 0);
	_null = _nuConstant(arena, ConstantKind{ConstantKind::Null{}}, 0);
}

const Constant* AllConstants::_nuConstant(Arena& arena, const ConstantKind kind, const size_t id) {
	const Constant* res = arena.nu<const Constant>()(kind, id);
	all.add(arena, res);
	return res;
}

const Constant* AllConstants::arr(Arena& arena, const ConcreteStruct* arrayType, const ConcreteType elementType, const Arr<const Constant*> elements) {
	const ConstantArrKey key = ConstantArrKey{elementType, elements};
	return arrays.getOrAdd(arena, key, [&]() {
		return _nuConstant(arena, ConstantKind{ConstantKind::Array{key, arrayType}}, nextArrId++);
	});
}

const Constant* AllConstants::ptr(Arena& arena, const Constant* array, const size_t index) {
	const ConstantKind::Array a = array->kind.asArray();
	assert(index < a.size());
	const Arr<const Constant*> ptrs = arrayToPtrs.getOrAdd(arena, array, [&]() {
		return fillArr<const Constant*>{}(arena, a.size(), [&](const size_t idx) {
			return _nuConstant(arena, ConstantKind::Ptr{array, idx}, nextPtrId++);
		});
	});
	return ptrs[index];
}

const Constant* AllConstants::_char(Arena& arena, const char value) {
	return chars.getOrAdd(arena, value, [&]() {
		return _nuConstant(arena, ConstantKind{value}, value);
	});
}

const Constant* AllConstants::int64(Arena& arena, const Int64 value) {
	return int64s.getOrAdd(arena, value, [&]() {
		// Id matters for mangling. Go ahead and wrap to nat64.
		return _nuConstant(arena, ConstantKind{value}, static_cast<Nat64>(value));
	});
}

const Constant* AllConstants::nat64(Arena& arena, const Nat64 value) {
	return nat64s.getOrAdd(arena, value, [&]() {
		return _nuConstant(arena, ConstantKind{value}, value);
	});
}

const Constant* AllConstants::funPtr(Arena& arena, const ConcreteFun* fun) {
	todo<void>("TODO: don't create multiple constants");
	return _nuConstant(arena, ConstantKind{fun}, nextLambdaId++);
}

const Constant* AllConstants::lambda(Arena& arena, const KnownLambdaBody* klb) {
	return _nuConstant(arena, ConstantKind{klb}, nextLambdaId++);
}

const Constant* AllConstants::record(Arena& arena, const ConcreteType type, const Arr<const Constant*> args) {
	assert(type.strukt->isRecord());
	ConstantsForRecord* cr = records.getOrAdd(arena, type, [&]() {
		return arena.nu<ConstantsForRecord>()();
	});
	return cr->values.getOrAdd(arena, args, [&]() {
		return _nuConstant(arena, ConstantKind{ConstantKind::Record{type, args}}, cr->nextId++);
	});
}

const Constant* AllConstants::_union(Arena& arena, const ConcreteType unionType, const size_t memberIndex, const Constant* member) {
	assert(unionType.strukt->isUnion());
	ConstantsForUnion* cu = unions.getOrAdd(arena, unionType.strukt, [&]() {
		return arena.nu<ConstantsForUnion>()();
	});
	const Constant* res = cu->values.getOrAdd(arena, member, [&]() {
		return _nuConstant(arena, ConstantKind{ConstantKind::Union{unionType.strukt, memberIndex, member}}, cu->nextId++);
	});
	assert(res->kind.asUnion().memberIndex == memberIndex);
	return res;
}
