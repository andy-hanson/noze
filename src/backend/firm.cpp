#include "./firm.h"

#include <libfirm/firm.h>

#include "../util/dictBuilder.h"

namespace {
	// We'll create both of these eagerly, even if only one is needed.
	struct TypesForStruct {
		ir_type* valueType;
		ir_type* pointerType;

		TypesForStruct(ir_type* _valueType, ir_type* _pointerType)
			: valueType{_valueType}, pointerType{_pointerType} {
			assert(is_Pointer_type(pointerType));
			assert(get_pointer_points_to_type(pointerType) == valueType);
		}
	};

	using TypesDict = Dict<const ConcreteStruct*, const TypesForStruct, comparePtr<const ConcreteStruct>>;

	// Just create the type, fill it in later.
	// This is never a pointer type (unless the type is ptr)
	ir_type* initValueType(const ConcreteStruct* s) {
		const Str name = s->mangledName;
		ident* id = new_id_from_chars(name.begin(), name.size);
		return s->body().match(
			[](const ConcreteStructBody::Builtin) {
				return todo<ir_type*>("BUILTIN");
			},
			[&](const ConcreteStructBody::Record) {
				return new_type_struct(id);
			},
			[](const ConcreteStructBody::Union) {
				return todo<ir_type*>("UNION");
			},
			[](const ConcreteStructBody::Iface) {
				return todo<ir_type*>("IFACE");
			});
	}

	void fillInType(const ConcreteStruct* s, ir_type* i, const TypesDict typesDict) {
		unused(i); unused(typesDict);
		s->body().match(
			[](const ConcreteStructBody::Builtin) {
				todo<void>("BUILTIN");
			},
			[](const ConcreteStructBody::Record) {
				todo<void>("FIELDS");
			},
			[](const ConcreteStructBody::Union) {
				// A noze union compiles to a struct with 2 fields -- a 'kind' enum, and a union of the members.
				todo<void>("UNION");
			},
			[](const ConcreteStructBody::Iface) {
				todo<void>("IFACE");
			});
	}

	const TypesDict getTypesDict(Arena& arena, const Arr<const ConcreteStruct*> allStructs) {
		const TypesDict typesDict = [&]() {
			DictBuilder<const ConcreteStruct*, const TypesForStruct, comparePtr<const ConcreteStruct>> allTypes {};
			for (const ConcreteStruct* s : allStructs) {
				ir_type* valueType = initValueType(s);
				ir_type* pointerType = new_type_pointer(valueType);
				set_pointer_points_to_type(pointerType, valueType);
				addToDict<const ConcreteStruct*, const TypesForStruct, comparePtr<const ConcreteStruct>>(arena, &allTypes, s, TypesForStruct{valueType, pointerType});
			}
			return finishDictShouldBeNoConflict(&allTypes);
		}();
		for (const ConcreteStruct* s : allStructs)
			fillInType(s, mustGetAt(typesDict, s).valueType, typesDict);
		return typesDict;
	}

	mtp_additional_properties getAdditionalProperties(const ConcreteFun* cf) {
		// TODO: we can improve performance by providing mroe information.
		// https://libfirm.github.io/api_latest/a00163.html#gga7f9b9d4beb2438880b94362a62459692a970970c5cd0c5127042f1b35c9d61c48

		// Especially mtp_property_private, which should be there for all non-extern functions
		unused(cf);
		return mtp_no_property;
	}

	void createProtoForFun(const ConcreteFun* cf, const TypesDict typesDict, ir_type* ctxPtrType) {

		const size_t fullArity = cf->arityIncludingCtxAndClosure();
		ir_type *proto_type = new_type_method(
			fullArity,
			// We are creating only one method, so n_res=1
			/*n_res*/ 1,
			/*is_variadic*/ false,
			// TODO: determine the calling convention to use. For private functions I'd like to just not specify this
			cc_cdecl_set,
			getAdditionalProperties(cf));

		if (cf->needsCtx) {
			set_method_param_type(proto_type, 0, ctxPtrType);
		}

		unused(typesDict);

		todo<void>("set other params, and return type");
	}

	void doIt(const ConcreteProgram program) {
		Arena arena {};
		const TypesDict typesDict = getTypesDict(arena, program.allStructs);

		ir_type* ctxType = mustGetAt(typesDict, program.ctxType).pointerType;

		for (const ConcreteFun* cf : program.allFuns) {
			createProtoForFun(cf, typesDict, ctxType);
		}
	}
}

void doSomethingWithLibFirm(const ConcreteProgram program) {
	ir_init();

	doIt(program);

	ir_finish();

	todo<void>("todo");
}
