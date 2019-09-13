#include "./writeToC.h"

#include "../concretize/concretizeUtil.h" // writeConcreteTypeForMangle
#include "../util/arrUtil.h"
#include "../util/mutSet.h"
#include "../util/writer.h"

namespace {
	void writeQuotedString(Writer* writer, const Arr<const Constant*> chars) {
		writeChar(writer, '"');
		for (const Constant* c : chars)
			writeEscapedChar(writer, c->kind.asChar());
		writeChar(writer, '"');
	}

	void writeValueType(Writer* writer, const ConcreteStruct* s) {
		writeStr(writer, s->mangledName);
	}

	void writeValueType(Writer* writer, const ConcreteType t) {
		writeValueType(writer, t.mustBeNonPointer());
	}

	void writeType(Writer* writer, const ConcreteType t) {
		writeValueType(writer, t.strukt);
		if (t.isPointer)
			writeChar(writer, '*');
	}

	void writeCastToType(Writer* writer, const ConcreteType type) {
		writeStatic(writer, "(");
		writeType(writer, type);
		writeStatic(writer, ") ");
	}

	void doWriteParam(Writer* writer, const ConcreteParam p) {
		writeType(writer, p.type);
		writeChar(writer, ' ');
		writeStr(writer, p.mangledName);
	}

	void writeJustParams(Writer* writer, const Bool wroteFirst, const Arr<const ConcreteParam> params) {
		Cell<const Bool> didWriteFirst { wroteFirst };
		for (const ConcreteParam p : params) {
			if (cellGet(&didWriteFirst))
				writeStatic(writer, ", ");
			else
				cellSet<const Bool>(&didWriteFirst, True);
			doWriteParam(writer, p);
		}
		writeChar(writer, ')');
	}

	void writeJustParamsAlwaysComma(Writer* writer, const Arr<const ConcreteParam> params) {
		for (const ConcreteParam p : params) {
			writeStatic(writer, ", ");
			doWriteParam(writer, p);
		}
	}

	void writeBuiltinAliasForStruct(
		Writer* writer,
		const Str name,
		const BuiltinStructKind kind,
		const Arr<const ConcreteType> typeArgs
	) {
		switch (kind) {
			case BuiltinStructKind::_char:
				// "char" shares the same name as in C, so no need for an alias.
				break;

			case BuiltinStructKind::funPtrN:
				writeStatic(writer, "typedef ");
				writeType(writer, first(typeArgs));
				writeStatic(writer, " (*");
				writeStr(writer, name);
				writeStatic(writer, ")(");
				for (const size_t i : Range{1, size(typeArgs)}) {
					if (i != 1)
						writeStatic(writer, ", ");
					writeType(writer, at(typeArgs, i));
				}
				writeStatic(writer, ");\n");
				break;

			default:
				writeStatic(writer, "typedef ");
				switch (kind) {
					case BuiltinStructKind::_bool:
						writeStatic(writer, "uint8_t");
						break;
					case BuiltinStructKind::byte:
						writeStatic(writer, "uint8_t");
						break;
					case BuiltinStructKind::float64:
						writeStatic(writer, "double");
						break;
					case BuiltinStructKind::int16:
						writeStatic(writer, "int16_t");
						break;
					case BuiltinStructKind::int32:
						writeStatic(writer, "int32_t");
						break;
					case BuiltinStructKind::int64:
						writeStatic(writer, "int64_t");
						break;
					case BuiltinStructKind::nat64:
						writeStatic(writer, "uint64_t");
						break;
					case BuiltinStructKind::nat32:
						writeStatic(writer, "uint32_t");
						break;
					case BuiltinStructKind::nat16:
						writeStatic(writer, "uint16_t");
						break;
					case BuiltinStructKind::ptr:
						writeType(writer, at(typeArgs, 0));
						writeChar(writer, '*');
						break;
					case BuiltinStructKind::_void:
						writeStatic(writer, "uint8_t");
						break;
					default:
						assert(0);
				}
				writeChar(writer, ' ');
				writeStr(writer, name);
				writeStatic(writer, ";\n");
				break;
		}
	}

	void writeStructHead(Writer* writer, const Str mangledName) {
		writeStatic(writer, "struct ");
		writeStr(writer, mangledName);
		writeStatic(writer, " {");
	}

	void writeStructEnd(Writer* writer) {
		writeStatic(writer, "\n};\n");
	}

	void writeFieldsStruct(Writer* writer, const Str mangledName, const Arr<const ConcreteField> fields) {
		writeStructHead(writer, mangledName);
		if (isEmpty(fields))
			// An empty structure is undefined behavior in C.
			writeStatic(writer, "\n\tbool __mustBeNonEmpty;\n};\n");
		else {
			for (const ConcreteField field : fields) {
				writeStatic(writer, "\n\t");
				writeType(writer, field.type);
				writeStatic(writer, " ");
				writeStr(writer, field.mangledName);
				writeStatic(writer, ";");
			}
			writeStructEnd(writer);
		}
	}

	void writeUnionStruct(Writer* writer, const Str mangledName, const Arr<const ConcreteType> members) {
		writeStructHead(writer, mangledName);
		writeStatic(writer, "\n\tint kind;");
		writeStatic(writer, "\n\tunion {");
		for (const ConcreteType member : members) {
			writeStatic(writer, "\n\t\t");
			writeType(writer, member);
			writeStatic(writer, " as_");
			writeStr(writer, member.strukt->mangledName);
			writeChar(writer, ';');
		}
		writeStatic(writer, "\n\t};");
		writeStructEnd(writer);
	}

	void writeIfaceStruct(Writer* writer, const Str mangledName, const Arr<const ConcreteSig> messages) {
		writeStructHead(writer, mangledName);

		// Create a vtable struct containing its fields.
		// And add funs to call those.
		writeStatic(writer, "\n\tstruct Funs {");
		for (const ConcreteSig msg : messages) {
			writeStatic(writer, "\n\t\t");
			writeType(writer, msg.returnType);
			writeStatic(writer, " (*)(ctx* ctx, void* closure");
			writeJustParamsAlwaysComma(writer, msg.params);
			writeChar(writer, ' ');
			writeStr(writer, msg.mangledName);
			writeChar(writer, ';');
		}
		todo<void>("do without mixins");

		// For convenience, write a method for calling each of the funs.
		// E.g.:
		// fut<_void>* doSomething(ctx* ctx, bool param0) {
		//	return vtable.funs.push(ctx, data, param0);
		// }

		for (const ConcreteSig msg : messages) {
			writeStatic(writer, "\n\t");
			writeType(writer, msg.returnType);
			writeChar(writer, ' ');
			writeStr(writer, msg.mangledName);
			writeStatic(writer, "ctx* ctx");
			writeJustParamsAlwaysComma(writer, msg.params);
			writeStatic(writer, " {\n\t\treturn vtable->funs.");
			writeStr(writer, msg.mangledName);
			writeStatic(writer, "(ctx, data");
			for (const ConcreteParam p : msg.params) {
				writeStatic(writer, ", ");
				writeStr(writer, p.mangledName);
			}
			writeStatic(writer, ");\n\t}");
		}

		writeStructEnd(writer);
	}

	enum class StructState {
		declared,
		defined
	};

	using StructStates = MutDict<const ConcreteStruct*, const StructState, comparePtr<const ConcreteStruct>>;

	const Bool canReferenceType(const ConcreteType t, const StructStates* structStates) {
		const Opt<const StructState> state = getAt_mut(structStates, t.strukt);
		if (has(state))
			switch (force(state)) {
				case StructState::declared:
					return t.isPointer;
				case StructState::defined:
					return True;
				default:
					assert(0);
			}
		else
			return False;
	}

	const Bool sigCanReferenceTypes(const ConcreteSig s, const StructStates* structStates) {
		return _and(
			canReferenceType(s.returnType, structStates),
			every(s.params, [&](const ConcreteParam p) {
				return canReferenceType(p.type, structStates);
			}));
	}

	void declareStruct(Writer* writer, const ConcreteStruct* strukt) {
		writeStatic(writer, "typedef struct ");
		writeStr(writer, strukt->mangledName);
		writeStatic(writer, " ");
		writeStr(writer, strukt->mangledName);
		writeStatic(writer, ";\n");
	}

	// Returns any new work it did -- if we declared or defined the struct
	const Opt<const StructState> writeStructDeclarationOrDefinition(
		Writer* writer,
		const ConcreteStruct* strukt,
		const StructStates* structStates
	) {
		auto declare = [&]() -> const Opt<const StructState> {
			if (!hasKey_mut(structStates, strukt)) {
				declareStruct(writer, strukt);
				return some<const StructState>(StructState::declared);
			} else
				return none<const StructState>();
		};
		const Opt<const StructState> defined = some<const StructState>(StructState::defined);

		return strukt->body().match(
			[&](const ConcreteStructBody::Builtin b) {
				if (every(b.typeArgs, [&](const ConcreteType t) { return canReferenceType(t, structStates); })) {
					writeBuiltinAliasForStruct(writer, strukt->mangledName, b.info.kind, b.typeArgs);
					return defined;
				} else
					return none<const StructState>();
			},
			[&](const ConcreteStructBody::Record r) {
				if (every(r.fields, [&](const ConcreteField f) { return canReferenceType(f.type, structStates); })) {
					declare();
					writeFieldsStruct(writer, strukt->mangledName, r.fields);
					return defined;
				} else
					return declare();
			},
			[&](const ConcreteStructBody::Union u) {
				if (every(u.members, [&](const ConcreteType t) { return canReferenceType(t, structStates); })) {
					declare();
					writeUnionStruct(writer, strukt->mangledName, u.members);
					return defined;
				} else
					return declare();
			},
			[&](const ConcreteStructBody::Iface i) {
				if (every(i.messages, [&](const ConcreteSig s) { return sigCanReferenceTypes(s, structStates); })) {
					declare();
					writeIfaceStruct(writer, strukt->mangledName, i.messages);
					return defined;
				} else
					return declare();
			});
	}

	void writeStructs(Writer* writer, const Arr<const ConcreteStruct*> allStructs) {
		Arena tempArena {};
		StructStates structStates {};
		for (;;) {
			Cell<const Bool> madeProgress { False };
			Cell<const Bool> someIncomplete { False };
			for (const ConcreteStruct* strukt : allStructs) {
				const Opt<const StructState> curState = getAt_mut(&structStates, strukt);
				if (!has(curState) || force(curState) != StructState::defined) {
					const Opt<const StructState> didWork =
						writeStructDeclarationOrDefinition(writer, strukt, &structStates);
					if (has(didWork)) {
						setInDict<
							const ConcreteStruct*,
							const StructState,
							comparePtr<const ConcreteStruct>
						>(&tempArena, &structStates, strukt, force(didWork));
						cellSet<const Bool>(&madeProgress, True);
					} else
						cellSet<const Bool>(&someIncomplete, True);
				}
			}
			if (cellGet(&someIncomplete))
				assert(cellGet(&madeProgress));
			else
				break;
		}
		writeStatic(writer, "\n");
	}
}

namespace {
	void writeConstantRecordName(Writer* writer, const Constant* c, const ConcreteType t) {
		writeStatic(writer, "_constant__");
		writeConcreteTypeForMangle(writer, t);
		writeStatic(writer, "__");
		writeNat(writer, c->id);
	}

	void writeConstantUnionName(Writer* writer, const Constant* c, const ConcreteStruct* s) {
		writeStatic(writer, "_constant__");
		writeStr(writer, s->mangledName);
		writeStatic(writer, "__");
		writeNat(writer, c->id);
	}

	void writeConstantReference(Writer* writer, const Constant* c, const Bool isInsideConstant) {
		auto writeRef = [&](const CStr s) {
			writeStatic(writer, "_constant");
			writeStatic(writer, s);
			writeNat(writer, c->id);
		};

		auto maybeWriteCast = [&]() -> void {
			if (!isInsideConstant) {
				writeChar(writer, '(');
				writeValueType(writer, c->type());
				writeStatic(writer, ") ");
			}
		};

		c->kind.match(
			[&](const ConstantKind::Array) {
				maybeWriteCast();
				writeRef("Arr");
			},
			[&](const Bool b) {
				writeStatic(writer, b ? "1" : "0");
			},
			[&](const char c) {
				writeChar(writer, '\'');
				writeEscapedChar(writer, c);
				writeChar(writer, '\'');
			},
			[&](const ConstantKind::FunPtr f) {
				writeStatic(writer, "(&");
				writeStr(writer, f.fun->mangledName());
				writeChar(writer, ')');
			},
			[&](const Int16 i) {
				writeInt(writer, i);
			},
			[&](const Int32 i) {
				writeInt(writer, i);
			},
			[&](const Int64 i) {
				writeInt(writer, i);
				writeStatic(writer, "ll");
			},
			[&](const ConstantKind::Lambda l) {
				unused(l);
				// Don't need a decl for this (other than the concretefun), just create it here
				todo<void>("write reference for const lambda");
				// fun0___void{someFun, closurePtr};
			},
			[&](const Nat16 n) {
				writeNat(writer, n);
				writeStatic(writer, "u");
			},
			[&](const Nat32 n) {
				writeNat(writer, n);
				writeStatic(writer, "u");
			},
			[&](const Nat64 n) {
				writeNat(writer, n);
				writeStatic(writer, "ull");
			},
			[&](const ConstantKind::Null) {
				writeStatic(writer, "0");
			},
			[&](const ConstantKind::Ptr p) {
				writeStatic(writer, "(_constantArrBacking");
				writeNat(writer, p.array->id);
				writeStatic(writer, " + ");
				writeNat(writer, p.index);
				writeChar(writer, ')');
			},
			[&](const ConstantKind::Record r) {
				if (r.type.isPointer)
					writeChar(writer, '&');
				else
					maybeWriteCast();
				writeConstantRecordName(writer, c, r.type);
			},
			[&](const ConstantKind::Union u) {
				maybeWriteCast();
				writeConstantUnionName(writer, c, u.unionType);
			},
			[&](const ConstantKind::Void) {
				// Value should be ignored, so any will do
				writeChar(writer, '0');
			});
	}

	void writeConstantReferencesWithCommas(Writer* writer, const Arr<const Constant*> constants) {
		writeWithCommas(writer, constants, /*leadingComma*/ False, [&](const Constant* c) {
			writeConstantReference(writer, c, /*isInsideConstant*/ True);
		});
	}

	struct WrittenConstants {
		Arena arena {};
		MutSet<const Constant*, comparePtr<const Constant>> written {};
	};

	void ensureWrittenConstantDecl(Writer* writer, WrittenConstants* written, const Constant* c);

	void ensureWrittenConstantDecls(Writer* writer, WrittenConstants* written, const Arr<const Constant*> elements) {
		for (const Constant* e : elements)
			ensureWrittenConstantDecl(writer, written, e);
	}

	void writeConstantDecl(Writer* writer, WrittenConstants* written, const Constant* c) {
		const ConcreteType type = c->type();
		c->kind.match(
			[&](const ConstantKind::Array a) {
				// static nat64 _constantArrBacking123[3] = {1, 2, 3};
				// static arr_nat64 _constantArr123 = arr_nat64{3, _constantArrBacking123};

				// Strings are special:
				// static arr_char _constantArr123 = arr_char{5, const_cast<char*>("hello")};

				ensureWrittenConstantDecls(writer, written, a.elements());

				const size_t size = a.size();
				const Bool isStr = first(a.elements())->kind.isChar();
				const Nat64 id = c->id;
				if (size != 0) {
					writeStatic(writer, "static ");
					writeType(writer, a.elementType());
					writeStatic(writer, " _constantArrBacking");
					writeNat(writer, id);
					writeChar(writer, '[');
					writeNat(writer, size);
					writeStatic(writer, "] = ");
					if (isStr)
						writeQuotedString(writer, a.elements());
					else {
						writeChar(writer, '{');
						writeConstantReferencesWithCommas(writer, a.elements());
						writeChar(writer, '}');
					}
					writeStatic(writer, ";\n");
				}

				writeStatic(writer, "#define ");
				writeStatic(writer, " _constantArr");
				writeNat(writer, id);
				// NOTE: we can't have a cast to the type here, because that causes it to be non-constant apparently.
				//writeStatic(writer, " (");
				//writeValueType(writer, type);
				writeStatic(writer, " { ");
				writeNat(writer, size);
				writeStatic(writer, ", _constantArrBacking");
				writeNat(writer, id);
				writeStatic(writer, " }\n");
			},
			[](const Bool) {},
			[](const char) {},
			[](const ConstantKind::FunPtr) {},
			[](const Int16) {},
			[](const Int32) {},
			[](const Int64) {},
			// Do nothing for lambda now -- fun is by-value. Closure is emitted as a separate constant.
			[](const ConstantKind::Lambda) {},
			[](const Nat16) {},
			[](const Nat32) {},
			[](const Nat64) {},
			[](const ConstantKind::Null) {},
			[&](const ConstantKind::Ptr p) {
				ensureWrittenConstantDecl(writer, written, p.array);
			},
			[&](const ConstantKind::Record r) {
				ensureWrittenConstantDecls(writer, written, r.args);

				auto writeInitializer = [&]() -> void {
					writeStatic(writer, "{ ");
					if (isEmpty(r.args))
						// C requires structs to be non-empty
						writeStatic(writer, "0");
					else
						writeConstantReferencesWithCommas(writer, r.args);
					writeStatic(writer, " }");
				};

				// For a by-ref type: use 'static' variable.
				// For a by-val type: use a define
				if (type.isPointer) {
					writeStatic(writer, "static ");
					writeType(writer, r.type);
					writeChar(writer, ' ');
					writeConstantRecordName(writer, c, r.type);
					writeStatic(writer, " = ");
					writeInitializer();
					writeStatic(writer, ";\n");
				} else {
					writeStatic(writer, "#define ");
					writeConstantRecordName(writer, c, r.type);
					writeChar(writer, ' ');
					//writeStatic(writer, " (");
					//writeType(writer, r.type);
					//writeStatic(writer,") ");
					writeInitializer();
					writeStatic(writer, "\n");
				}
			},
			[&](const ConstantKind::Union u) {
				ensureWrittenConstantDecl(writer, written, u.member);

				writeStatic(writer, "#define ");
				writeConstantUnionName(writer, c, u.unionType);
				writeStatic(writer, " { ");
				writeNat(writer, u.memberIndex);
				writeStatic(writer, ", .as_");
				writeStr(writer, u.member->type().strukt->mangledName);
				writeStatic(writer, " = ");
				writeConstantReference(writer, u.member, /*isInsideConstant*/ True);
				writeStatic(writer, " }\n");
			},
			[](const ConstantKind::Void) {});
	}

	void ensureWrittenConstantDecl(Writer* writer, WrittenConstants* written, const Constant* c) {
		if (tryAddToMutSet(&written->arena, &written->written, c))
			writeConstantDecl(writer, written, c);
	}

	void writeConstants(Writer* writer, const Arr<const Constant*> allConstants) {
		// Unlike structs, constants can't have circular references.
		// So we'll just make sure to write a constant's dependencies immediately before writing it.
		WrittenConstants written {};
		ensureWrittenConstantDecls(writer, &written, allConstants);
	}

	void writeNewIfaceImpl(Writer* writer, const ConcreteExpr::NewIfaceImpl impl) {
		unused(writer);
		unused(impl);
		todo<void>("writeNewIfaceImpl");
	}

	void writeConcreteExpr(WriterWithIndent* writer, const ConcreteExpr ex);

	void writeConstantOrExpr(WriterWithIndent* writer, const ConstantOrExpr ce) {
		ce.match(
			[&](const Constant* c) {
				writeConstantReference(writer->writer, c, /*isInsideConstant*/ False);
			},
			[&](const ConcreteExpr* e) {
				writeConcreteExpr(writer, *e);
			});
	}

	void writeLocalRef(Writer* writer, const ConcreteLocal* local) {
		writeStr(writer, local->mangledName);
	}

	void writeLocalDeclaration(Writer* writer, const ConcreteLocal* local) {
		writeType(writer, local->type);
		writeStatic(writer, " ");
		writeLocalRef(writer, local);
		writeStatic(writer, ";\n\t");
	}

	void writeLocalAssignment(Writer* writer, const ConcreteLocal* local) {
		writeLocalRef(writer, local);
		writeStatic(writer, " = ");
	}

	void writeFailForType(Writer* writer, const ConcreteType type) {
		writeStatic(writer, "_fail");
		if (type.isPointer)
			writeStatic(writer, "VoidPtr");
		else
			writeStr(writer, type.strukt->mangledName);
		writeStatic(writer, "()");
	}

	void writeMatch(WriterWithIndent* writer, const ConcreteExpr ce, const ConcreteExpr::Match e) {
		// matched1 = bar(foo),
		// (matched1.kind == 0 ? 42 : (s = matched1.as_some, s.value))
		// TODO: for safety, hard-fail if the union kind is wrong
		const ConcreteLocal* matchedLocal = e.matchedLocal;
		writeChar(writer, '(');
		writeLocalAssignment(writer->writer, matchedLocal);
		writeConcreteExpr(writer, *e.matchedValue);
		writeChar(writer, ',');
		indent(writer);
		zipWithIndex(
			e.matchedUnionMembers(),
			e.cases,
			[&](const ConcreteType member, const ConcreteExpr::Match::Case kase, const size_t i) {
				const Str memberName = member.strukt->mangledName;
				writeLocalRef(writer->writer, matchedLocal);
				writeStatic(writer, ".kind == ");
				writeNat(writer->writer, i);
				newline(writer);
				writeStatic(writer, "? ");
				if (has(kase.local)) {
					writeStatic(writer, "(");
					writeLocalAssignment(writer->writer, force(kase.local));
					writeLocalRef(writer->writer, matchedLocal);
					writeStatic(writer, ".as_");
					writeStr(writer, memberName);
					writeStatic(writer, ",");
					newline(writer);
				}
				writeConstantOrExpr(writer, kase.then);
				newline(writer);
				if (has(kase.local))
					writeStatic(writer, ")");
				writeStatic(writer, ": ");
			});
		writeFailForType(writer->writer, ce.typeWithoutKnownLambdaBody());
		decrIndent(writer);
		writeChar(writer, ')');
	}

	void writeFieldAccess(
		WriterWithIndent* writer,
		const Bool targetIsPointer,
		const ConstantOrExpr target,
		const ConcreteField* field
	) {
		writeConstantOrExpr(writer, target);
		writeStatic(writer, targetIsPointer ? "->" : ".");
		writeStr(writer, field->mangledName);
	}

	void writeCallOperator(
		WriterWithIndent* writer,
		const BuiltinFunInfo bf,
		__attribute__((unused)) const Arr<const ConcreteType> typeArgs,
		const ConcreteExpr ce,
		const ConcreteExpr::Call e
	) {
		auto writeArg = [&](const size_t index) -> void {
			writeConstantOrExpr(writer, at(e.args, index));
		};

		auto binaryOperatorWorker = [&](const Str _operator) -> void {
			assert(size(e.args) == 2);
			writeArg(0);
			writeStatic(writer, " ");
			writeStr(writer, _operator);
			writeStatic(writer, " ");
			writeArg(1);
		};

		auto binaryOperator = [&](const CStr s) -> void {
			binaryOperatorWorker(strLiteral(s));
		};

		auto unaryOperatorWorker = [&](const Str _operator) -> void {
			assert(size(e.args) == 1);
			writeStr(writer, _operator);
			writeStatic(writer, "(");
			writeArg(0);
			writeStatic(writer, ")");
		};

		auto unaryOperator = [&](const CStr s) -> void {
			unaryOperatorWorker(strLiteral(s));
		};

		switch (bf.kind) {
			case BuiltinFunKind::_and:
				binaryOperator("&&");
				break;

			case BuiltinFunKind::as:
			case BuiltinFunKind::asNonConst:
				writeArg(0);
				break;

			case BuiltinFunKind::bitwiseAndNat16:
			case BuiltinFunKind::bitwiseAndNat32:
			case BuiltinFunKind::bitwiseAndNat64:
				binaryOperator("&");
				break;

			case BuiltinFunKind::callFunPtr:
				writeArg(0);
				writeStatic(writer, "(");
				for (const size_t i : Range{1, size(e.args)}) {
					if (i != 1)
						writeStatic(writer, ", ");
					writeArg(i);
				}
				writeStatic(writer, ")");
				break;

			case BuiltinFunKind::getCtx:
				writeStatic(writer, "ctx");
				break;

			case BuiltinFunKind::_if:
				writeStatic(writer, "(");
				// If the condition is a constant, we should have only compiled 'then' or 'else'.
				assert(first(e.args).isConcreteExpr());
				writeArg(0);
				writeStatic(writer, " ? ");
				writeArg(1);
				writeStatic(writer, " : ");
				writeArg(2);
				writeStatic(writer, ")");
				break;

			case BuiltinFunKind::_not:
				unaryOperator("!");
				break;

			case BuiltinFunKind::_or:
				binaryOperator("||");
				break;

			// TODO: int operators don't wrap in c++!
			case BuiltinFunKind::wrapAddInt64:
				binaryOperator("+");
				break;
			case BuiltinFunKind::wrapSubInt64:
				binaryOperator("-");
				break;
			case BuiltinFunKind::wrapMulInt64:
				binaryOperator("*");
				break;

			case BuiltinFunKind::addPtr:
			case BuiltinFunKind::addFloat64:
			case BuiltinFunKind::wrapAddNat64:
			case BuiltinFunKind::wrapAddNat32:
			case BuiltinFunKind::wrapAddNat16:
				binaryOperator("+");
				break;

			case BuiltinFunKind::subFloat64:
			case BuiltinFunKind::subPtrNat:
			case BuiltinFunKind::wrapSubNat64:
				binaryOperator("-");
				break;

			case BuiltinFunKind::mulFloat64:
			case BuiltinFunKind::wrapMulNat64:
				binaryOperator("*");
				break;

			case BuiltinFunKind::asAnyPtr:
			case BuiltinFunKind::toIntFromInt16:
			case BuiltinFunKind::toIntFromInt32:
			case BuiltinFunKind::toNatFromNat16:
			case BuiltinFunKind::toNatFromNat32:
			case BuiltinFunKind::toNatFromPtr:
				writeCastToType(writer->writer, e.returnType());
				writeArg(0);
				break;

			case BuiltinFunKind::unsafeDivFloat64:
			case BuiltinFunKind::unsafeDivInt64:
			case BuiltinFunKind::unsafeDivNat64:
				binaryOperator("/");
				break;

			case BuiltinFunKind::unsafeModNat64:
				binaryOperator("%");
				break;

			case BuiltinFunKind::unsafeNat64ToInt64:
			case BuiltinFunKind::unsafeNat64ToNat32:
			case BuiltinFunKind::unsafeInt64ToNat64:
				writeCastToType(writer->writer, e.returnType());
				writeArg(0);
				break;

			case BuiltinFunKind::deref:
				unaryOperator("*");
				break;

			case BuiltinFunKind::ptrCast:
				writeCastToType(writer->writer, ce.typeWithoutKnownLambdaBody());
				writeArg(0);
				break;

			case BuiltinFunKind::ptrTo:
			case BuiltinFunKind::refOfVal:
				writeStatic(writer, "(&(");
				writeArg(0);
				writeStatic(writer, "))");
				break;

			case BuiltinFunKind::setPtr:
				// ((*(p) = v), 0)
				writeStatic(writer, "((*(");
				writeArg(0);
				writeStatic(writer, ") = ");
				writeArg(1);
				writeStatic(writer, "), 0)");
				break;

			default:
				printf("writeToC: unhandled BuiltinFunKind: %d\n", static_cast<int>(bf.kind));
				assert(0);
		}
	}

	void writeArgsWithCtx(WriterWithIndent* writer, const Arr<const ConstantOrExpr> args) {
		writeStatic(writer, "(ctx");
		writeWithCommas(writer->writer, args, /*leadingComma*/ True, [&](const ConstantOrExpr e) {
			writeConstantOrExpr(writer, e);
		});
		writeStatic(writer, ")");
	}

	void writeArgsNoCtxNoParens(WriterWithIndent* writer, const Arr<const ConstantOrExpr> args) {
		writeWithCommas(writer->writer, args, /*leadingComma*/ False, [&](const ConstantOrExpr e) {
			writeConstantOrExpr(writer, e);
		});
	}

	void writeArgsNoCtx(WriterWithIndent* writer, const Arr<const ConstantOrExpr> args) {
		writeStatic(writer, "(");
		writeArgsNoCtxNoParens(writer, args);
		writeStatic(writer, ")");
	}

	void writeArgsNoCtxWithBraces(WriterWithIndent* writer, const Arr<const ConstantOrExpr> args) {
		writeStatic(writer, "{");
		writeArgsNoCtxNoParens(writer, args);
		writeStatic(writer, "}");
	}

	void writeArgsWithOptionalCtx(WriterWithIndent* writer, const Bool needsCtx, const Arr<const ConstantOrExpr> args) {
		if (needsCtx)
			writeArgsWithCtx(writer, args);
		else
			writeArgsNoCtx(writer, args);
	}

	const Bool isParameterlessExternFun(const ConcreteFun* fun) {
		return _and(
			fun->body().isExtern(),
			fun->arityExcludingCtxAndClosure() == 0);
	}

	void writeCall(WriterWithIndent* writer, const ConcreteExpr ce, const ConcreteExpr::Call e) {
		auto call = [&]() -> void {
			writeStr(writer, e.called->mangledName());
			writeArgsWithOptionalCtx(writer, e.called->needsCtx, e.args);
		};

		const ConcreteFunBody calledBody = e.called->body();
		if (calledBody.isBuiltin()) {
			const ConcreteFunBody::Builtin builtin = calledBody.asBuiltin();
			const BuiltinFunInfo bf = builtin.builtinInfo;
			switch (bf.emit) {
				case BuiltinFunEmit::special:
					call();
					break;
				case BuiltinFunEmit::_operator:
					writeCallOperator(writer, bf, builtin.typeArgs, ce, e);
					break;
				case BuiltinFunEmit::constant:
				case BuiltinFunEmit::generate:
					unreachable<void>();
				default:
					assert(0);
			}
		} else if (isParameterlessExternFun(e.called))
			// Extern 0-argument function is a global
			// (TODO: maybe not always?)
			writeStr(writer, e.called->mangledName());
		else
			call();
	}

	void writeNewIfaceImpl(WriterWithIndent* writer, const ConcreteExpr::NewIfaceImpl e) {
		unused(writer);
		unused(e);
		todo<void>("writenewifaceimpl");
	}

	//TODO:MOVE?
	const ConcreteType getMemberType(const ConcreteExpr ce, const ConcreteExpr::ImplicitConvertToUnion e) {
		return at(ce.typeWithoutKnownLambdaBody().strukt->body().asUnion().members, e.memberIndex);
	}

	void writeConcreteExpr(WriterWithIndent* writer, const ConcreteExpr ce) {
		const ConcreteType type = force(ce.typeWithKnownLambdaBody());

		ce.match(
			[](const ConcreteExpr::Bogus) {
				unreachable<void>();
			},
			[&](const ConcreteExpr::Alloc e) {
				const ConcreteFun* alloc = e.alloc;
				assert(alloc->needsCtx); // The allocator function uses the ctx to allocate
				writeStatic(writer, "_init");
				writeStr(writer->writer, type.mustBePointer()->mangledName);
				writeStatic(writer, "(");
				writeStr(writer, alloc->mangledName());
				writeStatic(writer, "(ctx, ");
				writeNat(writer->writer, type.mustBePointer()->sizeBytes());
				writeStatic(writer, "), ");
				writeConcreteExpr(writer, *e.inner);
				writeStatic(writer, ")");
			},
			[&](const ConcreteExpr::Call e) {
				writeCall(writer, ce, e);
			},
			[&](const ConcreteExpr::Cond e) {
				writeConcreteExpr(writer, *e.cond);
				indent(writer);
				writeStatic(writer, "? ");
				writeConstantOrExpr(writer, e.then);
				newline(writer);
				writeStatic(writer, ": ");
				writeConstantOrExpr(writer, e.elze);
				decrIndent(writer);
			},
			[&](const ConcreteExpr::CreateArr) {
				todo<void>("how to alloc array?");
			},
			[&](const ConcreteExpr::CreateRecord e) {
				writeCastToType(writer->writer, type);
				if (isEmpty(e.args))
					// C forces structs to be non-empty
					writeStatic(writer->writer, "{ 0 }");
				else
					writeArgsNoCtxWithBraces(writer, e.args);
			},
			[&](const ConcreteExpr::ImplicitConvertToUnion e) {
				writeCastToType(writer->writer, type);
				writeStatic(writer, "{ ");
				// write member index, then member
				writeNat(writer->writer, e.memberIndex);
				writeStatic(writer, ", .as_");
				writeStr(writer, getMemberType(ce, e).strukt->mangledName);
				writeStatic(writer, " = ");
				writeConstantOrExpr(writer, e.arg);
				writeStatic(writer, " }");
			},
			[&](const ConcreteExpr::Lambda e) {
				const KnownLambdaBody* klb = force(ce.knownLambdaBody());
				writeCastToType(writer->writer, force(klb->closureType()));
				writeArgsNoCtxWithBraces(writer, e.closureInit);
			},
			[&](const ConcreteExpr::LambdaToDynamic e) {
				const Arr<const ConcreteField> fields = type.strukt->body().asRecord().fields;
				assert(size(fields) == 2);
				const ConcreteField funPtrField = at(fields, 0);
				const ConcreteField dataPtrField = at(fields, 1);

				writeCastToType(writer->writer, type);
				writeStatic(writer, "{");
				indent(writer);
				writeCastToType(writer->writer, funPtrField.type);
				writeStr(writer, e.fun->mangledName());
				writeStatic(writer, ", ");
				writeCastToType(writer->writer, dataPtrField.type);
				writeConstantOrExpr(writer, e.closure);
				writeStatic(writer, " }");
			},
			[&](const ConcreteExpr::Let e) {
				// ((x = value),
				// then)
				writeStatic(writer, "((");
				writeLocalAssignment(writer->writer, e.local);
				writeConcreteExpr(writer, *e.value);
				writeStatic(writer, "),");
				newline(writer);
				writeConstantOrExpr(writer, e.then);
				writeStatic(writer, ")");
			},
			[&](const ConcreteExpr::LocalRef e) {
				writeLocalRef(writer->writer, e.local);
			},
			[&](const ConcreteExpr::Match e) {
				writeMatch(writer, ce, e);
			},
			[&](const ConcreteExpr::MessageSend e) {
				writeConstantOrExpr(writer, e.target);
				writeStatic(writer, ".");
				writeStr(writer, e.message->mangledName);
				writeArgsWithCtx(writer, e.args);
			},
			[&](const ConcreteExpr::NewIfaceImpl e) {
				writeNewIfaceImpl(writer, e);
			},
			[&](const ConcreteExpr::ParamRef e) {
				writeStr(writer, e.param->mangledName);
			},
			[&](const ConcreteExpr::RecordFieldAccess e) {
				writeFieldAccess(writer, e.targetIsPointer, e.target, e.field);
			},
			[&](const ConcreteExpr::RecordFieldSet e) {
				// (s.x = v), 0
				writeStatic(writer, "(");
				writeFieldAccess(writer, e.targetIsPointer, ConstantOrExpr{e.target}, e.field);
				writeStatic(writer, " = ");
				writeConstantOrExpr(writer, e.value);
				writeStatic(writer, "), 0");
			},
			[&](const ConcreteExpr::Seq e) {
				// (a,
				// b)
				writeChar(writer, '(');
				writeConcreteExpr(writer, *e.first);
				writeChar(writer, ',');
				newline(writer);
				writeConstantOrExpr(writer, e.then);
				writeChar(writer, ')');
			},
			[&](const ConcreteExpr::SpecialUnary e) {
				switch (e.kind) {
					case ConcreteExpr::SpecialUnary::Kind::deref:
						writeStatic(writer, "*(");
						writeConcreteExpr(writer, *e.arg);
						writeChar(writer, ')');
						break;
					default:
						assert(0);
				}
			},
			[&](const ConcreteExpr::SpecialBinary e) {
				writeStatic(writer, "(");
				writeConstantOrExpr(writer, e.left);
				using Kind = ConcreteExpr::SpecialBinary::Kind;
				writeChar(writer, ' ');
				writeStatic(writer, [&]() {
					switch (e.kind) {
						case Kind::add:
							return "+";
						case Kind::eq:
							return "==";
						case Kind::less:
							return "<";
						case Kind::_or:
							return "||";
						case Kind::sub:
							return "-";
						default:
							assert(0);
					}
				}());
				writeChar(writer, ' ');
				writeConstantOrExpr(writer, e.right);
				writeStatic(writer, ")");
			});
	}

	void writeSigParams(
		Writer* writer,
		const Bool needsCtx,
		const Opt<const ConcreteParam> closure,
		const Arr<const ConcreteParam> params
	) {
		writeChar(writer, '(');
		if (needsCtx)
			writeStatic(writer, "ctx* ctx");
		if (has(closure)) {
			if (needsCtx)
				writeStatic(writer, ", ");
			doWriteParam(writer, force(closure));
		}
		writeJustParams(writer, _or(needsCtx, has(closure)), params);
	}

	void writeFunReturnTypeNameAndParams(Writer* writer, const ConcreteFun* fun) {
		writeType(writer, fun->returnType());
		writeChar(writer, ' ');
		writeStr(writer, fun->mangledName());
		if (!isParameterlessExternFun(fun))
			writeSigParams(writer, fun->needsCtx, fun->closureParam, fun->paramsExcludingCtxAndClosure());
	}

	void writeConcreteFunDeclaration(Writer* writer, const ConcreteFun* fun) {
		//TODO:HAX: printf apparently *must* be declared as variadic
		if (strEqLiteral(fun->mangledName(), "printf"))
			writeStatic(writer, "int printf(const char* format, ...);\n");
		else {
			if (fun->body().isExtern())
				writeStatic(writer, "extern ");
			writeFunReturnTypeNameAndParams(writer, fun);
			writeStatic(writer, ";\n");
		}
	}

	template <typename CbWriteBody>
	void writeFunWithBodyWorker(Writer* writer, const ConcreteFun* fun, CbWriteBody cbWriteBody) {
		writeFunReturnTypeNameAndParams(writer, fun);
		writeStatic(writer, " {\n\t");
		cbWriteBody();
		writeStatic(writer, "\n}\n");
	}

	void declareLocals(Writer* writer, const Arr<const ConcreteLocal*> locals) {
		for (const ConcreteLocal* local : locals)
			writeLocalDeclaration(writer, local);
	}

	void writeFunWithConstantBody(Writer* writer, const ConcreteFun* fun, const Constant* body) {
		writeFunWithBodyWorker(writer, fun, [&]() {
			writeStatic(writer, "return ");
			writeConstantReference(writer, body, /*isInsideExpr*/ False);
			writeStatic(writer, ";");
		});
	}

	void writeFunWithExprBody(Writer* writer, const ConcreteFun* fun, const ConcreteFunExprBody body) {
		writeFunWithBodyWorker(writer, fun, [&]() {
			declareLocals(writer, body.allLocals);
			writeStatic(writer, "return ");
			WriterWithIndent writerWithIndent { writer, 1 };
			writeConcreteExpr(&writerWithIndent, *body.expr);
			writeStatic(writer, ";");
		});
	}

	void writeSpecialBody(Writer* writer, const ConcreteFun* fun, const BuiltinFunKind kind) {
		switch (kind) {
			case BuiltinFunKind::compareExchangeStrong: {
				writeStatic(writer, "return atomic_compare_exchange_strong(");
				const Arr<const ConcreteParam> params = fun->paramsExcludingCtxAndClosure();
				assert(size(params) == 3);
				writeStr(writer, at(params, 0).mangledName);
				writeStatic(writer, ", ");
				writeStr(writer, at(params, 1).mangledName);
				writeStatic(writer, ", ");
				writeStr(writer, at(params, 2).mangledName);
				writeStatic(writer, ");");
				break;
			}

			case BuiltinFunKind::getErrno: {
				writeStatic(writer, "return errno;");
			}

			case BuiltinFunKind::hardFail:
				writeStatic(writer, "assert(0);");
				break;

			default:
				printf("Other special? %d\n", static_cast<int>(kind));
				assert(0);
		}
	}

	void writeConcreteFunDefinition(Writer* writer, const ConcreteFun* fun) {
		fun->body().match(
			[](const ConcreteFunBody::Bogus) {
				unreachable<void>();
			},
			[&](const ConcreteFunBody::Builtin builtin) {
				const BuiltinFunInfo info = builtin.builtinInfo;
				switch (info.emit) {
					case BuiltinFunEmit::generate:
						// A 'generate' builtin should have given us a ConcreteExpr body.
						unreachable<void>();
						break;
					case BuiltinFunEmit::_operator:
						// will emit it inline
						break;
					case BuiltinFunEmit::constant:
						// those tagged like this should always evaluate to constants
						unreachable<void>();
						break;
					case BuiltinFunEmit::special:
						writeFunWithBodyWorker(writer, fun, [&]() {
							writeSpecialBody(writer, fun, info.kind);
						});
						break;
					default:
						assert(0);
				}
			},
			// Already declared it, nothing more to do
			[&](const ConcreteFunBody::Extern) {},
			[&](const Constant* c) {
				writeFunWithConstantBody(writer, fun, c);
			},
			[&](const ConcreteFunExprBody b) {
				writeFunWithExprBody(writer, fun, b);
			});
	}

	void writeInitAndFailFuns(Writer* writer, const Arr<const ConcreteStruct*> allStructs) {
		//TODO: only do 'init' for structs that will be allocated
		//TODO: only do 'fail' for structs returned from a match by value
		for (const ConcreteStruct* strukt : allStructs) {
			auto writeType = [&]() -> void {
				writeValueType(writer, strukt);
			};
			writeStatic(writer, "\n");
			writeType();
			writeStatic(writer,"* _init");
			writeStr(writer, strukt->mangledName);
			writeStatic(writer, "(byte* out, ");
			writeType();
			writeStatic(writer, " value) {\n\t");
			writeType();
			writeStatic(writer, "* res = (");
			writeType();
			writeStatic(writer, "*) out;\n\t*res = value;\n\treturn res;\n}\n");

			writeType();
			writeStatic(writer, " _fail");
			writeStr(writer, strukt->mangledName);
			writeStatic(writer, "() {\n\tassert(0);\n}\n\n");
		}

		writeStatic(writer, "void* _failVoidPtr() { assert(0); }");
	}

	void writeMain(Writer* writer, const Arr<const ConcreteStruct*> allStructs) {
		writeStatic(writer, "\n\nint main() {");
		for (const ConcreteStruct* strukt : allStructs) {
			writeStatic(writer, "\n\tassert(sizeof(");
			writeStr(writer, strukt->mangledName);
			writeStatic(writer, ") == ");
			writeNat(writer, strukt->sizeBytes());
			writeStatic(writer, ");");
		}
		writeStatic(writer,"\n\treturn (int) main___int();\n}\n");
	}
}

const Str writeToC(Arena* arena, const ConcreteProgram program) {
	Writer writer { arena };

	//writeStatic(writer, "#include <stdatomic.h>\n"); // compare_exchange_strong
	writeStatic(&writer, "#include <assert.h>\n");
	writeStatic(&writer, "#include <errno.h>\n");
	writeStatic(&writer, "#include <stdatomic.h>\n");
	writeStatic(&writer, "#include <stdint.h>\n");

	writeStructs(&writer, program.allStructs);
	writeConstants(&writer, program.allConstants);
	writeInitAndFailFuns(&writer, program.allStructs);

	for (const ConcreteExpr::NewIfaceImpl impl : program.allNewIfaceImpls)
		writeNewIfaceImpl(&writer, impl);

	for (const ConcreteFun* fun : program.allFuns)
		writeConcreteFunDeclaration(&writer, fun);

	for (const ConcreteFun* fun : program.allFuns)
		writeConcreteFunDefinition(&writer, fun);

	writeMain(&writer, program.allStructs);
	return finishWriter(&writer);
}
