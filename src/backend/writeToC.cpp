#include "./writeToC.h"

#include "../concretize/concretizeUtil.h" // writeConcreteTypeForMangle
#include "../util/arrUtil.h"
#include "../util/writer.h"

namespace {
	void writeQuotedString(Writer& writer, const Arr<const Constant*> chars) {
		writeChar(writer, '"');
		for (const Constant* c : chars)
			writeEscapedChar(writer, c->kind.asChar());
		writeChar(writer, '"');
	}

	void writeValueType(Writer& writer, const ConcreteStruct* s) {
		writeStr(writer, s->mangledName);
	}

	void writeValueType(Writer& writer, const ConcreteType t) {
		writeValueType(writer, t.mustBeNonPointer());
	}

	void writeType(Writer& writer, const ConcreteType t) {
		writeValueType(writer, t.strukt);
		if (t.isPointer)
			writeChar(writer, '*');
	}

	void doWriteParam(Writer& writer, const ConcreteParam p) {
		writeType(writer, p.type);
		writeChar(writer, ' ');
		writeStr(writer, p.mangledName);
	}

	void writeJustParams(Writer& writer, const Bool wroteFirst, const Arr<const ConcreteParam> params) {
		Cell<const Bool> didWriteFirst { wroteFirst };
		for (const ConcreteParam p : params) {
			if (didWriteFirst.get())
				writeStatic(writer, ", ");
			else
				didWriteFirst.set(True);
			doWriteParam(writer, p);
		}
		writeChar(writer, ')');
	}

	void writeJustParamsAlwaysComma(Writer& writer, const Arr<const ConcreteParam> params) {
		for (const ConcreteParam p : params) {
			writeStatic(writer, ", ");
			doWriteParam(writer, p);
		}
	}

	void writeBuiltinAliasForStruct(Writer& writer, const Str name, const BuiltinStructKind kind, const Arr<const ConcreteType> typeArgs) {
		// "bool" and "char" share the same name as in C, so no need for an alias.
		if (kind != BuiltinStructKind::_bool && kind != BuiltinStructKind::_char) {
			writeStatic(writer, "using ");
			writeStr(writer, name);
			writeStatic(writer, " = ");
			switch (kind) {
				case BuiltinStructKind::byte:
					writeStatic(writer, "uint8_t");
					break;
				case BuiltinStructKind::float64:
					writeStatic(writer, "double");
					break;
				case BuiltinStructKind::int64:
					writeStatic(writer, "int64_t");
					break;
				case BuiltinStructKind::funPtrN:
					writeType(writer, first(typeArgs));
					writeStatic(writer, " (*)(");
					for (const size_t i : Range{1, typeArgs.size}) {
						if (i != 1)
							writeStatic(writer, ", ");
						writeType(writer, at(typeArgs, i));
					}
					writeChar(writer, ')');
					break;
				case BuiltinStructKind::nat64:
					writeStatic(writer, "uint64_t");
					break;
				case BuiltinStructKind::ptr:
					writeType(writer, at(typeArgs, 0));
					writeChar(writer, '*');
					break;
				case BuiltinStructKind::_void:
					writeStatic(writer, "uint8_t");
					break;
				case BuiltinStructKind::_bool:
				case BuiltinStructKind::_char:
					unreachable<void>();
				default:
					assert(0);
			}

			writeStatic(writer, ";\n");
		}
	}

	void writeStructHead(Writer& writer, const Str mangledName) {
		writeStatic(writer, "struct ");
		writeStr(writer, mangledName);
		writeStatic(writer, " {");
	}

	void writeStructEnd(Writer& writer) {
		writeStatic(writer, "\n};\n");
	}

	void writeFieldsStruct(Writer& writer, const Str mangledName, const Arr<const ConcreteField> fields) {
		writeStructHead(writer, mangledName);
		if (isEmpty(fields))
			writeStatic(writer, "};\n");
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

	void writeUnionStruct(Writer& writer, const Str mangledName, const Arr<const ConcreteType> members) {
		writeStructHead(writer, mangledName);

		auto writeMemberName = [&](const ConcreteType member) -> void {
			writeStr(writer, member.strukt->mangledName);
		};

		writeStatic(writer, "\n\tenum class Kind {");
		for (const ConcreteType member : members) {
			writeStatic(writer, "\n\t\t");
			writeMemberName(member);
			writeChar(writer, ',');
		}
		writeStatic(writer, "\n\t};");
		writeStatic(writer, "\n\tKind kind;");
		writeStatic(writer, "\n\tunion {");
		for (const ConcreteType member : members) {
			writeStatic(writer, "\n\t\t");
			writeType(writer, member);
			writeStatic(writer, " as_");
			writeMemberName(member);
			writeChar(writer, ';');
		}
		writeStatic(writer, "\n\t};");

		for (const ConcreteType member : members) {
			writeStatic(writer, "\n\t");
			writeStr(writer, mangledName);
			writeChar(writer, '(');
			writeType(writer, member);
			writeStatic(writer, " value) : kind{Kind::");
			writeMemberName(member);
			writeStatic(writer, "}, as_");
			writeMemberName(member);
			writeStatic(writer, "{value} {}");
		}

		writeStructEnd(writer);
	}

	void writeIfaceStruct(Writer& writer, const Str mangledName, const Arr<const ConcreteSig> messages) {
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

	using StructStates = MutDict<const ConcreteStruct*, const StructState, comparePointer<const ConcreteStruct>>;

	const Bool canReferenceType(const ConcreteType t, const StructStates& structStates) {
		const Opt<const StructState> state = structStates.get(t.strukt);
		if (state.has())
			switch (state.force()) {
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

	const Bool sigCanReferenceTypes(const ConcreteSig s, const StructStates& structStates) {
		return _and(
			canReferenceType(s.returnType, structStates),
			every(s.params, [&](const ConcreteParam p) {
				return canReferenceType(p.type, structStates);
			}));
	}

	void declareStruct(Writer& writer, const ConcreteStruct* strukt) {
		writeStatic(writer, "struct ");
		writeStr(writer, strukt->mangledName);
		writeStatic(writer, ";\n");
	}

	// Returns any new work it did -- if we declared or defined the struct
	const Opt<const StructState> writeStructDeclarationOrDefinition(Writer& writer, const ConcreteStruct* strukt, const StructStates& structStates) {
		auto declare = [&]() -> const Opt<const StructState> {
			if (!structStates.has(strukt)) {
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
			[&](const ConcreteStructBody::Fields f) {
				if (every(f.fields, [&](const ConcreteField f) { return canReferenceType(f.type, structStates); })) {
					writeFieldsStruct(writer, strukt->mangledName, f.fields);
					return defined;
				} else
					return declare();
			},
			[&](const ConcreteStructBody::Union u) {
				if (every(u.members, [&](const ConcreteType t) { return canReferenceType(t, structStates); })) {
					writeUnionStruct(writer, strukt->mangledName, u.members);
					return defined;
				} else
					return declare();
			},
			[&](const ConcreteStructBody::Iface i) {
				if (every(i.messages, [&](const ConcreteSig s) { return sigCanReferenceTypes(s, structStates); })) {
					writeIfaceStruct(writer, strukt->mangledName, i.messages);
					return defined;
				} else
					return declare();
			});
	}

	void writeStructs(Writer& writer, const Arr<const ConcreteStruct*> allStructs) {
		Arena tempArena {};
		StructStates structStates {};
		for (;;) {
			Cell<const Bool> madeProgress { False };
			Cell<const Bool> someIncomplete { False };
			for (const ConcreteStruct* strukt : allStructs) {
				const Opt<const StructState> curState = structStates.get(strukt);
				if (!curState.has() || curState.force() != StructState::defined) {
					const Opt<const StructState> didWork = writeStructDeclarationOrDefinition(writer, strukt, structStates);
					if (didWork.has()) {
						structStates.set(tempArena, strukt, didWork.force());
						madeProgress.set(True);
					} else
						someIncomplete.set(True);
				}
			}
			if (someIncomplete.get())
				assert(madeProgress.get());
			else
				break;
		}

		writeStatic(writer, "\n");
		for (const ConcreteStruct* strukt : allStructs) {
			writeStatic(writer, "static_assert(sizeof(");
			writeStr(writer, strukt->mangledName);
			writeStatic(writer, ") == ");
			writeNat(writer, strukt->sizeBytes());
			writeStatic(writer, ", \"\");\n");
		}
		writeStatic(writer, "\n");
	}
}

namespace {
	void writeConstantRecordName(Writer& writer, const Constant* c, const ConcreteType t) {
		writeStatic(writer, "_constant__");
		writeConcreteTypeForMangle(writer, t);
		writeStatic(writer, "__");
		writeNat(writer, c->id);
	}

	void writeConstantUnionName(Writer& writer, const Constant* c, const ConcreteStruct* s) {
		writeStatic(writer, "_constant__");
		writeStr(writer, s->mangledName);
		writeStatic(writer, "__");
		writeNat(writer, c->id);
	}

	void writeConstantReference(Writer& writer, const Constant* c) {
		auto writeref = [&](const CStr s) {
			writeStatic(writer, "_constant");
			writeStatic(writer, s);
			writeNat(writer, c->id);
		};

		c->kind.match(
			[&](const ConstantKind::Array) {
				writeref("Arr");
			},
			[&](const Bool b) {
				writeStatic(writer, b ? "true" : "false");
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
			[&](const Int64 i) {
				writeInt(writer, i);
			},
			[&](const ConstantKind::Lambda l) {
				unused(l);
				// Don't need a decl for this (other than the concretefun), just create it here
				todo<void>("write reference for const lambda");
				// fun0___void{someFun, closurePtr};
			},
			[&](const Nat64 n) {
				writeNat(writer, n);
			},
			[&](const ConstantKind::Null) {
				writeStatic(writer, "0");
			},
			[&](const ConstantKind::Ptr p) {
				writeChar(writer, '&');
				writeConstantReference(writer, p.array);
				writeStatic(writer, ".data[");
				writeNat(writer, p.index);
				writeChar(writer, ']');
			},
			[&](const ConstantKind::Record r) {
				if (r.type.isPointer)
					writeChar(writer, '&');
				writeConstantRecordName(writer, c, r.type);
			},
			[&](const ConstantKind::Union u) {
				writeConstantUnionName(writer, c, u.unionType);
			},
			[&](const ConstantKind::Void) {
				// Value should be ignored, so any will do
				writeChar(writer, '0');
			});
	}

	void writeConstantReferencesWithCommas(Writer& writer, const Arr<const Constant*> constants) {
		writeWithCommas(writer, constants, /*leadingComma*/ False, [&](const Constant* c) {
			writeConstantReference(writer, c);
		});
	}

	void writeConstantDecl(Writer& writer, const Constant* c) {
		const ConcreteType type = c->type();
		c->kind.match(
			[&](const ConstantKind::Array a) {
				// static nat64 _constantArrBacking123[3] = {1, 2, 3};
				// static arr_nat64 _constantArr123 = arr_nat64{3, _constantArrBacking123};

				// Strings are special:
				// static arr_char _constantArr123 = arr_char{5, const_cast<char*>("hello")};

				const size_t size = a.size();
				const Bool isStr = first(a.elements())->kind.isChar();
				const size_t id = c->id;
				if (size != 0 && !isStr) {
					writeStatic(writer, "static ");
					writeType(writer, a.elementType());
					writeStatic(writer, " _constantArrBacking");
					writeNat(writer, id);
					writeChar(writer, '[');
					writeNat(writer, size);
					writeStatic(writer, "] = {");
					writeConstantReferencesWithCommas(writer, a.elements());
					writeStatic(writer, "};\n");
				}

				writeStatic(writer, "static ");
				writeValueType(writer, type);
				writeStatic(writer, " _constantArr");
				writeNat(writer, id);
				writeStatic(writer, " = ");
				writeValueType(writer, type);
				writeChar(writer, '{');
				writeNat(writer, size);
				writeStatic(writer, ", ");
				if (isStr) {
					writeStatic(writer, "const_cast<char*>(");
					writeQuotedString(writer, a.elements());
					writeStatic(writer, ")");
				} else {
					writeStatic(writer, "_constantArrBacking");
					writeNat(writer, id);
				}
				writeStatic(writer, "};\n");
			},
			[](const Bool) {},
			[](const char) {},
			[](const ConstantKind::FunPtr) {},
			[](const Int64) {},
			// Do nothing for lambda now -- fun is by-value. Closure is emitted as a separate constant.
			[](const ConstantKind::Lambda) {},
			[](const Nat64) {},
			[](const ConstantKind::Null) {},
			// ptr will be written inlien as `&arr.data[3]`
			[](const ConstantKind::Ptr) {},
			[&](const ConstantKind::Record r) {
				writeStatic(writer, "static ");
				writeType(writer, r.type);
				writeChar(writer, ' ');
				writeConstantRecordName(writer, c, r.type);
				writeStatic(writer, " = ");
				writeType(writer, r.type);
				writeChar(writer, '{');
				writeConstantReferencesWithCommas(writer, r.args);
				writeStatic(writer, "};\n");
			},
			[&](const ConstantKind::Union u) {
				writeStatic(writer, "static ");
				writeValueType(writer, u.unionType);
				writeChar(writer, ' ');
				writeConstantUnionName(writer, c, u.unionType);
				writeStatic(writer, " = ");
				writeValueType(writer, u.unionType);
				writeChar(writer, '(');
				writeConstantReference(writer, u.member);
				writeStatic(writer, ");\n");
			},
			[](const ConstantKind::Void) {});
	}

	void writeNewIfaceImpl(Writer& writer, const ConcreteExpr::NewIfaceImpl impl) {
		unused(writer, impl);
		todo<void>("writeNewIfaceImpl");
	}

	struct WriteExprCtx {
		Writer& writer;
		uint _indent = 1;

		void newline() {
			writeChar(writer, '\n');
			for (__attribute__((unused)) const size_t _ : Range{_indent})
				writeChar(writer, '\t');
		}

		void decrIndent() {
			assert(_indent != 0);
			_indent--;
		}

		void incrIndent() {
			_indent++;
		}

		void indent() {
			incrIndent();
			newline();
		}

		void dedent() {
			decrIndent();
			newline();
		}

		void write(const CStr text) {
			writeStatic(writer, text);
		}

		void writeStr(const Str s) {
			::writeStr(writer, s);
		}
	};

	void writeConcreteExprAsStatement(WriteExprCtx& ctx, const ConcreteExpr ex, const Bool isReturn);
	void writeConcreteExprAsExpr(WriteExprCtx& ctx, const ConcreteExpr ex);

	void writeConstantOrExprAsStatement(WriteExprCtx& ctx, const ConstantOrExpr ce, const Bool isReturn) {
		return ce.match(
			[&](const Constant* c) {
				if (isReturn) {
					ctx.write("return ");
					writeConstantReference(ctx.writer, c);
					ctx.write(";");
				}
			},
			[&](const ConcreteExpr* e) {
				writeConcreteExprAsStatement(ctx, *e, isReturn);
			});
	}

	void writeConstantOrExprAsExpr(WriteExprCtx& ctx, const ConstantOrExpr ce) {
		ce.match(
			[&](const Constant* c) {
				writeConstantReference(ctx.writer, c);
			},
			[&](const ConcreteExpr* e) {
				writeConcreteExprAsExpr(ctx, *e);
			});
	}

	void writeLocalEquals(Writer& writer, const ConcreteLocal* local) {
		writeType(writer, local->type);
		writeStatic(writer, " ");
		writeStr(writer, local->mangledName);
		writeStatic(writer, " = ");
	}

	void writeMatchAsStatement(WriteExprCtx& ctx, const ConcreteExpr::Match e, const Bool isReturn) {
		writeValueType(ctx.writer, e.matchedUnion);
		ctx.write(" matched = ");
		writeConcreteExprAsExpr(ctx, *e.matched);
		ctx.write(";");
		ctx.newline();
		ctx.write("switch (matched.kind) {");
		ctx.indent();
		zip(e.matchedUnionMembers(), e.cases, [&](const ConcreteType member, const ConcreteExpr::Match::Case kase) {
			const Str memberName = member.strukt->mangledName;
			ctx.write("case ");
			ctx.writeStr(e.matchedUnion->mangledName);
			ctx.write("::Kind::");
			ctx.writeStr(memberName);
			ctx.write(": {");
			ctx.indent();
			if (kase.local.has()) {
				writeLocalEquals(ctx.writer, kase.local.force());
				ctx.write("matched.as_");
				ctx.writeStr(memberName);
				ctx.write(";");
				ctx.newline();
			}
			writeConstantOrExprAsStatement(ctx, kase.then, isReturn);
			ctx.dedent();
			ctx.write("}");
			ctx.newline();
		});
		ctx.write("default: assert(0);");
		ctx.dedent();
		ctx.write("}");
	}

	void writeFieldAccess(WriteExprCtx& ctx, const Bool targetIsPointer, const ConstantOrExpr target, const ConcreteField* field) {
		writeConstantOrExprAsExpr(ctx, target);
		ctx.write(targetIsPointer ? "->" : ".");
		ctx.writeStr(field->mangledName);
	}

	void writeConcreteExprAsStatement(WriteExprCtx& ctx, const ConcreteExpr ex, const Bool isReturn) {
		auto expr = [&]() -> void {
			if (isReturn)
				ctx.write("return ");
			writeConcreteExprAsExpr(ctx, ex);
			ctx.write(";");
		};
		ex.match(
			[](const ConcreteExpr::Bogus) {
				unreachable<void>();
			},
			[&](const ConcreteExpr::Alloc) {
				expr();
			},
			[&](const ConcreteExpr::CallConcreteFun) {
				expr();
			},
			[&](const ConcreteExpr::Cond e) {
				ctx.write("if (");
				writeConcreteExprAsExpr(ctx, *e.cond);
				ctx.write(") {");
				ctx.indent();
				writeConstantOrExprAsStatement(ctx, e.then, isReturn);
				ctx.dedent();
				// To be pretty, use 'else if' for nested cond
				const Bool elseIf = _and(e.elze.isConcreteExpr(), e.elze.asConcreteExpr()->isCond());
				if (elseIf)
					ctx.write("} else ");
				else {
					ctx.write("} else {");
					ctx.indent();
				}
				writeConstantOrExprAsStatement(ctx, e.elze, isReturn);
				if (!elseIf) {
					ctx.dedent();
					ctx.write("}");
				}
			},
			[&](const ConcreteExpr::CreateArr) {
				expr();
			},
			[&](const ConcreteExpr::CreateRecord) {
				expr();
			},
			[&](const ConcreteExpr::ImplicitConvertToUnion) {
				expr();
			},
			[&](const ConcreteExpr::Lambda) {
				expr();
			},
			[&](const ConcreteExpr::LambdaToDynamic) {
				expr();
			},
			[&](const ConcreteExpr::Let e) {
				writeLocalEquals(ctx.writer, e.local);
				writeConcreteExprAsExpr(ctx, *e.value);
				ctx.write(";");
				ctx.newline();
				writeConstantOrExprAsStatement(ctx, e.then, isReturn);
			},
			[&](const ConcreteExpr::LocalRef) {
				expr();
			},
			[&](const ConcreteExpr::Match e) {
				writeMatchAsStatement(ctx, e, isReturn);
			},
			[&](const ConcreteExpr::MessageSend) {
				expr();
			},
			[&](const ConcreteExpr::NewIfaceImpl) {
				expr();
			},
			[&](const ConcreteExpr::ParamRef) {
				expr();
			},
			[&](const ConcreteExpr::Seq e) {
				writeConcreteExprAsStatement(ctx, *e.first, /*isReturn*/ False);
				ctx.newline();
				writeConstantOrExprAsStatement(ctx, e.then, isReturn);
			},
			[&](const ConcreteExpr::SpecialBinary) {
				expr();
			},
			[&](const ConcreteExpr::StructFieldAccess) {
				expr();
			},
			[&](const ConcreteExpr::StructFieldSet e) {
				writeFieldAccess(ctx, e.targetIsPointer, ConstantOrExpr{e.target}, e.field);
				ctx.write(" = ");
				writeConstantOrExprAsExpr(ctx, e.value);
				ctx.write(";");
				if (isReturn) {
					ctx.newline();
					ctx.write("return 0;");
				}
			});
	}

	void writeCallOperator(WriteExprCtx& ctx, const BuiltinFunInfo bf, const Arr<const ConcreteType> typeArgs, const ConcreteExpr::CallConcreteFun e) {
		auto writeArg = [&](const size_t index) -> void {
			writeConstantOrExprAsExpr(ctx, at(e.args, index));
		};

		auto binaryOperatorWorker = [&](const Str _operator) -> void {
			assert(e.args.size == 2);
			writeArg(0);
			ctx.write(" ");
			ctx.writeStr(_operator);
			ctx.write(" ");
			writeArg(1);
		};

		auto binaryOperator = [&](const CStr s) -> void {
			binaryOperatorWorker(strLiteral(s));
		};

		auto unaryOperatorWorker = [&](const Str _operator) -> void {
			assert(e.args.size == 1);
			ctx.writeStr(_operator);
			ctx.write("(");
			writeArg(0);
			ctx.write(")");
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

			case BuiltinFunKind::callFunPtr:
				writeArg(0);
				ctx.write("(");
				for (const size_t i : Range{1, e.args.size}) {
					if (i != 1)
						ctx.write(", ");
					writeArg(i);
				}
				ctx.write(")");
				break;

			case BuiltinFunKind::getCtx:
				ctx.write("ctx");
				break;

			case BuiltinFunKind::_if:
				ctx.write("(");
				writeArg(0);
				ctx.write(" ? ");
				writeArg(1);
				ctx.write(" : ");
				writeArg(2);
				ctx.write(")");
				break;

			case BuiltinFunKind::_not:
				unaryOperator("!");
				break;

			case BuiltinFunKind::_or:
				binaryOperator("||");
				break;

			case BuiltinFunKind::wrappingAddInt64:
			case BuiltinFunKind::wrappingSubInt64:
			case BuiltinFunKind::wrappingMulInt64:
				todo<void>("this isn't wrapping in c++!");

			case BuiltinFunKind::addPtr:
			case BuiltinFunKind::addFloat64:
			case BuiltinFunKind::wrappingAddNat64:
				binaryOperator("+");
				break;

			case BuiltinFunKind::subFloat64:
			case BuiltinFunKind::wrappingSubNat64:
				binaryOperator("-");
				break;

			case BuiltinFunKind::mulFloat64:
			case BuiltinFunKind::wrappingMulNat64:
				binaryOperator("*");
				break;

			case BuiltinFunKind::unsafeDivFloat64:
			case BuiltinFunKind::unsafeDivInt64:
			case BuiltinFunKind::unsafeDivNat64:
				binaryOperator("/");
				break;

			case BuiltinFunKind::unsafeModNat64:
				binaryOperator("%");
				break;

			case BuiltinFunKind::deref:
				unaryOperator("*");
				break;

			case BuiltinFunKind::ptrCast:
				ctx.write("reinterpret_cast<");
				writeType(ctx.writer, first(typeArgs));
				ctx.write("*>(");
				writeArg(0);
				ctx.write(")");
				break;

			case BuiltinFunKind::refOfVal:
				ctx.write("&(");
				writeArg(0);
				ctx.write(")");
				break;

			case BuiltinFunKind::setPtr:
				// TODO: this is ugly in the usual case that we're writing as a statement. Have a writeCallConcreteFunAsStatement too.
				// [&]() { *(p) = v; return 0; }()
				ctx.write("[&]() { *(");
				writeArg(0);
				ctx.write(") = ");
				writeArg(1);
				ctx.write("; return 0; }()");
				break;

			default:
				printf("Unhandled BuiltinFunKind: %d\n", bf.kind);
				assert(0);
		}
	}

	void writeArgsWithCtx(WriteExprCtx& ctx, const Arr<const ConstantOrExpr> args) {
		ctx.write("(ctx");
		writeWithCommas(ctx.writer, args, /*leadingComma*/ True, [&](const ConstantOrExpr e) {
			writeConstantOrExprAsExpr(ctx, e);
		});
		ctx.write(")");
	}

	void writeArgsNoCtxNoParens(WriteExprCtx& ctx, const Arr<const ConstantOrExpr> args) {
		writeWithCommas(ctx.writer, args, /*leadingComma*/ False, [&](const ConstantOrExpr e) {
			writeConstantOrExprAsExpr(ctx, e);
		});
	}

	void writeArgsNoCtx(WriteExprCtx& ctx, const Arr<const ConstantOrExpr> args) {
		ctx.write("(");
		writeArgsNoCtxNoParens(ctx, args);
		ctx.write(")");
	}

	void writeArgsNoCtxWithBraces(WriteExprCtx& ctx, const Arr<const ConstantOrExpr> args) {
		ctx.write("{");
		writeArgsNoCtxNoParens(ctx, args);
		ctx.write("}");
	}

	void writeArgsWithOptionalCtx(WriteExprCtx& ctx, const Bool needsCtx, const Arr<const ConstantOrExpr> args) {
		if (needsCtx)
			writeArgsWithCtx(ctx, args);
		else
			writeArgsNoCtx(ctx, args);
	}

	void writeCallConcreteFunAsExpr(WriteExprCtx& ctx, const ConcreteExpr::CallConcreteFun e) {
		auto call = [&]() -> void {
			ctx.writeStr(e.called->mangledName());
			writeArgsWithOptionalCtx(ctx, e.called->needsCtx, e.args);
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
					writeCallOperator(ctx, bf, builtin.typeArgs, e);
					break;
				case BuiltinFunEmit::constant:
				case BuiltinFunEmit::generate:
					unreachable<void>();
				default:
					assert(0);
			}
		} else
			call();
	}

	void writeNewIfaceImplAsExpr(WriteExprCtx& ctx, const ConcreteExpr::NewIfaceImpl e) {
		unused(ctx, e);
		todo<void>("writenewifaceimplasexpr");
	}

	void writeConcreteExprAsExpr(WriteExprCtx& ctx, const ConcreteExpr ce) {
		auto iife = [&]() -> void {
			ctx.write("[&]() {");
			ctx.indent();
			writeConcreteExprAsStatement(ctx, ce, /*isReturn*/ True);
			ctx.dedent();
			ctx.write("}()");
		};

		const ConcreteType type = ce.typeWithKnownLambdaBody().force();

		ce.match(
			[](const ConcreteExpr::Bogus) {
				unreachable<void>();
			},
			[&](const ConcreteExpr::Alloc e) {
				const ConcreteFun* alloc = e.alloc;
				assert(alloc->needsCtx); // The allocator function uses the ctx to allocate
				ctx.write("_alloc<");
				writeValueType(ctx.writer, type.mustBePointer());
				ctx.write(">(");
				ctx.writeStr(alloc->mangledName());
				ctx.write("(ctx, ");
				writeNat(ctx.writer, type.mustBePointer()->sizeBytes());
				ctx.write("), ");
				writeConcreteExprAsExpr(ctx, *e.inner);
				ctx.write(")");
			},
			[&](const ConcreteExpr::CallConcreteFun e) {
				writeCallConcreteFunAsExpr(ctx, e);
			},
			[&](const ConcreteExpr::Cond e) {
				writeConcreteExprAsExpr(ctx, *e.cond);
				ctx.write(" ? ");
				writeConstantOrExprAsExpr(ctx, e.then);
				ctx.write(" : ");
				writeConstantOrExprAsExpr(ctx, e.elze);
			},
			[&](const ConcreteExpr::CreateArr) {
				/*
				D version was:
				arr__nat64 a = arr__nat64(
					3,
					_alloc<nat64[3]>(
						allocate_bytes__ptr__byte__nat64(ctx, nat64.sizeof * 3),
						cast(nat64[3]) [1, 2, 3]
					).ptr);
				*/
				todo<void>("how to alloc array in c++?");
			},
			[&](const ConcreteExpr::CreateRecord e) {
				writeValueType(ctx.writer, type.strukt);
				writeArgsNoCtxWithBraces(ctx, e.args);
			},
			[&](const ConcreteExpr::ImplicitConvertToUnion e) {
				writeType(ctx.writer, type);
				ctx.write("(");
				writeConstantOrExprAsExpr(ctx, e.arg);
				ctx.write(")");
			},
			[&](const ConcreteExpr::Lambda e) {
				const KnownLambdaBody* klb = ce.knownLambdaBody().force();
				ctx.writeStr(klb->closureType().force().strukt->mangledName);
				writeArgsNoCtxWithBraces(ctx, e.closureInit);
			},
			[&](const ConcreteExpr::LambdaToDynamic e) {
				const Arr<const ConcreteField> fields = type.strukt->body().asFields().fields;
				assert(fields.size == 2);
				const ConcreteField funPtrField = at(fields, 0);
				const ConcreteField dataPtrField = at(fields, 1);

				writeValueType(ctx.writer, type.mustBeNonPointer());
				ctx.write("{");
				ctx.indent();
				ctx.write("reinterpret_cast<");
				writeType(ctx.writer, funPtrField.type);
				ctx.write(">(");
				ctx.writeStr(e.fun->mangledName());
				ctx.write("), reinterpret_cast<");
				writeType(ctx.writer, dataPtrField.type);
				ctx.write(">(");
				writeConstantOrExprAsExpr(ctx, e.closure);
				ctx.write(")}");
			},
			[&](const ConcreteExpr::Let) {
				iife();
			},
			[&](const ConcreteExpr::LocalRef e) {
				ctx.writeStr(e.local->mangledName);
			},
			[&](const ConcreteExpr::Match) {
				iife();
			},
			[&](const ConcreteExpr::MessageSend e) {
				writeConstantOrExprAsExpr(ctx, e.target);
				ctx.write(".");
				ctx.writeStr(e.message->mangledName);
				writeArgsWithCtx(ctx, e.args);
			},
			[&](const ConcreteExpr::NewIfaceImpl e) {
				writeNewIfaceImplAsExpr(ctx, e);
			},
			[&](const ConcreteExpr::ParamRef e) {
				ctx.writeStr(e.param->mangledName);
			},
			[&](const ConcreteExpr::Seq) {
				iife();
			},
			[&](const ConcreteExpr::SpecialBinary e) {
				ctx.write("(");
				writeConstantOrExprAsExpr(ctx, e.left);
				using Kind = ConcreteExpr::SpecialBinary::Kind;
				switch (e.kind) {
					case Kind::less:
						ctx.write(" < ");
						break;
					case Kind::_or:
						ctx.write(" || ");
						break;
					default:
						assert(0);
				}
				writeConstantOrExprAsExpr(ctx, e.right);
				ctx.write(")");
			},
			[&](const ConcreteExpr::StructFieldAccess e) {
				writeFieldAccess(ctx, e.targetIsPointer, e.target, e.field);
			},
			[&](const ConcreteExpr::StructFieldSet e) {
				unused(e);
				todo<void>("structfieldset as expr -- use IIFE");
			});
	}

	void writeConstantOrExpr(Writer& writer, const ConstantOrExpr ce) {
		WriteExprCtx ctx { writer };
		writeConstantOrExprAsStatement(ctx, ce, /*isReturn*/ True);
	}

	void writeSigParams(Writer& writer, const Bool needsCtx, const Opt<const ConcreteParam> closure, const Arr<const ConcreteParam> params) {
		writeChar(writer, '(');
		if (needsCtx)
			writeStatic(writer, "ctx* ctx");
		if (closure.has()) {
			if (needsCtx)
				writeStatic(writer, ", ");
			doWriteParam(writer, closure.force());
		}
		writeJustParams(writer, _or(needsCtx, closure.has()), params);
	}

	void writeFunReturnTypeNameAndParams(Writer& writer, const ConcreteFun* fun) {
		writeType(writer, fun->returnType());
		writeChar(writer, ' ');
		writeStr(writer, fun->mangledName());
		writeSigParams(writer, fun->needsCtx, fun->closureParam, fun->paramsExcludingCtxAndClosure());
	}

	void writeConcreteFunDeclaration(Writer& writer, const ConcreteFun* fun) {
		if (fun->body().isExtern())
			writeStatic(writer, "extern \"C\" ");
		writeFunReturnTypeNameAndParams(writer, fun);
		writeStatic(writer, ";\n");
	}

	template <typename CbWriteBody>
	void writeFunWithBodyWorker(Writer& writer, const ConcreteFun* fun, CbWriteBody cbWriteBody) {
		writeFunReturnTypeNameAndParams(writer, fun);
		writeStatic(writer, " {\n\t");
		cbWriteBody();
		writeStatic(writer, "\n}\n");
	}

	void writeFunWithBody(Writer& writer, const ConcreteFun* fun, const ConstantOrExpr body) {
		writeFunWithBodyWorker(writer, fun, [&]() {
			writeConstantOrExpr(writer, body);
		});
	}

	void writeConcreteFunDefinition(Writer& writer, const ConcreteFun* fun) {
		fun->body().match(
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
						if (info.kind != BuiltinFunKind::hardFail) {
							printf("Other special? %d\n", info.kind);
						}
						assert(info.kind == BuiltinFunKind::hardFail);
						writeFunWithBodyWorker(writer, fun, [&]() {
							writeStatic(writer, "assert(0);");
						});
						break;
					default:
						assert(0);
				}
			},
			// Already declared it, nothing more to do
			[&](const ConcreteFunBody::Extern) {},
			[&](const Constant* c) {
				writeFunWithBody(writer, fun, ConstantOrExpr{c});
			},
			[&](const ConcreteExpr* e) {
				writeFunWithBody(writer, fun, ConstantOrExpr{e});
			});
	}
}

const Str writeToC(Arena& arena, const ConcreteProgram program) {
	Writer writer { arena };

	writeStatic(writer, "#include <cassert>\n");
	writeStatic(writer, "#include <cstdint>\n");

	// Problem: pointer and function pointer types are declared using 'using' and can't be predeclared.
	// But those may refer to other types. So may not be able to write them yet.
	writeStructs(writer, program.allStructs);

	for (const Constant* c : program.allConstants)
		writeConstantDecl(writer, c);

	writeStatic(writer, "template <typename T>");
	writeStatic(writer, "\nT* _alloc(byte* out, T value) {");
	writeStatic(writer, "\n\tT* res = reinterpret_cast<T*>(out);");
	writeStatic(writer, "\n\t*res = value;");
	writeStatic(writer, "\n\treturn res;");
	writeStatic(writer, "\n}\n\n");

	for (const ConcreteExpr::NewIfaceImpl impl : program.allNewIfaceImpls)
		writeNewIfaceImpl(writer, impl);

	for (const ConcreteFun* fun : program.allFuns)
		writeConcreteFunDeclaration(writer, fun);

	for (const ConcreteFun* fun : program.allFuns)
		writeConcreteFunDefinition(writer, fun);

	writeStatic(writer, "\n\nint main() { return (int) main__int64(); }\n");
	return writer.finish();
}
