#include "./writeToC.h"

#include "../util/arrUtil.h"
#include "../concretize/concretizeUtil.h" // writeConcreteTypeForMangle
#include "../concretize/writer.h"

namespace {
	void writeEscapedChar(Writer& writer, const char c) {
		switch (c) {
			case '\n':
				writer.writeStatic("\\n");
				break;
			case '\t':
				writer.writeStatic("\\t");
				break;
			case '\'':
				writer.writeStatic("\\\'");
				break;
			case '"':
				writer.writeStatic("\\\"");
				break;
			case '\\':
				writer.writeStatic("\\\\");
				break;
			case '\0':
				writer.writeStatic("\\0");
				break;
			default:
				writer.writeChar(c);
				break;
		}
	}

	void writeQuotedString(Writer& writer, const Arr<const Constant*> chars) {
		writer.writeChar('"');
		for (const Constant* c : chars)
			writeEscapedChar(writer, c->kind.asChar());
		writer.writeChar('"');
	}

	template <typename T, typename Cb>
	void writeWithCommas(Writer& w, const Arr<T> a, const Bool leadingComma, Cb cb) {
		for (const size_t i : Range{a.size}) {
			if (leadingComma || i != 0)
				w.writeStatic(", ");
			cb(a[i]);
		}
	}

	void writeValueType(Writer& writer, const ConcreteStruct* s) {
		writer.writeStr(s->mangledName);
	}

	void writeType(Writer& writer, const ConcreteType t) {
		writeValueType(writer, t.strukt);
		if (t.isPointer)
			writer.writeChar('*');
	}

	void doWriteParam(Writer& writer, const ConcreteParam p) {
		writeType(writer, p.type);
		writer.writeChar(' ');
		writer.writeStr(p.mangledName);
	}

	void writeJustParams(Writer& writer, const Bool wroteFirst, const Arr<const ConcreteParam> params) {
		Cell<const Bool> didWriteFirst { wroteFirst };
		for (const ConcreteParam p : params) {
			if (didWriteFirst.get())
				writer.writeStatic(", ");
			else
				didWriteFirst.set(True);
			doWriteParam(writer, p);
		}
		writer.writeChar(')');
	}

	void writeJustParamsAlwaysComma(Writer& writer, const Arr<const ConcreteParam> params) {
		for (const ConcreteParam p : params) {
			writer.writeStatic(", ");
			doWriteParam(writer, p);
		}
	}

	void writeBuiltinAliasForStruct(Writer& writer, const Str name, const BuiltinStructKind kind, const Arr<const ConcreteType> typeArgs) {
		// "bool" and "char" share the same name as in C, so no need for an alias.
		if (kind != BuiltinStructKind::_bool && kind != BuiltinStructKind::_char) {
			writer.writeStatic("using ");
			writer.writeStr(name);
			writer.writeStatic(" = ");
			switch (kind) {
				case BuiltinStructKind::byte:
					writer.writeStatic("uint8_t");
					break;
				case BuiltinStructKind::float64:
					writer.writeStatic("double");
					break;
				case BuiltinStructKind::int64:
					writer.writeStatic("int64_t");
					break;
				case BuiltinStructKind::funPtrN:
					writeType(writer, typeArgs[0]);
					writer.writeStatic(" (*)(");
					for (const size_t i : Range{1, typeArgs.size}) {
						if (i != 1)
							writer.writeStatic(", ");
						writeType(writer, typeArgs[i]);
					}
					writer.writeChar(')');
					break;
				case BuiltinStructKind::nat64:
					writer.writeStatic("uint64_t");
					break;
				case BuiltinStructKind::ptr:
					writeType(writer, typeArgs[0]);
					writer.writeChar('*');
					break;
				case BuiltinStructKind::_void:
					writer.writeStatic("uint8_t");
					break;
				case BuiltinStructKind::_bool:
				case BuiltinStructKind::_char:
					unreachable<void>();
				default:
					assert(0);
			}

			writer.writeStatic(";\n");
		}
	}

	void writeStructHead(Writer& writer, const Str mangledName) {
		writer.writeStatic("struct ");
		writer.writeStr(mangledName);
		writer.writeStatic(" {");
	}

	void writeStructEnd(Writer& writer) {
		writer.writeStatic("\n};\n");
	}

	void writeFieldsStruct(Writer& writer, const Str mangledName, const Arr<const ConcreteField> fields) {
		writeStructHead(writer, mangledName);
		if (isEmpty(fields))
			writer.writeStatic("};\n");
		else {
			for (const ConcreteField field : fields) {
				writer.writeStatic("\n\t");
				writeType(writer, field.type);
				writer.writeStatic(" ");
				writer.writeStr(field.mangledName);
				writer.writeStatic(";");
			}
			writeStructEnd(writer);
		}
	}

	void writeUnionStruct(Writer& writer, const Str mangledName, const Arr<const ConcreteType> members) {
		writeStructHead(writer, mangledName);

		auto writeMemberName = [&](const ConcreteType member) -> void {
			writer.writeStr(member.strukt->mangledName);
		};

		writer.writeStatic("\n\tenum class Kind {");
		for (const ConcreteType member : members) {
			writer.writeStatic("\n\t\t");
			writeMemberName(member);
			writer.writeChar(',');
		}
		writer.writeStatic("\n\t};");
		writer.writeStatic("\n\tKind kind;");
		writer.writeStatic("\n\tunion {");
		for (const ConcreteType member : members) {
			writer.writeStatic("\n\t\t");
			writeType(writer, member);
			writer.writeStatic(" as_");
			writeMemberName(member);
			writer.writeChar(';');
		}
		writer.writeStatic("\n\t};");

		for (const ConcreteType member : members) {
			writer.writeStatic("\n\t");
			writer.writeStr(mangledName);
			writer.writeChar('(');
			writeType(writer, member);
			writer.writeStatic(" value) : kind{Kind::");
			writeMemberName(member);
			writer.writeStatic("}, as_");
			writeMemberName(member);
			writer.writeStatic("{value} {}");
		}

		writeStructEnd(writer);
	}

	void writeIfaceStruct(Writer& writer, const Str mangledName, const Arr<const ConcreteSig> messages) {
		writeStructHead(writer, mangledName);

		// Create a vtable struct containing its fields.
		// And add funs to call those.
		writer.writeStatic("\n\tstruct Funs {");
		for (const ConcreteSig msg : messages) {
			writer.writeStatic("\n\t\t");
			writeType(writer, msg.returnType);
			writer.writeStatic(" (*)(ctx* ctx, void* closure");
			writeJustParamsAlwaysComma(writer, msg.params);
			writer.writeChar(' ');
			writer.writeStr(msg.mangledName);
			writer.writeChar(';');
		}
		todo<void>("do without mixins");

		// For convenience, write a method for calling each of the funs.
		// E.g.:
		// fut<_void>* doSomething(ctx* ctx, bool param0) {
		//	return vtable.funs.push(ctx, data, param0);
		// }

		for (const ConcreteSig msg : messages) {
			writer.writeStatic("\n\t");
			writeType(writer, msg.returnType);
			writer.writeChar(' ');
			writer.writeStr(msg.mangledName);
			writer.writeStatic("ctx* ctx");
			writeJustParamsAlwaysComma(writer, msg.params);
			writer.writeStatic(" {\n\t\treturn vtable->funs.");
			writer.writeStr(msg.mangledName);
			writer.writeStatic("(ctx, data");
			for (const ConcreteParam p : msg.params) {
				writer.writeStatic(", ");
				writer.writeStr(p.mangledName);
			}
			writer.writeStatic(");\n\t}");
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
		writer.writeStatic("struct ");
		writer.writeStr(strukt->mangledName);
		writer.writeStatic(";\n");
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

		writer.writeStatic("\n");
		for (const ConcreteStruct* strukt : allStructs) {
			writer.writeStatic("static_assert(sizeof(");
			writer.writeStr(strukt->mangledName);
			writer.writeStatic(") == ");
			writer.writeUint(strukt->sizeBytes());
			writer.writeStatic(", \"\");\n");
		}
		writer.writeStatic("\n");
	}
}

namespace {
	void writeConstantRecordName(Writer& writer, const Constant* c, const ConcreteType t) {
		writer.writeStatic("_constant__");
		writeConcreteTypeForMangle(writer, t);
		writer.writeStatic("__");
		writer.writeUint(c->id);
	}

	void writeConstantUnionName(Writer& writer, const Constant* c, const ConcreteStruct* s) {
		writer.writeStatic("_constant__");
		writer.writeStr(s->mangledName);
		writer.writeStatic("__");
		writer.writeUint(c->id);
	}

	void writeConstantReference(Writer& writer, const Constant* c) {
		auto writeRef = [&](const char* s) {
			writer.writeStatic("_constant");
			writer.writeStatic(s);
			writer.writeUint(c->id);
		};

		c->kind.match(
			[&](const ConstantKind::Array) {
				writeRef("Arr");
			},
			[&](const Bool b) {
				writer.writeStatic(b ? "true" : "false");
			},
			[&](const char c) {
				writer.writeChar('\'');
				writeEscapedChar(writer, c);
				writer.writeChar('\'');
			},
			[&](const ConstantKind::FunPtr f) {
				writer.writeStatic("(&");
				writer.writeStr(f.fun->mangledName());
				writer.writeChar(')');
			},
			[&](const Int64 i) {
				writer.writeInt(i);
			},
			[&](const ConstantKind::Lambda l) {
				unused(l);
				// Don't need a decl for this (other than the concretefun), just create it here
				todo<void>("const lambda");
			},
			[&](const Nat64 n) {
				writer.writeUint(n);
			},
			[&](const ConstantKind::Null) {
				writer.writeStatic("nullptr");
			},
			[&](const ConstantKind::Ptr p) {
				writer.writeChar('&');
				writeConstantReference(writer, p.array);
				writer.writeStatic(".data[");
				writer.writeUint(p.index);
				writer.writeChar(']');
			},
			[&](const ConstantKind::Record r) {
				if (r.type.isPointer)
					writer.writeChar('&');
				writeConstantRecordName(writer, c, r.type);
			},
			[&](const ConstantKind::Union u) {
				writeConstantUnionName(writer, c, u.unionType);
			},
			[&](const ConstantKind::Void) {
				// Value should be ignored, so any will do
				writer.writeChar('0');
			});
	}

	void writeConstantReferencesWithCommas(Writer& writer, const Arr<const Constant*> constants) {
		writeWithCommas(writer, constants, /*leadingComma*/ False, [&](const Constant* c) {
			writeConstantReference(writer, c);
		});
	}

	void writeConstantDecl(Writer& writer, const Constant* c) {
		c->kind.match(
			[&](const ConstantKind::Array a) {
				// static nat64 _constantArrBacking123[3] = {1, 2, 3};
				// static arr_nat64 _constantArr123 = arr_nat64{3, _constantArrBacking123};

				// Strings are special:
				// static arr_char _constantArr123 = arr_char{5, const_cast<char*>("hello")};

				const size_t size = a.size();
				const Bool isStr = a.elements()[0]->kind.isChar();
				const size_t id = c->id;
				if (size != 0 && !isStr) {
					writer.writeStatic("static ");
					writeType(writer, a.elementType());
					writer.writeStatic(" _constantArrBacking");
					writer.writeUint(id);
					writer.writeChar('[');
					writer.writeUint(size);
					writer.writeStatic("] = {");
					writeConstantReferencesWithCommas(writer, a.elements());
					writer.writeStatic("};\n");
				}

				writer.writeStatic("static ");
				writeValueType(writer, a.arrayType);
				writer.writeStatic(" _constantArr");
				writer.writeUint(id);
				writer.writeStatic(" = ");
				writeValueType(writer, a.arrayType);
				writer.writeChar('{');
				writer.writeUint(size);
				writer.writeStatic(", ");
				if (isStr) {
					writer.writeStatic("const_cast<char*>(");
					writeQuotedString(writer, a.elements());
					writer.writeStatic(")");
				} else {
					writer.writeStatic("_constantArrBacking");
					writer.writeUint(id);
				}
				writer.writeStatic("};\n");
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
				writer.writeStatic("static ");
				writeType(writer, r.type);
				writer.writeChar(' ');
				writeConstantRecordName(writer, c, r.type);
				writer.writeStatic(" = ");
				writeType(writer, r.type);
				writer.writeChar('(');
				writeConstantReferencesWithCommas(writer, r.args);
				writer.writeStatic(");\n");
			},
			[&](const ConstantKind::Union u) {
				writer.writeStatic("static ");
				writeValueType(writer, u.unionType);
				writer.writeChar(' ');
				writeConstantUnionName(writer, c, u.unionType);
				writer.writeStatic(" = ");
				writeValueType(writer, u.unionType);
				writer.writeChar('(');
				writeConstantReference(writer, u.member);
				writer.writeStatic(");\n");
			},
			[](const ConstantKind::Void) {});
	}

	void writeNewIfaceImpl(Writer& writer, const ConcreteExpr::NewIfaceImpl impl) {
		unused(writer, impl);
		todo<void>("!!!");
	}

	struct WriteExprCtx {
		Writer& writer;
		uint _indent = 1;

		void newline() {
			writer.writeChar('\n');
			for (__attribute__((unused)) const size_t _ : Range{_indent})
				writer.writeChar('\t');
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

		void write(const char* text) {
			writer.writeStatic(text);
		}

		void writeStr(const Str s) {
			writer.writeStr(s);
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
		writer.writeStatic(" ");
		writer.writeStr(local->mangledName);
		writer.writeStatic(" = ");
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
			ctx.write(":");
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
		});
		ctx.write("default: assert(0);");
		ctx.dedent();
		ctx.write("}");
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
			});
	}

	void writeCallOperator(WriteExprCtx& ctx, const BuiltinFunInfo bf, const Arr<const ConcreteType> typeArgs, const ConcreteExpr::CallConcreteFun e) {
		auto writeArg = [&](const size_t index) -> void {
			writeConstantOrExprAsExpr(ctx, e.args[index]);
		};

		auto binaryOperatorWorker = [&](const Str _operator) -> void {
			assert(e.args.size == 2);
			writeArg(0);
			ctx.write(" ");
			ctx.writeStr(_operator);
			ctx.write(" ");
			writeArg(1);
		};

		auto binaryOperator = [&](const char* s) -> void {
			binaryOperatorWorker(strLiteral(s));
		};

		auto unaryOperatorWorker = [&](const Str _operator) -> void {
			assert(e.args.size == 1);
			ctx.writeStr(_operator);
			ctx.write("(");
			writeArg(0);
			ctx.write(")");
		};

		auto unaryOperator = [&](const char* s) -> void {
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

			case BuiltinFunKind::deref:
				unaryOperator("*");
				break;

			case BuiltinFunKind::ptrCast:
				ctx.write("static_cast<");
				writeType(ctx.writer, typeArgs[0]);
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

		ce.match(
			[](const ConcreteExpr::Bogus) {
				unreachable<void>();
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
				if (e.alloc.has()) {
					const ConcreteFun* alloc = e.alloc.force();
					assert(alloc->needsCtx); // The allocator function uses the ctx to allocate
					ctx.write("_alloc<");
					writeValueType(ctx.writer, e.type.strukt);
					ctx.write(">(");
					ctx.writeStr(alloc->mangledName());
					ctx.write("(ctx, ");
					ctx.writer.writeUint(e.type.strukt->sizeBytes());
					ctx.write("), ");
				}
				writeValueType(ctx.writer, e.type.strukt);
				ctx.write("{");
				writeArgsNoCtxNoParens(ctx, e.args);
				ctx.write("}");
				if (e.type.isPointer)
					ctx.write(")");
			},
			[&](const ConcreteExpr::ImplicitConvertToUnion e) {
				writeType(ctx.writer, e.unionType);
				ctx.write("(");
				writeConstantOrExprAsExpr(ctx, e.arg);
				ctx.write(")");
			},
			[&](const ConcreteExpr::Lambda e) {
				const KnownLambdaBody* klb = ce.knownLambdaBody().force();
				assert(klb->hasClosure()); // Otherwise this would be a constant
				ctx.writeStr(klb->closureStructMangledName());
				writeArgsNoCtx(ctx, e.closureInit);
			},
			[&](const ConcreteExpr::LambdaToDynamic) {
				todo<void>("write lambdatodynamic");
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
				writeConstantOrExprAsExpr(ctx, e.target);
				ctx.write(".");
				ctx.writeStr(e.field->mangledName);
			});
	}

	void writeConstantOrExpr(Writer& writer, const ConstantOrExpr ce) {
		WriteExprCtx ctx { writer };
		writeConstantOrExprAsStatement(ctx, ce, /*isReturn*/ True);
	}

	void writeSigParams(Writer& writer, const Bool needsCtx, const Opt<const ConcreteType> closure, const Arr<const ConcreteParam> params) {
		writer.writeChar('(');
		if (needsCtx)
			writer.writeStatic("ctx* ctx");
		if (closure.has()) {
			if (needsCtx)
				writer.writeStatic(", ");
			writeType(writer, closure.force());
			writer.writeStatic(" _closure");
		}
		writeJustParams(writer, _or(needsCtx, closure.has()), params);
	}

	void writeFunReturnTypeNameAndParams(Writer& writer, const ConcreteFun* fun) {
		writeType(writer, fun->returnType());
		writer.writeChar(' ');
		writer.writeStr(fun->mangledName());
		writeSigParams(writer, fun->needsCtx, fun->closureType, fun->paramsExcludingClosure());
	}

	void writeConcreteFunDeclaration(Writer& writer, const ConcreteFun* fun) {
		if (fun->body().isExtern())
			writer.writeStatic("extern \"C\" ");
		writeFunReturnTypeNameAndParams(writer, fun);
		writer.writeStatic(";\n");
	}

	template <typename CbWriteBody>
	void writeFunWithBodyWorker(Writer& writer, const ConcreteFun* fun, CbWriteBody cbWriteBody) {
		writeFunReturnTypeNameAndParams(writer, fun);
		writer.writeStatic(" {\n\t");
		cbWriteBody();
		writer.writeStatic("\n}\n");
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
						assert(info.kind == BuiltinFunKind::hardFail);
						writeFunWithBodyWorker(writer, fun, [&]() {
							writer.writeStatic("assert(0);");
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

	writer.writeStatic("#include <cassert>\n");
	writer.writeStatic("#include <cstdint>\n");

	// Problem: pointer and function pointer types are declared using 'using' and can't be predeclared.
	// But those may refer to other types. So may not be able to write them yet.
	writeStructs(writer, program.allStructs);

	for (const Constant* c : program.allConstants)
		writeConstantDecl(writer, c);

	writer.writeStatic("template <typename T>");
	writer.writeStatic("\nT* _alloc(byte* out, T value) {");
	writer.writeStatic("\n\tT* res = static_cast<T*>(out);");
	writer.writeStatic("\n\t*res = value;");
	writer.writeStatic("\n}\n\n");

	for (const ConcreteExpr::NewIfaceImpl impl : program.allNewIfaceImpls)
		writeNewIfaceImpl(writer, impl);

	for (const ConcreteFun* fun : program.allFuns)
		writeConcreteFunDeclaration(writer, fun);

	for (const ConcreteFun* fun : program.allFuns)
		writeConcreteFunDefinition(writer, fun);

	writer.writeStatic("\n\nint main() { return (int) main__int64(); }\n");
	return writer.finish();
}
