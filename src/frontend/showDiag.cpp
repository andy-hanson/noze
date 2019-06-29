#include "./showDiag.h"

#include "../util/arrUtil.h"

namespace {
	//TODO:MOVE
	void writePath(Writer& writer, const Path* p) {
		if (p->parent.has())
			writePath(writer, p->parent.force());
		writeChar(writer, '/');
		writeStr(writer, p->baseName);
	}

	//TODO:MOVE
	void writeLineAndColumn(Writer& writer, const LineAndColumn lc) {
		writeNat(writer, lc.line + 1);
		writeChar(writer, ':');
		writeNat(writer, lc.column + 1);
	}

	void showPos(Writer& writer, const LineAndColumnGetter lc, const Pos pos) {
		writeLineAndColumn(writer, lineAndColumnAtPos(lc, pos));
	}

	void showRange(Writer& writer, const LineAndColumnGetter lc, const SourceRange range) {
		showPos(writer, lc, range.start);
		writeChar(writer, '-');
		showPos(writer, lc, range.end);
	}

	void showChar(Writer& writer, char c) {
		switch (c) {
			case '\0':
				writeStatic(writer, "\\0");
				break;
			case '\n':
				writeStatic(writer, "\\n");
				break;
			case '\t':
				writeStatic(writer, "\\t");
				break;
			default:
				writeChar(writer, c);
				break;
		}
	}

	void writeParseDiag(Writer& writer, const ParseDiag d) {
		d.match(
			[&](const ParseDiag::ExpectedCharacter) {
				todo<void>("expectedcharacter");
			},
			[&](const ParseDiag::ExpectedIndent) {
				writeStatic(writer, "expected an indent");
			},
			[&](const ParseDiag::ExpectedPurityAfterSpace) {
				writeStatic(writer, "after trialing space, expected to parse 'mutable' or 'sendable'");
			},
			[&](const ParseDiag::LeadingSpace) {
				todo<void>("leadingspace");
			},
			[&](const ParseDiag::LetMustHaveThen) {
				writeStatic(writer, "after 'x = y', must have another line after it");
			},
			[&](const ParseDiag::MatchWhenNewMayNotAppearInsideArg) {
				todo<void>("mwn");
			},
			[&](const ParseDiag::MustEndInBlankLine) {
				writeStatic(writer, "file must end in a blank line");
			},
			[&](const ParseDiag::TrailingSpace) {
				todo<void>("trailingspace");
			},
			[&](const ParseDiag::TypeParamCantHaveTypeArgs) {
				writeStatic(writer, "a type parameter can't have type arguments");
			},
			[&](const ParseDiag::UnexpectedCharacter d) {
				writeStatic(writer, "unexpected character '");
				showChar(writer, d.ch);
				writeStatic(writer, "'");
			},
			[&](const ParseDiag::UnionCantBeEmpty) {
				writeStatic(writer, "union type can't be empty");
			},
			[&](const ParseDiag::WhenMustHaveElse) {
				writeStatic(writer, "'when' expression must end in 'else'");
			});
	}

	//TODO:MOVE
	void writePurity(Writer& writer, const Purity p) {
		switch (p) {
			case Purity::data:
				writeStatic(writer, "data");
				break;
			case Purity::sendable:
				writeStatic(writer, "sendable");
				break;
			case Purity::nonSendable:
				writeStatic(writer, "mut");
				break;
			default:
				assert(0);
		}
	}

	void writeDiag(Writer& writer, const Diag d) {
		d.match(
			[&](const Diag::CantCallNonNoCtx) {
				writeStatic(writer, "a 'noctx' fun can't call a non-'noctx' fun.");
			},
			[&](const Diag::CantCallSummon) {
				writeStatic(writer, "non-'summon' fun can't call 'summon' fun");
			},
			[&](const Diag::CantCallUnsafe) {
				writeStatic(writer, "non-'trusted' and non-'unsafe' function can't call 'unsafe' function");
			},
			[&](const Diag::CantCreateNonRecordStruct d) {
				writeStatic(writer, "non-record struct ");
				writeStr(writer, d.strukt->name);
				writeStatic(writer, " can't be constructed");
			},
			[&](const Diag::CantInferTypeArguments) {
				writeStatic(writer, "can't infer type arguments");
			},
			[&](const Diag::CircularImport) {
				todo<void>("print circular import diag");
			},
			[&](const Diag::CommonTypesMissing) {
				writeStatic(writer, "common types are missing from 'include.nz'");
			},
			[&](const Diag::DuplicateDeclaration) {
				todo<void>("print duplicate declaration diag");
			},
			[&](const Diag::ExpectedTypeIsNotALambda d) {
				if (d.expectedType.has()) {
					writeStatic(writer, "the expected type at the lambda is ");
					writeType(writer, d.expectedType.force());
					writeStatic(writer, ", which is not a lambda type");
				} else
					writeStatic(writer, "there is no expected type at this location; lambdas need an expected type");
			},
			[&](const Diag::FieldPurityWorseThanStructPurity d) {
				writeStatic(writer, "struct is ");
				writePurity(writer, d.structPurity);
				writeStatic(writer, ", but field is ");
				writePurity(writer, d.fieldPurity);
			},
			[&](const Diag::FileDoesNotExist) {
				// We handle this specially
				unreachable<void>();
			},
			[&](const Diag::MatchCaseStructNamesDoNotMatch d) {
				writeStatic(writer, "expected the case names to be: ");
				writeWithCommas(writer, d.unionMembers, [&](const StructInst* i) {
					writeStr(writer, i->decl->name);
				});
			},
			[&](const Diag::MatchOnNonUnion d) {
				writeType(writer, d.type);
				writeStatic(writer, " is not a union type");
			},
			[&](const Diag::MultipleFunctionCandidates d) {
				writeStatic(writer, "multiple fun candidates match: ");
				for (const size_t i : Range{d.candidates.size}) {
					if (i != 0)
						writeStatic(writer, ", ");
					writeStr(writer, at(d.candidates, i).name());
				}
			},
			[&](const Diag::NameNotFound d) {
				const CStr kind = [&]() {
					switch (d.kind) {
						case Diag::NameNotFound::Kind::strukt:
							return "struct";
						case Diag::NameNotFound::Kind::spec:
							return "spec";
						case Diag::NameNotFound::Kind::iface:
							return "interface";
						case Diag::NameNotFound::Kind::typeParam:
							return "type parameter";
						default:
							assert(0);
					}
				}();
				writeStatic(writer, kind);
				writeStatic(writer, " name not found: ");
				writeStr(writer, d.name);
			},
			[&](const Diag::NoSuchFunction d) {
				writeStatic(writer, "no such function '");
				writeStr(writer, d.name);
				writeStatic(writer, "' exists with those argument types and return type");
			},
			[&](const Diag::ParamShadowsPrevious) {
				todo<void>("print paramshadowsprevious");
			},
			[&](const ParseDiag pd) {
				writeParseDiag(writer, pd);
			},
			[&](const Diag::ShouldNotHaveTypeParamsInIface) {
				writeStatic(writer, "a member function of an interface should not have type parameters");
			},
			[&](const Diag::SpecImplHasSpecs d) {
				writeStatic(writer, "spec implementation ");
				writeStr(writer, d.funName);
				writeStatic(writer, " has specs; currently this is not allowed");
			},
			[&](const Diag::SpecImplNotFound d) {
				writeStatic(writer, "no implementation was found for spec signature ");
				writeStr(writer, d.sigName);
			},
			[&](const Diag::TypeConflict d) {
				writeStatic(writer, "the type of the expression conflicts with its expected type.\nexpected: ");
				writeType(writer, d.expected);
				writeStatic(writer, "\nactual: ");
				writeType(writer, d.actual);
			},
			[&](const Diag::TypeNotSendable) {
				writeStatic(writer, "this type is not sendable and should not appear in an interface");
			},
			[&](const Diag::WrongNumberNewStructArgs d) {
				writeStatic(writer, "struct initializer expected to get ");
				writeNat(writer, d.nExpectedArgs);
				writeStatic(writer, " args, but got ");
				writeNat(writer, d.nActualArgs);
			},
			[&](const Diag::WrongNumberTypeArgsForSpec d) {
				writeStr(writer, d.decl->name);
				writeStatic(writer, ": expected to get ");
				writeNat(writer, d.nExpectedTypeArgs);
				writeStatic(writer, " type args, but got ");
				writeNat(writer, d.nActualTypeArgs);
			},
			[&](const Diag::WrongNumberTypeArgsForStruct d) {
				writeStr(writer, d.decl.name());
				writeStatic(writer, ": expected to get ");
				writeNat(writer, d.nExpectedTypeArgs);
				writeStatic(writer, " type args, but got ");
				writeNat(writer, d.nActualTypeArgs);
			});
	}

	void showDiagnostic(Writer& writer, const LineAndColumnGetter lc, const Diagnostic d) {
		writePath(writer, d.where.path);
		writeChar(writer, ' ');
		showRange(writer, lc, d.range);
		writeChar(writer, ' ');
		writeDiag(writer, d.diag);
		writeChar(writer, '\n');
	}
}

void printDiagnostics(const Diagnostics diagnostics) {
	Arena tempArena {};
	Writer writer { tempArena};
	//TODO: sort diagnostics by file / range
	const Arr<const Arr<const Diagnostic>> groups = sortAndGroup(tempArena, diagnostics.diagnostics, [&](const Diagnostic a, const Diagnostic b) {
		return comparePathAndStorageKind(a.where, b.where);
	});

	for (const Arr<const Diagnostic> group : groups) {
		if (first(group).diag.isFileDoesNotExist()) {
			assert(group.size == 1);
			writePath(writer, only(group).where.path);
			writeStatic(writer, ": file does not exist\n");
		} else {
			for (const Diagnostic d : group) {
				const LineAndColumnGetter lc = diagnostics.lineAndColumnGetters.mustGet(d.where);
				showDiagnostic(writer, lc, d);
			}
		}
	}

	printf("%s\n", writer.finishCStr());
}
