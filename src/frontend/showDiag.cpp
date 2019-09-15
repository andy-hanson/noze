#include "./showDiag.h"

#include "../util/arrUtil.h"

namespace {
	//TODO:MOVE
	void writePath(Writer* writer, const Path* p) {
		if (has(p->parent)) {
			writePath(writer, force(p->parent));
			writeChar(writer, '/');
		}
		writeStr(writer, p->baseName);
	}

	void writeRelPath(Writer* writer, const RelPath p) {
		repeat(p.nParents, [&]() {
			writeStatic(writer, "../");
		});
		writePath(writer, p.path);
	}

	void writePathAndStorageKind(Writer* writer, const PathAndStorageKind p) {
		writePath(writer, p.path);
	}

	//TODO:MOVE
	void writeLineAndColumn(Writer* writer, const LineAndColumn lc) {
		writeNat(writer, lc.line + 1);
		writeChar(writer, ':');
		writeNat(writer, lc.column + 1);
	}

	void writePos(Writer* writer, const LineAndColumnGetter lc, const Pos pos) {
		writeLineAndColumn(writer, lineAndColumnAtPos(lc, pos));
	}

	void writeRange(Writer* writer, const LineAndColumnGetter lc, const SourceRange range) {
		writePos(writer, lc, range.start);
		writeChar(writer, '-');
		writePos(writer, lc, range.end);
	}

	void writeWhere(Writer* writer, const FilesInfo fi, const PathAndStorageKindAndRange where) {
		writeBold(writer);
		Arena temp {};
		writeHyperlink(
			writer,
			pathToStr(&temp, fi.absolutePathsGetter.getAbsolutePath(&temp, where.pathAndStorageKind)),
			pathToStr(&temp, where.pathAndStorageKind.path));
		writeChar(writer, ' ');
		writeRed(writer);
		const LineAndColumnGetter lcg = mustGetAt<
			const PathAndStorageKind,
			const LineAndColumnGetter,
			comparePathAndStorageKind
		>(fi.lineAndColumnGetters, where.pathAndStorageKind);
		writeRange(writer, lcg, where.range);
		writeReset(writer);
	}

	void writeLineNumber(Writer* writer, const FilesInfo fi, const Module* module, const SourceRange range) {
		// TODO
		const PathAndStorageKind where = module->pathAndStorageKind;
		writeBold(writer);
		writePathAndStorageKind(writer, where);
		writeReset(writer);
		writeStatic(writer, " line ");
		const LineAndColumnGetter lcg = mustGetAt<
			const PathAndStorageKind,
			const LineAndColumnGetter,
			comparePathAndStorageKind
		>(fi.lineAndColumnGetters, where);
		const size_t line = lineAndColumnAtPos(lcg, range.start).line;
		writeNat(writer, line + 1);
	}

	void showChar(Writer* writer, char c) {
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

	void writeName(Writer* writer, const Sym name) {
		writeChar(writer, '\'');
		writeSym(writer, name);
		writeChar(writer, '\'');
	}

	void writeParseDiag(Writer* writer, const ParseDiag d) {
		d.match(
			[&](const ParseDiag::ExpectedCharacter) {
				todo<void>("expectedcharacter");
			},
			[&](const ParseDiag::ExpectedDedent) {
				writeStatic(writer, "expected a dedent");
			},
			[&](const ParseDiag::ExpectedIndent) {
				writeStatic(writer, "expected an indent");
			},
			[&](const ParseDiag::ExpectedPurityAfterSpace) {
				writeStatic(writer, "after trialing space, expected to parse 'mutable' or 'sendable'");
			},
			[&](const ParseDiag::IndentNotDivisible d) {
				writeStatic(writer, "expected indentation by ");
				writeNat(writer, d.nSpacesPerIndent);
				writeStatic(writer, " spaces, but got ");
				writeNat(writer, d.nSpaces);
				writeStatic(writer, " which is not divisible");
			},
			[&](const ParseDiag::IndentWrongCharacter d) {
				writeStatic(writer, "expected indentation by ");
				writeStatic(writer, d.expectedTabs ? "tabs" : "spaces");
				writeStatic(writer, " (based on first indented line), but here there is a ");
				writeStatic(writer, d.expectedTabs ? "space" : "tab");
			},
			[&](const ParseDiag::LetMustHaveThen) {
				writeStatic(writer,
					"the final line of a block can not be 'x = y'\n(hint: remove 'x =', or add another line)");
			},
			[&](const ParseDiag::MatchWhenNewMayNotAppearInsideArg) {
				todo<void>("mwn");
			},
			[&](const ParseDiag::MustEndInBlankLine) {
				writeStatic(writer, "file must end in a blank line");
			},
			[&](const ParseDiag::ReservedName d) {
				writeName(writer, d.name);
				writeStatic(writer, " is a reserved word and can't be used as a name");
			},
			[&](const ParseDiag::TypeParamCantHaveTypeArgs) {
				writeStatic(writer, "a type parameter can't have type arguments");
			},
			[&](const ParseDiag::UnexpectedCharacter d) {
				writeStatic(writer, "unexpected character '");
				showChar(writer, d.ch);
				writeStatic(writer, "'");
			},
			[&](const ParseDiag::UnexpectedDedent) {
				writeStatic(writer, "unexpected dedent");
			},
			[&](const ParseDiag::UnexpectedIndent) {
				writeStatic(writer, "unexpected indent");
			},
			[&](const ParseDiag::UnionCantBeEmpty) {
				writeStatic(writer, "union type can't be empty");
			},
			[&](const ParseDiag::WhenMustHaveElse) {
				writeStatic(writer, "'when' expression must end in 'else'");
			});
	}

	//TODO:MOVE
	void writePurity(Writer* writer, const Purity p) {
		writeChar(writer, '\'');
		switch (p) {
			case Purity::data:
				writeStatic(writer, "data");
				break;
			case Purity::sendable:
				writeStatic(writer, "sendable");
				break;
			case Purity::mut:
				writeStatic(writer, "mut");
				break;
			default:
				assert(0);
		}
		writeChar(writer, '\'');
	}

	void writeSigJustTypes(Writer* writer, const Sig s) {
		writeType(writer, s.returnType);
		writeChar(writer, '(');
		writeWithCommas(writer, s.params, [&](const Param p) {
			writeType(writer, p.type);
		});
		writeChar(writer, ')');
	}

	void writeCalledDecl(Writer* writer, const FilesInfo fi, const CalledDecl c) {
		writeSigJustTypes(writer, c.sig());
		return c.match(
			[&](const FunDecl* funDecl) {
				// TODO: write the module it's from
				writeStatic(writer, " (from ");
				writeLineNumber(writer, fi, funDecl->containingModule(), funDecl->range());
				writeChar(writer, ')');
			},
			[&](const SpecSig specSig) {
				writeStatic(writer, " (from spec ");
				specSig.specInst->name();
				writeChar(writer, ')');
			});
	}

	template <typename Filter>
	void writeCalledDecls(Writer* writer, const FilesInfo fi, const Arr<const CalledDecl> cs, Filter flt) {
		for (const CalledDecl c : cs)
			if (flt(c)) {
				writeChar(writer, '\n');
				writeCalledDecl(writer, fi, c);
			}
	}

	void writeCalledDecls(Writer* writer, const FilesInfo fi, const Arr<const CalledDecl> cs) {
		writeCalledDecls(writer, fi, cs, [](const CalledDecl) {
			return True;
		});
	}

	void writeCallNoMatch(Writer* writer, const FilesInfo fi, const Diag::CallNoMatch d) {
		const Bool someCandidateHasCorrectArity = exists(d.allCandidates, [&](const CalledDecl c) {
			return eq(arity(c), d.actualArity);
		});

		if (isEmpty(d.allCandidates)) {
			writeStatic(writer, "there is no function ");
			if (d.actualArity == 0)
				// If there is no local variable by that name we try a call,
				// but message should reflect that the user might not have wanted a call.
				writeStatic(writer, "or variable ");
			else if (d.actualArity == 1)
				writeStatic(writer, "or field ");
			writeStatic(writer, "named ");
			writeName(writer, d.funName);

			if (size(d.actualArgTypes) == 1) {
				writeStatic(writer, "\nargument type: ");
				writeType(writer, only(d.actualArgTypes));
			}
		} else if (!someCandidateHasCorrectArity) {
			writeStatic(writer, "there are functions named ");
			writeName(writer, d.funName);
			writeStatic(writer, ", but none takes ");
			writeNat(writer, d.actualArity);
			writeStatic(writer, " arguments. candidates:");
			writeCalledDecls(writer, fi, d.allCandidates);
		} else {
			writeStatic(writer, "there are functions named ");
			writeName(writer, d.funName);
			writeStatic(writer, ", but they do not match the ");
			const Bool hasRet = has(d.expectedReturnType);
			const Bool hasArgs = _not(isEmpty(d.actualArgTypes));
			const char* descr = hasRet
				? hasArgs ? "expected return type and actual argument types" : "expected return type"
				: "actual argument types";
			writeStatic(writer, descr);
			writeStatic(writer, ".");
			if (hasRet) {
				writeStatic(writer, "\nexpected return type: ");
				writeType(writer, force(d.expectedReturnType));
			}
			if (hasArgs) {
				writeStatic(writer, "\nactual argument types: ");
				writeWithCommas(writer, d.actualArgTypes, [&](const Type t) {
					writeType(writer, t);
				});
				if (size(d.actualArgTypes) < d.actualArity)
					writeStatic(writer, " (other arguments not checked, gave up early)");
			}
			writeStatic(writer, "\ncandidates (with ");
			writeNat(writer, d.actualArity);
			writeStatic(writer, " arguments):");
			writeCalledDecls(writer, fi, d.allCandidates, [&](const CalledDecl c) {
				return arity(c) == d.actualArity;
			});
		}
	}

	void writeDiag(Writer* writer, const FilesInfo fi, const Diag d) {
		d.match(
			[&](const Diag::CallMultipleMatches d) {
				writeStatic(writer, "cannot choose an overload of ");
				writeName(writer, d.funName);
				writeStatic(writer, ". multiple functions match:");
				writeCalledDecls(writer, fi, d.matches);
			},
			[&](const Diag::CallNoMatch d) {
				writeCallNoMatch(writer, fi, d);
			},
			[&](const Diag::CantCall c) {
				const char* descr = [&]() {
					switch (c.reason) {
						case Diag::CantCall::Reason::nonNoCtx:
							return "a 'noctx' fun can't call a non-'noctx' fun";
						case Diag::CantCall::Reason::summon:
							return "a non-'summon' fun can't call a 'summon' fun";
						case Diag::CantCall::Reason::unsafe:
							return "a non-'trusted' and non-'unsafe' fun can't call an 'unsafe' fun";
						default:
							assert(0);
					}
				}();
				writeStatic(writer, descr);
				writeStatic(writer, "\ncalling: ");
				writeName(writer, c.callee->name());
				writeStatic(writer, "\ncaller: ");
				writeName(writer, c.caller->name());
			},
			[&](const Diag::CantCreateNonRecordType d) {
				writeStatic(writer, "non-record type ");
				writeType(writer, d.type);
				writeStatic(writer, " can't be constructed");
			},
			[&](const Diag::CantCreateRecordWithoutExpectedType) {
				writeStatic(writer, "don't know what to 'new' (maybe provide a type argument)");
			},
			[&](const Diag::CantInferTypeArguments) {
				writeStatic(writer, "can't infer type arguments");
			},
			[&](const Diag::CircularImport d) {
				writeStatic(writer, "circular import from ");
				writePathAndStorageKind(writer, d.from);
				writeStatic(writer, " to ");
				writePathAndStorageKind(writer, d.to);
			},
			[&](const Diag::CommonTypesMissing) {
				writeStatic(writer, "common types are missing from 'include.nz'");
			},
			[&](const Diag::CreateRecordByRefNoCtx d) {
				writeStatic(writer, "the current function is 'noctx' and record ");
				writeName(writer, d.strukt->name);
				writeStatic(writer, " is not marked 'by-val'; can't allocate");
			},
			[&](const Diag::DuplicateDeclaration d) {
				writeStatic(writer, "duplicate ");
				switch (d.kind) {
					case Diag::DuplicateDeclaration::Kind::structOrAlias:
						writeStatic(writer, "struct");
						break;
					case Diag::DuplicateDeclaration::Kind::spec:
						writeStatic(writer, "spec");
						break;
					case Diag::DuplicateDeclaration::Kind::field:
						writeStatic(writer, "field");
						break;
					case Diag::DuplicateDeclaration::Kind::unionMember:
						writeStatic(writer, "member");
						break;
					default:
						assert(0);
				}
				writeChar(writer, ' ');
				writeName(writer, d.name);
			},
			[&](const Diag::DuplicateImports d) {
				writeStatic(writer, "the symbol ");
				writeName(writer, d.name);
				writeStatic(writer, " appears in multiple modules");
			},
			[&](const Diag::ExpectedTypeIsNotALambda d) {
				if (has(d.expectedType)) {
					writeStatic(writer, "the expected type at the lambda is ");
					writeType(writer, force(d.expectedType));
					writeStatic(writer, ", which is not a lambda type");
				} else
					writeStatic(writer, "there is no expected type at this location; lambdas need an expected type");
			},
			[&](const Diag::FileDoesNotExist d) {
				switch (d.kind) {
					case Diag::FileDoesNotExist::Kind::root:
						writeStatic(writer, "root path ");
						writePathAndStorageKind(writer, d.path);
						writeStatic(writer, " does not exist");
						break;
					case Diag::FileDoesNotExist::Kind::import:
						writeStatic(writer, "import resolves to ");
						writePathAndStorageKind(writer, d.path);
						writeStatic(writer, ", but file does not exist\n");
						break;
					default:
						assert(0);
				}
			},
			[&](const Diag::FunAsLambdaCantOverload) {
				writeStatic(writer, "fun has multiple overloads, can't convert to lambda");
			},
			[&](const Diag::FunAsLambdaWrongNumberTypeArgs d) {
				writeName(writer, d.fun->name());
				writeStatic(writer, " takes ");
				writeNat(writer, typeArity(d.fun));
				writeStatic(writer, " type arguments, but ");
				writeNat(writer, d.nProvidedTypeArgs);
				writeStatic(writer, " were provided");
			},
			[&](const Diag::FunAsLambdaWrongReturnType d) {
				writeStatic(writer, "fun as lambda has wrong return type\nactual: ");
				writeType(writer, d.actual);
				writeStatic(writer, "\nexpected: ");
				writeType(writer, d.expected);
			},
			[&](const Diag::LambdaCantInferParamTypes) {
				writeStatic(writer, "can't infer parameter types for lambda.\nconsider prefixing with 'as<...>:'");
			},
			[&](const Diag::LambdaClosesOverMut d) {
				writeStatic(writer, "lambda is a plain 'fun' but closes over ");
				writeName(writer, d.field->name);
				writeStatic(writer, " of of 'mut' type ");
				writeType(writer, d.field->type);
				writeStatic(writer, " (should it be a 'fun-mut'?)");
			},
			[&](const Diag::LambdaForFunPtrHasClosure d) {
				writeStatic(writer, "lambda closes over ");
				writeName(writer, d.field->name);
				writeStatic(writer, "; a lambda for a 'fun-ptr' is not allowed to close over anything");
			},
			[&](const Diag::LocalShadowsPrevious d) {
				writeName(writer, d.name);
				writeStatic(writer, " is already in scope");
			},
			[&](const Diag::MatchCaseStructNamesDoNotMatch d) {
				writeStatic(writer, "expected the case names to be: ");
				writeWithCommas(writer, d.unionMembers, [&](const StructInst* i) {
					writeName(writer, i->decl->name);
				});
			},
			[&](const Diag::MatchOnNonUnion d) {
				writeType(writer, d.type);
				writeStatic(writer, " is not a union type");
			},
			[&](const Diag::MutFieldNotAllowed d) {
				const char* message = [&]() {
					switch (d.reason) {
						case Diag::MutFieldNotAllowed::Reason::recordIsNotMut:
							return "field is mut, but containing record was not marked mut";
						case Diag::MutFieldNotAllowed::Reason::recordIsForcedByVal:
							return "field is mut, but containing record was forced by-val";
						default:
							assert(0);
					}
				}();
				writeStatic(writer, message);
			},
			[&](const Diag::NameNotFound d) {
				const CStr kind = [&]() {
					switch (d.kind) {
						case Diag::NameNotFound::Kind::strukt:
							return "struct";
						case Diag::NameNotFound::Kind::spec:
							return "spec";
						case Diag::NameNotFound::Kind::typeParam:
							return "type parameter";
						default:
							assert(0);
					}
				}();
				writeStatic(writer, kind);
				writeStatic(writer, " name not found: ");
				writeName(writer, d.name);
			},
			[&](const Diag::ParamShadowsPrevious) {
				todo<void>("print paramshadowsprevious");
			},
			[&](const ParseDiag pd) {
				writeParseDiag(writer, pd);
			},
			[&](const Diag::PurityOfFieldWorseThanRecord d) {
				writeStatic(writer, "struct ");
				writeName(writer, d.strukt->name);
				writeStatic(writer, " has purity ");
				writePurity(writer, d.strukt->purity);
				writeStatic(writer, ", but field type ");
				writeType(writer, d.fieldType);
				writeStatic(writer, " has purity ");
				writePurity(writer, d.fieldType.bestCasePurity());
			},
			[&](const Diag::PurityOfMemberWorseThanUnion d) {
				writeStatic(writer, "union ");
				writeName(writer, d.strukt->name);
				writeStatic(writer, " has purity ");
				writePurity(writer, d.strukt->purity);
				writeStatic(writer, ", but member type ");
				writeStructInst(writer, d.member);
				writeStatic(writer, " has purity ");
				writePurity(writer, d.member->bestCasePurity);
			},
			[&](const Diag::RelativeImportReachesPastRoot d) {
				writeStatic(writer, "importing ");
				writeRelPath(writer, d.imported);
				writeStatic(writer, " reaches above the source directory");
				//TODO: recommend a compiler option to fix this
			},
			[&](const Diag::SendFunDoesNotReturnFut d) {
				writeStatic(writer, "a fun-ref should return a fut, but returns ");
				writeType(writer, d.actualReturnType);
			},
			[&](const Diag::SpecBuiltinNotSatisfied d) {
				writeStatic(writer, "trying to call ");
				writeName(writer, d.called->name());
				writeStatic(writer, ", but ");
				writeType(writer, d.type);
				const char* message = [&]() {
					switch (d.kind) {
						case SpecBody::Builtin::Kind::data:
							return " is not 'data'";
						case SpecBody::Builtin::Kind::send:
							return " is not 'send'";
						default:
							assert(0);
					}
				}();
				writeStatic(writer, message);
			},
			[&](const Diag::SpecImplHasSpecs d) {
				writeStatic(writer, "spec implementation ");
				writeName(writer, d.funName);
				writeStatic(writer, " has specs itself; currently this is not allowed");
			},
			[&](const Diag::SpecImplNotFound d) {
				writeStatic(writer, "no implementation was found for spec signature ");
				writeName(writer, d.sigName);
			},
			[&](const Diag::TypeConflict d) {
				writeStatic(writer, "the type of the expression conflicts with its expected type.\n\texpected: ");
				writeType(writer, d.expected);
				writeStatic(writer, "\n\tactual: ");
				writeType(writer, d.actual);
			},
			[&](const Diag::TypeNotSendable) {
				writeStatic(writer, "this type is not sendable and should not appear in an interface");
			},
			[&](const Diag::WriteToNonExistentField d) {
				writeStatic(writer, "type ");
				writeType(writer, d.targetType);
				writeStatic(writer, " has no field ");
				writeName(writer, d.fieldName);
			},
			[&](const Diag::WriteToNonMutableField d) {
				writeStatic(writer, "field ");
				writeName(writer, d.field->name);
				writeStatic(writer, " is not mutable");
			},
			[&](const Diag::WrongNumberNewStructArgs d) {
				writeStatic(writer, "struct initializer expected to get ");
				writeNat(writer, d.nExpectedArgs);
				writeStatic(writer, " args, but got ");
				writeNat(writer, d.nActualArgs);
			},
			[&](const Diag::WrongNumberTypeArgsForSpec d) {
				writeName(writer, d.decl->name);
				writeStatic(writer, " expected to get ");
				writeNat(writer, d.nExpectedTypeArgs);
				writeStatic(writer, " type args, but got ");
				writeNat(writer, d.nActualTypeArgs);
			},
			[&](const Diag::WrongNumberTypeArgsForStruct d) {
				writeName(writer, d.decl.name());
				writeStatic(writer, " expected to get ");
				writeNat(writer, d.nExpectedTypeArgs);
				writeStatic(writer, " type args, but got ");
				writeNat(writer, d.nActualTypeArgs);
			});
	}

	void showDiagnostic(Writer* writer, const FilesInfo fi, const Diagnostic d) {
		writeWhere(writer, fi, d.where);
		writeChar(writer, ' ');
		writeDiag(writer, fi, d.diag);
		writeChar(writer, '\n');
	}
}

void printDiagnostics(const Diagnostics diagnostics) {
	Arena tempArena {};
	Writer writer { &tempArena };
	//TODO: sort diagnostics by file / range
	const Arr<const Diags> groups = sortAndGroup(
		&tempArena,
		diagnostics.diagnostics,
		[&](const Diagnostic a, const Diagnostic b) {
			return comparePathAndStorageKind(a.where.pathAndStorageKind, b.where.pathAndStorageKind);
		});

	for (const Diags group : groups)
		for (const Diagnostic d : group)
			showDiagnostic(&writer, diagnostics.filesInfo, d);

	printf("%s\n", finishWriterToCStr(&writer));
}
