#include "./showDiag.h"

#include "../util/arrUtil.h"

namespace {
	struct Output {
		Output& operator<<(const Str s) {
			printf("%.*s", safeSizeTToInt(s.size), s.begin());
			return *this;
		}

		Output& operator<<(const size_t s) {
			printf("%zu", s);
			return *this;
		}

		Output& operator<<(const uint u) {
			return *this << static_cast<size_t>(u);
		}

		Output& operator<<(const char* c) {
			printf("%s", c);
			return *this;
		}

		Output& operator<<(const char c) {
			printf("%c", c);
			return *this;
		}
	};

	Output& operator<<(Output& out, const Path* p) {
		if (p->parent.has())
			out << p->parent.force();
		return out << '/' << p->baseName;
	}

	Output& operator<<(Output& out, const LineAndColumn lc) {
		return out << (lc.line + 1) << ':' << (lc.column + 1);
	}

	void showPos(Output& out, const LineAndColumnGetter lc, const Pos pos) {
		out << lineAndColumnAtPos(lc, pos);
	}

	void showRange(Output& out, const LineAndColumnGetter lc, const SourceRange range) {
		showPos(out, lc, range.start);
		out << '-';
		showPos(out, lc, range.end);
	}

	void showChar(Output& out, char c) {
		switch (c) {
			case '\n':
				out << "\\n";
				break;
			case '\t':
				out << "\\t";
				break;
			default:
				out << c;
				break;
		}
	}

	Output& operator<<(Output& out, const ParseDiag d) {
		d.match(
			[&](const ParseDiag::ExpectedCharacter) {
				todo<void>("expectedcharacter");
			},
			[&](const ParseDiag::ExpectedIndent) {
				out << "expected an indent";
			},
			[&](const ParseDiag::ExpectedPurityAfterSpace) {
				out << "after trialing space, expected to parse 'mutable' or 'sendable'";
			},
			[&](const ParseDiag::LeadingSpace) {
				todo<void>("leadingspace");
			},
			[&](const ParseDiag::MatchWhenNewMayNotAppearInsideArg) {
				todo<void>("mwn");
			},
			[&](const ParseDiag::MustEndInBlankLine) {
				out << "file must end in a blank line";
			},
			[&](const ParseDiag::TrailingSpace) {
				todo<void>("trailingspace");
			},
			[&](const ParseDiag::TypeParamCantHaveTypeArgs) {
				out << "a type parameter can't have type arguments";
			},
			[&](const ParseDiag::UnexpectedCharacter d) {
				out << "unexpected character '";
				showChar(out, d.ch);
				out << "'";
			},
			[&](const ParseDiag::UnionCantBeEmpty) {
				out << "union type can't be empty";
			},
			[&](const ParseDiag::WhenMustHaveElse) {
				out << "'when' expression must end in 'else'";
			});
		return out;
	}

	Output& operator<<(Output& out, const Type type) {
		type.match(
			[&](const Type::Bogus) {
				unreachable<void>();
			},
			[&](const TypeParam* p) {
				out << '?' << p->name;
			},
			[&](const StructInst* s) {
				out << s->decl->name;
				if (!isEmpty(s->typeArgs)) {
					bool first = true;
					for (const Type t : s->typeArgs) {
						out << (first ? '<' : ' ') << t;
						first = false;
					}
					out << '>';
				}
			});
		return out;
	}

	Output& operator<<(Output& out, const Diag d) {
		d.match(
			[&](const Diag::CantCallNonNoCtx) {
				out << "a 'noctx' fun can't call a non-'noctx' fun.";
			},
			[&](const Diag::CantCallSummon) {
				out << "non-'summon' fun can't call 'summon' fun";
			},
			[&](const Diag::CantCallUnsafe) {
				out << "non-'trusted' and non-'unsafe' function can't call 'unsafe' function";
			},
			[&](const Diag::CantCreateNonRecordStruct d) {
				out << "non-record struct " << d.strukt->name << " can't be constructed";
			},
			[&](const Diag::CantInferTypeArguments) {
				out << "can't infer type arguments";
			},
			[&](const Diag::CircularImport) {
				todo<void>("print circular import diag");
			},
			[&](const Diag::CommonTypesMissing) {
				out << "common types are missing from 'include.nz'";
			},
			[&](const Diag::DuplicateDeclaration) {
				todo<void>("print duplicate declaration diag");
			},
			[&](const Diag::ExpectedTypeIsNotALambda d) {
				if (d.expectedType.has())
					out << "the expected type at the lambda is " << d.expectedType.force() << ", which is not a lambda type";
				else
					out << "there is no expected type at this location; lambdas need an expected type";
			},
			[&](const Diag::FileDoesNotExist) {
				// We handle this specially
				unreachable<void>();
			},
			[&](const Diag::MultipleFunctionCandidates d) {
				out << "multiple fun candidates match: ";
				for (const size_t i : Range{d.candidates.size}) {
					if (i != 0)
						out << ", ";
					out << d.candidates[i].name();
				}
			},
			[&](const Diag::NameNotFound d) {
				const char* kind = [&]() {
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
				out << kind << " name not found: " << d.name;
			},
			[&](const Diag::NoSuchFunction d) {
				out << "no such function " << d.name << " exists with those argument types and return type";
			},
			[&](const Diag::ParamShadowsPrevious) {
				todo<void>("print paramshadowsprevious");
			},
			[&](const ParseDiag pd) {
				out << pd;
			},
			[&](const Diag::ShouldNotHaveTypeParamsInIface) {
				out << "a member function of an interface should not have type parameters";
			},
			[&](const Diag::TypeConflict d) {
				out << "the type of the expression conflicts with its expected type.\nexpected: "
					<< d.expected
					<< "\nactual: "
					<< d.actual;
			},
			[&](const Diag::TypeNotSendable) {
				out << "this type is not sendable and should not appear in an interface";
			},
			[&](const Diag::WrongNumberNewStructArgs d) {
				out << "struct initializer expected to get " << d.nExpectedArgs << " args, but got " << d.nActualArgs;
			},
			[&](const Diag::WrongNumberTypeArgsForSpec d) {
				out << d.decl->name << ": expected to get " << d.nExpectedTypeArgs << " type args, but got " << d.nActualTypeArgs;
			},
			[&](const Diag::WrongNumberTypeArgsForStruct d) {
				out << d.decl.name() << ": expected to get " << d.nExpectedTypeArgs << " type args, but got " << d.nActualTypeArgs;
			});
		return out;
	}

	void showDiagnostic(Output& out, const LineAndColumnGetter lc, const Diagnostic d) {
		out << d.where.path << ' ';
		showRange(out, lc, d.range);
		out << ' ' << d.diag << '\n';
	}
}

void printDiagnostics(const Diagnostics diagnostics) {
	Output out {};
	Arena tempArena {};
	//TODO: sort diagnostics by file / range
	const Arr<const Arr<const Diagnostic>> groups = sortAndGroup(tempArena, diagnostics.diagnostics, [&](const Diagnostic a, const Diagnostic b) {
		return comparePathAndStorageKind(a.where, b.where);
	});

	for (const Arr<const Diagnostic> group : groups) {
		if (group[0].diag.isFileDoesNotExist())
			out << group[0].where.path << ": file does not exist\n";
		else {
			for (const Diagnostic d : group)
				showDiagnostic(out, diagnostics.lineAndColumnGetters.mustGet(d.where), d);
		}
	}
}

