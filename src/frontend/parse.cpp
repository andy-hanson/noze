#include "./parse.h"

#include "./lexer.h"
#include "./parseExpr.h"
#include "./parseType.h"

namespace {
	const Arr<const TypeParamAst> parseTypeParams(Lexer& lexer) {
		if (tryTake(lexer, '<')) {
			ArrBuilder<const TypeParamAst> res {};
			do {
				const Pos start = curPos(lexer);
				take(lexer, '?');
				const Str name = takeName(lexer);
				res.add(lexer.arena, TypeParamAst{range(lexer, start), name});
			} while(tryTake(lexer, ", "));
			take(lexer, '>');
			return res.finish();
		} else
			return emptyArr<const TypeParamAst>();
	}

	PuritySpecifier parsePurity(Lexer& lexer) {
		if (tryTake(lexer, "mutable"))
			return PuritySpecifier::nonSendable;
		else if (tryTake(lexer, "sendable"))
			return PuritySpecifier::sendable;
		else
			return throwAtChar<const PuritySpecifier>(lexer, ParseDiag{ParseDiag::ExpectedPurityAfterSpace{}});
	}

	const ImportAst parseSingleImport(Lexer& lexer) {
		const Pos start = curPos(lexer);
		uint nDots = 0;
		while (tryTake(lexer, '.'))
			nDots++;

		Path const* path = rootPath(lexer.arena, takeName(lexer));
		while (tryTake(lexer, '.'))
			path = childPath(lexer.arena, path, takeName(lexer));
		path = addExtension(lexer.arena, path, strLiteral(".nz"));
		return ImportAst{range(lexer, start), nDots, path};
	}

	const Arr<const ParamAst> parseParams(Lexer& lexer) {
		take(lexer, '(');
		if (tryTake(lexer, ')'))
			return emptyArr<const ParamAst>();
		else {
			ArrBuilder<const ParamAst> res {};
			for (;;) {
				const Pos start = curPos(lexer);
				const Str name = takeName(lexer);
				take(lexer, ' ');
				const TypeAst type = parseType(lexer);
				res.add(lexer.arena, ParamAst{range(lexer, start), name, type});
				if (tryTake(lexer, ')'))
					break;
				take(lexer, ", ");
			}
			return res.finish();
		}
	}

	const SigAst parseSigAfterNameAndSpace(Lexer& lexer, const Pos start, const Str name) {
		const TypeAst returnType = parseType(lexer);
		const Arr<const ParamAst> params = parseParams(lexer);
		return SigAst{range(lexer, start), name, returnType, params};
	}

	const SigAst parseSig(Lexer& lexer) {
		const Pos start = curPos(lexer);
		const Str sigName = takeName(lexer);
		take(lexer, ' ');
		return parseSigAfterNameAndSpace(lexer, start, sigName);
	}

	const Arr<const ImportAst> parseImports(Lexer& lexer) {
		ArrBuilder<const ImportAst> imports {};
		do {
			imports.add(lexer.arena, parseSingleImport(lexer));
		} while (tryTake(lexer, ' '));
		take(lexer, '\n');
		return imports.finish();
	}

	const Arr<const SigAst> parseIndentedSigs(Lexer& lexer) {
		ArrBuilder<const SigAst> res {};
		do {
			res.add(lexer.arena, parseSig(lexer));
		} while (takeNewlineOrSingleDedent(lexer) == NewlineOrDedent::newline);
		return res.finish();
	}

	const SpecDeclAst parseSpec(Lexer& lexer, const Bool isPublic, const Pos start) {
		const Str name = takeName(lexer);
		const Arr<const TypeParamAst> typeParams = parseTypeParams(lexer);
		takeIndent(lexer);
		const Arr<const SigAst> sigs = parseIndentedSigs(lexer);
		return SpecDeclAst{range(lexer, start), isPublic, name, typeParams, sigs};
	}

	enum class SpaceOrNewlineOrIndent {
		space,
		newline,
		indent,
	};
	enum class NonFunKeyword {
		alias,
		builtin,
		iface,
		record,
		_union,
	};

	struct NonFunKeywordAndIndent {
		const NonFunKeyword keyword;
		const SpaceOrNewlineOrIndent after;
	};

	const Opt<const NonFunKeywordAndIndent> tryTake(Lexer& lexer, const CStr kwSpace, const CStr kwNl, const NonFunKeyword keyword) {
		if (tryTake(lexer, kwSpace))
			return some<const NonFunKeywordAndIndent>(NonFunKeywordAndIndent{keyword, SpaceOrNewlineOrIndent::space});
		else if (tryTake(lexer, kwNl)) {
			const SpaceOrNewlineOrIndent sni = tryTakeIndentAfterNewline(lexer)
				? SpaceOrNewlineOrIndent::indent
				: SpaceOrNewlineOrIndent::newline;
			return some<const NonFunKeywordAndIndent>(NonFunKeywordAndIndent{keyword, sni});
		} else
			return none<const NonFunKeywordAndIndent>();
	}

	const Opt<const NonFunKeywordAndIndent> parseNonFunKeyword(Lexer& lexer) {
		switch (curChar(lexer)) {
			case 'a':
				return tryTake(lexer, "alias ", "alias\n", NonFunKeyword::alias);
			case 'b':
				return tryTake(lexer, "builtin ", "builtin\n", NonFunKeyword::builtin);
			case 'i':
				return tryTake(lexer, "iface ", "iface\n", NonFunKeyword::iface);
			case 'r':
				return tryTake(lexer, "record ", "record\n", NonFunKeyword::record);
			case 'u':
				return tryTake(lexer, "union ", "union\n", NonFunKeyword::_union);
			default:
				return none<const NonFunKeywordAndIndent>();
		}
	}

	const Arr<const StructDeclAst::Body::Fields::Field> parseFields(Lexer& lexer) {
		ArrBuilder<const StructDeclAst::Body::Fields::Field> res {};
		do {
			const Str name = takeName(lexer);
			take(lexer, ' ');
			const TypeAst type = parseType(lexer);
			res.add(lexer.arena, StructDeclAst::Body::Fields::Field{name, type});
		} while (takeNewlineOrSingleDedent(lexer) == NewlineOrDedent::newline);
		return res.finish();
	}

	const Arr<const TypeAst::InstStruct> parseUnionMembers(Lexer& lexer) {
		ArrBuilder<const TypeAst::InstStruct> res {};
		do {
			const Pos start = curPos(lexer);
			const Str name = takeName(lexer);
			const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
			res.add(lexer.arena, TypeAst::InstStruct{range(lexer, start), name, typeArgs});
		} while (takeNewlineOrSingleDedent(lexer) == NewlineOrDedent::newline);
		return res.finish();
	}

	struct SpecUsesAndSpace {
		const Arr<const SpecUseAst> specUses;
		const Bool spaceAtEnd;
	};

	const SpecUsesAndSpace parseSpecUses(Lexer& lexer) {
		ArrBuilder<const SpecUseAst> su {};
		const Bool spaceAtEnd = [&]() {
			for (;;) {
				if (!tryTake(lexer, ' '))
					return False;
				const Pos start = curPos(lexer);
				if (tryTake(lexer, '$')) {
					const Str name = takeName(lexer);
					const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
					su.add(lexer.arena, SpecUseAst{range(lexer, start), name, typeArgs});
				} else
					return True;
			}
		}();
		return SpecUsesAndSpace{su.finish(), spaceAtEnd};
	}

	struct SigFlagsAndKwBody {
		const Bool noCtx;
		const Bool summon;
		const Bool unsafe;
		const Bool trusted;
		const Opt<const FunBodyAst> body; // 'builtin' or 'extern'
	};

	enum class AfterSigKeyword {
		noCtx,
		summon,
		unsafe,
		trusted,
		builtin,
		_extern,
	};

	AfterSigKeyword parseAfterSigKeyword(Lexer& lexer) {
		const Str name = takeName(lexer);
		if (strEqLiteral(name, "noctx"))
			return AfterSigKeyword::noCtx;
		else if (strEqLiteral(name, "summon"))
			return AfterSigKeyword::summon;
		else if (strEqLiteral(name, "unsafe"))
			return AfterSigKeyword::unsafe;
		else if (strEqLiteral(name, "trusted"))
			return AfterSigKeyword::trusted;
		else if (strEqLiteral(name, "builtin"))
			return AfterSigKeyword::builtin;
		else if (strEqLiteral(name, "extern"))
			return AfterSigKeyword::_extern;
		else
			return todo<const AfterSigKeyword>("bad after-sig keyword");
	}

	const SigFlagsAndKwBody parseSigFlagsAndKwBody(Lexer& lexer) {
		Cell<const Bool> noCtx { False };
		Cell<const Bool> summon { False };
		Cell<const Bool> unsafe { False };
		Cell<const Bool> trusted { False };
		Cell<const Bool> builtin { False };
		Cell<const Bool> _extern { False };

		auto setIt = [](Cell<const Bool>& b) {
			if (b.get())
				todo<void>("duplicate");
			b.set(True);
		};

		do {
			const AfterSigKeyword kw = parseAfterSigKeyword(lexer);
			switch (kw) {
				case AfterSigKeyword::noCtx:
					setIt(noCtx);
					break;
				case AfterSigKeyword::summon:
					setIt(summon);
					break;
				case AfterSigKeyword::unsafe:
					setIt(unsafe);
					break;
				case AfterSigKeyword::trusted:
					setIt(trusted);
					break;
				case AfterSigKeyword::builtin:
					builtin.set(True);
					goto end_of_loop;
				case AfterSigKeyword::_extern:
					_extern.set(True);
					goto end_of_loop;

			}
		} while (tryTake(lexer, ' '));
		end_of_loop:

		if (unsafe.get() && trusted.get())
			todo<void>("'unsafe trusted' is redundant");
		if (builtin.get() && trusted.get())
			todo<void>("'builtin trusted' is silly as builtin fun has no body");
		if (_extern.get() && trusted.get())
			todo<void>("'extern trusted' is silly as extern fun has no body");
		if (_extern.get()) {
			if (noCtx.get())
				todo<void>("'noctx extern' is redundant");
			noCtx.set(True);
		}

		Opt<const FunBodyAst> body = builtin.get() ? some<const FunBodyAst>(FunBodyAst{FunBodyAst::Builtin{}})
			: _extern.get() ? some<const FunBodyAst>(FunBodyAst{FunBodyAst::Extern{}})
			: none<const FunBodyAst>();
		return SigFlagsAndKwBody{noCtx.get(), summon.get(), unsafe.get(), trusted.get(), body};
	}

	const FunDeclAst parseFun(
		Lexer& lexer,
		const Bool isPublic,
		const Pos start,
		const Str name,
		const Arr<const TypeParamAst> typeParams
	) {
		const SigAst sig = parseSigAfterNameAndSpace(lexer, start, name);
		const SpecUsesAndSpace su = parseSpecUses(lexer);
		const SigFlagsAndKwBody flagz = su.spaceAtEnd
			? parseSigFlagsAndKwBody(lexer)
			: SigFlagsAndKwBody{False, False, False, False, none<const FunBodyAst>()};
		const FunBodyAst body = flagz.body.has() ? flagz.body.force() : FunBodyAst(parseFunExprBody(lexer));
		return FunDeclAst{isPublic, typeParams, sig, su.specUses, flagz.noCtx, flagz.summon, flagz.unsafe, flagz.trusted, body};
	}

	void parseStructOrFun(
		Lexer& lexer,
		const Bool isPublic,
		const Pos start,
		ArrBuilder<const StructAliasAst>& structAliases,
		ArrBuilder<const StructDeclAst>& structs,
		ArrBuilder<const FunDeclAst>& funs
	) {
		using Body = StructDeclAst::Body;
		const Str name = takeName(lexer);
		const Arr<const TypeParamAst> typeParams = parseTypeParams(lexer);
		take(lexer, ' ');

		const Opt<const NonFunKeywordAndIndent> opKwAndIndent = parseNonFunKeyword(lexer);
		if (opKwAndIndent.has()) {
			const NonFunKeywordAndIndent kwAndIndent = opKwAndIndent.force();
			const NonFunKeyword kw = kwAndIndent.keyword;
			const SpaceOrNewlineOrIndent after = kwAndIndent.after;
			const Opt<const PuritySpecifier> purity = after == SpaceOrNewlineOrIndent::space
				? some<const PuritySpecifier>(parsePurity(lexer))
				: none<const PuritySpecifier>();

			const Bool tookIndent = [&]() {
				switch (after) {
					case SpaceOrNewlineOrIndent::space:
						return enumEq(takeNewlineOrIndent(lexer), NewlineOrIndent::indent);
					case SpaceOrNewlineOrIndent::newline:
						return False;
					case SpaceOrNewlineOrIndent::indent:
						return True;
					default:
						return unreachable<Bool>();
				}
			}();

			if (kw == NonFunKeyword::alias) {
				if (purity.has())
					todo<void>("alias shouldn't have purity");
				const TypeAst::InstStruct target = parseStructType(lexer);
				takeDedent(lexer);
				structAliases.add(lexer.arena, StructAliasAst{range(lexer, start), isPublic, name, typeParams, target});
			} else {
				const Body body = [&]() {
					switch (kw) {
						case NonFunKeyword::alias:
							return unreachable<const Body>();
						case NonFunKeyword::builtin:
							if (tookIndent)
								todo<void>("shouldn't indent after builtin");
							return Body{Body::Builtin{}};
						case NonFunKeyword::record: {
							const Arr<const Body::Fields::Field> fields = tookIndent ? parseFields(lexer) : emptyArr<const Body::Fields::Field>();
							return Body{Body::Fields{fields}};
						}
						case NonFunKeyword::_union:
							return tookIndent
								? Body{Body::Union{parseUnionMembers(lexer)}}
								: throwAtChar<const Body>(lexer, ParseDiag{ParseDiag::UnionCantBeEmpty{}});
						case NonFunKeyword::iface:
							if (!tookIndent)
								todo<void>("iface can't be empty");
							return Body{Body::Iface{parseIndentedSigs(lexer)}};
						default:
							return unreachable<const Body>();
					}
				}();
				structs.add(lexer.arena, StructDeclAst{range(lexer, start), isPublic, name, typeParams, purity, body});
			}
		} else
			funs.add(lexer.arena, parseFun(lexer, isPublic, start, name, typeParams));
	}

	const FileAst doParseFile(Lexer& lexer) {
		skipBlankLines(lexer);
		const Arr<const ImportAst> imports = tryTake(lexer, "import ")
			? parseImports(lexer)
			: emptyArr<const ImportAst>();

		ArrBuilder<const SpecDeclAst> specs {};
		ArrBuilder<const StructAliasAst> structAliases {};
		ArrBuilder<const StructDeclAst> structs {};
		ArrBuilder<const FunDeclAst> funs {};

		Bool isPublic = True;
		for (;;) {
			skipBlankLines(lexer);
			if (tryTake(lexer, '\0'))
				break;
			if (tryTake(lexer, "private\n")) {
				if (!isPublic)
					todo<void>("already private");
				isPublic = False;
				skipBlankLines(lexer);
			}

			const Pos start = curPos(lexer);
			if (tryTake(lexer, '$'))
				specs.add(lexer.arena, parseSpec(lexer, isPublic, start));
			else
				parseStructOrFun(lexer, isPublic, start, structAliases, structs, funs);
		}

		return FileAst{imports, specs.finish(), structAliases.finish(), structs.finish(), funs.finish()};
	}
}

const Result<const FileAst, const ParseDiagnostic> parseFile(Arena& astArena, const NulTerminatedStr source) {
	try {
		Lexer lexer = createLexer(astArena, source);
		return success<const FileAst, const ParseDiagnostic>(doParseFile(lexer));
	} catch (ParseDiagnostic p) {
		return failure<const FileAst, const ParseDiagnostic>(p);
	}
}
