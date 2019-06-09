#include "./parse.h"

#include "./lexer.h"
#include "./parseExpr.h"
#include "./parseType.h"

namespace {
	const Arr<const TypeParamAst> parseTypeParams(Lexer& lexer) {
		if (lexer.tryTake('<')) {
			auto res = ArrBuilder<const TypeParamAst>{};
			do {
				const Pos start = lexer.at();
				lexer.take('?');
				const Str name = lexer.takeName();
				res.add(lexer.arena, TypeParamAst{lexer.range(start), name});
			} while(lexer.tryTake(", "));
			return res.finish();
		} else
			return emptyArr<const TypeParamAst>();
	}

	PuritySpecifier parsePurity(Lexer& lexer) {
		if (lexer.tryTake("mutable"))
			return PuritySpecifier::nonSendable;
		else if (lexer.tryTake("sendable"))
			return PuritySpecifier::sendable;
		else
			return lexer.throwAtChar<const PuritySpecifier>(ParseDiag{ParseDiag::Kind::expectedPurityAfterSpace});
	}

	const ImportAst parseSingleImport(Lexer& lexer) {
		const Pos start = lexer.at();
		uint nDots = 0;
		while (lexer.tryTake('.'))
			nDots++;

		Path const* path = Path::root(lexer.pathArena, lexer.takeName());
		while (lexer.tryTake('.'))
			path = childPath(lexer.pathArena, path, lexer.takeName());
		path = addExt(lexer.pathArena, path, strLiteral(".nz"));
		return ImportAst{lexer.range(start), nDots, path};
	}

	const Arr<const ParamAst> parseParams(Lexer& lexer) {
		lexer.take('(');
		if (lexer.tryTake(')'))
			return emptyArr<const ParamAst>();
		else {
			auto res = ArrBuilder<const ParamAst>{};
			while (true) {
				const Pos start = lexer.at();
				const Str name = lexer.takeName();
				lexer.take(' ');
				const TypeAst type = parseType(lexer);
				res.add(lexer.arena, ParamAst{lexer.range(start), name, type});
				if (lexer.tryTake(')'))
					break;
				lexer.take(", ");
			}
			return res.finish();
		}
	}

	const SigAst parseSigAfterNameAndSpace(Lexer& lexer, const Pos start, const Str name) {
		const TypeAst returnType = parseType(lexer);
		const Arr<const ParamAst> params = parseParams(lexer);
		return SigAst{lexer.range(start), name, returnType, params};
	}

	const SigAst parseSig(Lexer& lexer) {
		const Pos start = lexer.at();
		const Str sigName = lexer.takeName();
		lexer.take(' ');
		return parseSigAfterNameAndSpace(lexer, start, sigName);
	}

	const Arr<const ImportAst> parseImports(Lexer& lexer) {
		auto imports = ArrBuilder<const ImportAst>{};
		do {
			imports.add(lexer.arena, parseSingleImport(lexer));
		} while (lexer.tryTake(' '));
		lexer.take('\n');
		return imports.finish();
	}

	const Arr<const SigAst> parseIndentedSigs(Lexer& lexer) {
		auto res = ArrBuilder<const SigAst>{};
		do {
			res.add(lexer.arena, parseSig(lexer));
		} while (lexer.takeNewlineOrSingleDedent() == Lexer::NewlineOrDedent::newline);
		return res.finish();
	}

	const SpecDeclAst parseSpec(Lexer& lexer, const bool isPublic, const Pos start) {
		const Str name = lexer.takeName();
		const Arr<const TypeParamAst> typeParams = parseTypeParams(lexer);
		lexer.takeIndent();
		const Arr<const SigAst> sigs = parseIndentedSigs(lexer);
		return SpecDeclAst{lexer.range(start), isPublic, name, typeParams, sigs};
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

	const Opt<const NonFunKeywordAndIndent> tryTake(Lexer& lexer, const char* kwSpace, const char* kwNl, const NonFunKeyword keyword) {
		if (lexer.tryTake(kwSpace))
			return some<const NonFunKeywordAndIndent>(NonFunKeywordAndIndent{keyword, SpaceOrNewlineOrIndent::space});
		else if (lexer.tryTake(kwNl)) {
			const SpaceOrNewlineOrIndent sni = lexer.tryTakeIndentAfterNewline() ? SpaceOrNewlineOrIndent::indent : SpaceOrNewlineOrIndent::newline;
			return some<const NonFunKeywordAndIndent>(NonFunKeywordAndIndent{keyword, sni});
		} else
			return none<const NonFunKeywordAndIndent>();
	}

	const Opt<const NonFunKeywordAndIndent> parseNonFunKeyword(Lexer& lexer) {
		switch (lexer.cur()) {
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
		auto res = ArrBuilder<const StructDeclAst::Body::Fields::Field>{};
		do {
			const Str name = lexer.takeName();
			lexer.take(' ');
			const TypeAst type = parseType(lexer);
			res.add(lexer.arena, StructDeclAst::Body::Fields::Field{name, type});
		} while (lexer.takeNewlineOrSingleDedent() == Lexer::NewlineOrDedent::newline);
		return res.finish();
	}

	const Arr<const TypeAst::InstStruct> parseUnionMembers(Lexer& lexer) {
		auto res = ArrBuilder<const TypeAst::InstStruct>{};
		do {
			const Pos start = lexer.at();
			const Str name = lexer.takeName();
			const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
			res.add(lexer.arena, TypeAst::InstStruct{lexer.range(start), name, typeArgs});
		} while (lexer.takeNewlineOrSingleDedent() == Lexer::NewlineOrDedent::newline);
		return res.finish();
	}

	struct SpecUsesAndSpace {
		const Arr<const SpecUseAst> specUses;
		const bool spaceAtEnd;
	};

	const SpecUsesAndSpace parseSpecUses(Lexer& lexer) {
		auto su = ArrBuilder<const SpecUseAst>{};
		const bool spaceAtEnd = [&]() {
			while (true) {
				if (!lexer.tryTake(' '))
					return false;
				const Pos start = lexer.at();
				if (lexer.tryTake('$')) {
					const Str name = lexer.takeName();
					const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
					su.add(lexer.arena, SpecUseAst{lexer.range(start), name, typeArgs});
				} else
					return true;
			}
		}();
		return SpecUsesAndSpace{su.finish(), spaceAtEnd};
	}

	struct SigFlagsAndKwBody {
		const bool noCtx;
		const bool summon;
		const bool unsafe;
		const bool trusted;
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
		const Str name = lexer.takeName();
		if (strEq(name, "noctx"))
			return AfterSigKeyword::noCtx;
		else if (strEq(name, "summon"))
			return AfterSigKeyword::summon;
		else if (strEq(name, "unsafe"))
			return AfterSigKeyword::unsafe;
		else if (strEq(name, "trusted"))
			return AfterSigKeyword::trusted;
		else if (strEq(name, "builtin"))
			return AfterSigKeyword::builtin;
		else if (strEq(name, "extern"))
			return AfterSigKeyword::_extern;
		else
			return todo<const AfterSigKeyword>("bad after-sig keyword");
	}

	const SigFlagsAndKwBody parseSigFlagsAndKwBody(Lexer& lexer) {
		bool noCtx = false;
		bool summon = false;
		bool unsafe = false;
		bool trusted = false;
		bool builtin = false;
		bool _extern = false;

		auto setIt = [](bool& b) {
			if (b)
				todo<void>("duplicate");
			b = true;
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
					builtin = true;
					goto end_of_loop;
				case AfterSigKeyword::_extern:
					_extern = true;
					goto end_of_loop;

			}
		} while (lexer.tryTake(' '));
		end_of_loop:

		if (unsafe && trusted)
			todo<void>("'unsafe trusted' is redundant");
		if (builtin && trusted)
			todo<void>("'builtin trusted' is silly as builtin fun has no body");
		if (_extern && trusted)
			todo<void>("'extern trusted' is silly as extern fun has no body");
		if (_extern) {
			if (noCtx)
				todo<void>("'noctx extern' is redundant");
			noCtx = true;
		}

		Opt<const FunBodyAst> body = builtin ? some<const FunBodyAst>(FunBodyAst{FunBodyAst::Kind::builtin})
			: _extern ? some<const FunBodyAst>(FunBodyAst{FunBodyAst::Kind::_extern})
			: none<const FunBodyAst>();
		return SigFlagsAndKwBody{noCtx, summon, unsafe, trusted, body};
	}

	const FunDeclAst parseFun(
		Lexer& lexer,
		const bool isPublic,
		const Pos start,
		const Str name,
		const Arr<const TypeParamAst> typeParams
	) {
		const SigAst sig = parseSigAfterNameAndSpace(lexer, start, name);
		const SpecUsesAndSpace su = parseSpecUses(lexer);
		const SigFlagsAndKwBody flagz = su.spaceAtEnd
			? parseSigFlagsAndKwBody(lexer)
			: SigFlagsAndKwBody{false, false, false, false, none<const FunBodyAst>()};
		const FunBodyAst body = flagz.body.has() ? flagz.body.force() : FunBodyAst(parseFunExprBody(lexer));
		return FunDeclAst{isPublic, typeParams, sig, su.specUses, flagz.noCtx, flagz.summon, flagz.unsafe, flagz.trusted, body};
	}

	void parseStructOrFun(
		Lexer& lexer,
		const bool isPublic,
		const Pos start,
		ArrBuilder<const StructAliasAst>& structAliases,
		ArrBuilder<const StructDeclAst>& structs,
		ArrBuilder<const FunDeclAst>& funs
	) {
		using Body = StructDeclAst::Body;
		const Str name = lexer.takeName();
		const Arr<const TypeParamAst> typeParams = parseTypeParams(lexer);
		lexer.take(' ');

		const Opt<const NonFunKeywordAndIndent> opKwAndIndent = parseNonFunKeyword(lexer);
		if (opKwAndIndent.has()) {
			const NonFunKeywordAndIndent kwAndIndent = opKwAndIndent.force();
			const NonFunKeyword kw = kwAndIndent.keyword;
			const SpaceOrNewlineOrIndent after = kwAndIndent.after;
			const Opt<const PuritySpecifier> purity = after == SpaceOrNewlineOrIndent::space
				? some<const PuritySpecifier>(parsePurity(lexer))
				: none<const PuritySpecifier>();

			const bool tookIndent = [&]() {
				switch (after) {
					case SpaceOrNewlineOrIndent::space:
						return lexer.takeNewlineOrIndent() == Lexer::NewlineOrIndent::indent;
					case SpaceOrNewlineOrIndent::newline:
						return false;
					case SpaceOrNewlineOrIndent::indent:
						return true;
					default:
						return unreachable<bool>();
				}
			}();

			if (kw == NonFunKeyword::alias) {
				if (purity.has())
					todo<void>("alias shouldn't have purity");
				const TypeAst::InstStruct target = parseStructType(lexer);
				lexer.takeDedent();
				structAliases.add(lexer.arena, StructAliasAst{lexer.range(start), isPublic, name, typeParams, target});
			} else {
				const Body body = [&]() {
					switch (kw) {
						case NonFunKeyword::alias:
							return unreachable<const Body>();
						case NonFunKeyword::builtin:
							if (tookIndent)
								todo<void>("shouldn't indent after builtin");
							return Body{Body::Kind::builtin};
						case NonFunKeyword::record: {
							const Arr<const Body::Fields::Field> fields = tookIndent ? parseFields(lexer) : emptyArr<const Body::Fields::Field>();
							return Body{Body::Fields{fields}};
						}
						case NonFunKeyword::_union:
							return tookIndent
								? Body{Body::Union{parseUnionMembers(lexer)}}
								: lexer.throwAtChar<const Body>(ParseDiag{ParseDiag::Kind::unionCantBeEmpty});
						case NonFunKeyword::iface:
							if (!tookIndent)
								todo<void>("iface can't be empty");
							return Body{Body::Iface{parseIndentedSigs(lexer)}};
						default:
							return unreachable<const Body>();
					}
				}();
				structs.add(lexer.arena, StructDeclAst{lexer.range(start), isPublic, name, typeParams, purity, body});
			}
		} else
			funs.add(lexer.arena, parseFun(lexer, isPublic, start, name, typeParams));
	}

	const FileAst doParseFile(Lexer& lexer) {
		lexer.skipBlankLines();
		const Arr<const ImportAst> imports = lexer.tryTake("import ")
			? parseImports(lexer)
			: emptyArr<const ImportAst>();

		auto specs = ArrBuilder<const SpecDeclAst>{};
		auto structAliases = ArrBuilder<const StructAliasAst>{};
		auto structs = ArrBuilder<const StructDeclAst>{};
		auto funs = ArrBuilder<const FunDeclAst>{};

		bool isPublic = true;
		while (true) {
			lexer.skipBlankLines();
			if (lexer.tryTake('0'))
				break;
			if (lexer.tryTake("private\n")) {
				if (!isPublic)
					todo<void>("already private");
				isPublic = false;
				lexer.skipBlankLines();
			}

			const Pos start = lexer.at();
			if (lexer.tryTake('$'))
				specs.add(lexer.arena, parseSpec(lexer, isPublic, start));
			else
				parseStructOrFun(lexer, isPublic, start, structAliases, structs, funs);
		}

		return FileAst{imports, specs.finish(), structAliases.finish(), structs.finish(), funs.finish()};
	}
}

const Result<const FileAst, const ParseDiagnostic> parseFile(
	Arena& astArena,
	Arena& pathArena,
	const NulTerminatedStr source
) {
	try {
		Lexer lexer = createLexer(astArena, pathArena, source);
		return success<const FileAst, const ParseDiagnostic>(doParseFile(lexer));
	} catch (ParseDiagnostic p) {
		return failure<const FileAst, const ParseDiagnostic>(p);
	}
}
