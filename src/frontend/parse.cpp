#include "./parse.h"

#include "./lexer.h"
#include "./parseExpr.h"
#include "./parseType.h"

namespace {
	const Arr<const TypeParamAst> parseTypeParams(Lexer* lexer) {
		if (tryTake(lexer, '<')) {
			ArrBuilder<const TypeParamAst> res {};
			do {
				const Pos start = curPos(lexer);
				take(lexer, '?');
				const Sym name = takeName(lexer);
				add<const TypeParamAst>(lexer->arena, &res, TypeParamAst{range(lexer, start), name});
			} while(tryTake(lexer, ", "));
			take(lexer, '>');
			return finishArr(&res);
		} else
			return emptyArr<const TypeParamAst>();
	}

	PuritySpecifier parsePurity(Lexer* lexer) {
		if (tryTake(lexer, "mut"))
			return PuritySpecifier::mut;
		else if (tryTake(lexer, "sendable"))
			return PuritySpecifier::sendable;
		else if (tryTake(lexer, "force-sendable"))
			return PuritySpecifier::forceSendable;
		else
			return throwAtChar<const PuritySpecifier>(lexer, ParseDiag{ParseDiag::ExpectedPurityAfterSpace{}});
	}

	const ImportAst parseSingleImport(Lexer* lexer) {
		const Pos start = curPos(lexer);
		uint nDots = 0;
		while (tryTake(lexer, '.'))
			nDots++;

		Path const* path = rootPath(lexer->arena, takeNameAsStr(lexer));
		while (tryTake(lexer, '.'))
			path = childPath(lexer->arena, path, takeNameAsStr(lexer));
		path = addExtension(lexer->arena, path, strLiteral("nz"));
		return ImportAst{range(lexer, start), nDots, path};
	}

	struct ParamsAndMaybeDedent {
		const Arr<const ParamAst> params;
		// 0 if we took a newline but it didn't change the indent level from before parsing params.
		const Opt<const size_t> dedents;
	};

	const ParamAst parseSingleParam(Lexer* lexer) {
		const Pos start = curPos(lexer);
		const Sym name = takeName(lexer);
		take(lexer, ' ');
		const TypeAst type = parseType(lexer);
		return ParamAst{range(lexer, start), name, type};
	}

	const Arr<const ParamAst> parseParenthesizedParams(Lexer* lexer) {
		take(lexer, '(');
		if (tryTake(lexer, ')'))
			return emptyArr<const ParamAst>();
		else {
			ArrBuilder<const ParamAst> res {};
			for (;;) {
				add<const ParamAst>(lexer->arena, &res, parseSingleParam(lexer));
				if (tryTake(lexer, ')'))
					break;
				take(lexer, ", ");
			}
			return finishArr(&res);
		}
	}

	const ParamsAndMaybeDedent parseIndentedParams(Lexer* lexer) {
		ArrBuilder<const ParamAst> res {};
		for (;;) {
			add<const ParamAst>(lexer->arena, &res, parseSingleParam(lexer));
			const size_t dedents = takeNewlineOrDedentAmount(lexer);
			if (dedents != 0)
				return ParamsAndMaybeDedent{finishArr(&res), some<const size_t>(dedents - 1)};
		}
	}

	const ParamsAndMaybeDedent parseParams(Lexer* lexer) {
		const Opt<const NewlineOrIndent> opNi = tryTakeNewlineOrIndent(lexer);
		if (has(opNi))
			switch (force(opNi)) {
				case NewlineOrIndent::newline:
					return ParamsAndMaybeDedent{emptyArr<const ParamAst>(), some<const size_t>(0)};
				case NewlineOrIndent::indent:
					return parseIndentedParams(lexer);
				default:
					assert(0);
			}
		else
			return ParamsAndMaybeDedent{parseParenthesizedParams(lexer), none<const size_t>()};
	}

	struct SigAstAndMaybeDedent {
		const SigAst sig;
		const Opt<const size_t> dedents;
	};

	struct SigAstAndDedent {
		const SigAst sig;
		const size_t dedents;
	};

	const SigAstAndMaybeDedent parseSigAfterNameAndSpace(Lexer* lexer, const Pos start, const Sym name) {
		const TypeAst returnType = parseType(lexer);
		const ParamsAndMaybeDedent params = parseParams(lexer);
		const SigAst sigAst = SigAst{range(lexer, start), name, returnType, params.params};
		return SigAstAndMaybeDedent{sigAst, params.dedents};
	}

	const SigAstAndDedent parseSig(Lexer* lexer) {
		const Pos start = curPos(lexer);
		const Sym sigName = takeName(lexer);
		take(lexer, ' ');
		const SigAstAndMaybeDedent s = parseSigAfterNameAndSpace(lexer, start, sigName);
		const size_t dedents = has(s.dedents) ? force(s.dedents) : takeNewlineOrDedentAmount(lexer);
		return SigAstAndDedent{s.sig, dedents};
	}

	const Arr<const ImportAst> parseImports(Lexer* lexer) {
		ArrBuilder<const ImportAst> res {};
		do {
			add<const ImportAst>(lexer->arena, &res, parseSingleImport(lexer));
		} while (tryTake(lexer, ' '));
		take(lexer, '\n');
		return finishArr(&res);
	}

	const Arr<const SigAst> parseIndentedSigs(Lexer* lexer) {
		ArrBuilder<const SigAst> res {};
		for (;;) {
			const SigAstAndDedent sd = parseSig(lexer);
			add<const SigAst>(lexer->arena, &res, sd.sig);
			if (sd.dedents != 0) {
				// We started at in indent level of only 1, so can't go down more than 1.
				assert(sd.dedents == 1);
				return finishArr(&res);
			}
		}
	}

	enum class SpaceOrNewlineOrIndent {
		space,
		newline,
		indent,
	};
	enum class NonFunKeyword {
		alias,
		builtin,
		builtinSpec,
		record,
		spec,
		_union,
	};

	struct NonFunKeywordAndIndent {
		const NonFunKeyword keyword;
		const SpaceOrNewlineOrIndent after;
	};

	SpaceOrNewlineOrIndent spaceOrNewlineOrIndentFromNewlineOrIndent(const NewlineOrIndent ni) {
		switch (ni) {
			case NewlineOrIndent::newline:
				return SpaceOrNewlineOrIndent::newline;
			case NewlineOrIndent::indent:
				return SpaceOrNewlineOrIndent::indent;
			default:
				assert(0);
		}
	}

	const Opt<const NonFunKeywordAndIndent> tryTake(
		Lexer* lexer,
		const CStr kwSpace,
		const CStr kwNl,
		const NonFunKeyword keyword
	) {
		if (tryTake(lexer, kwSpace))
			return some<const NonFunKeywordAndIndent>(NonFunKeywordAndIndent{keyword, SpaceOrNewlineOrIndent::space});
		else if (tryTake(lexer, kwNl)) {
			const SpaceOrNewlineOrIndent sni =
				spaceOrNewlineOrIndentFromNewlineOrIndent(tryTakeIndentAfterNewline(lexer));
			return some<const NonFunKeywordAndIndent>(NonFunKeywordAndIndent{keyword, sni});
		} else
			return none<const NonFunKeywordAndIndent>();
	}

	const Opt<const NonFunKeywordAndIndent> parseNonFunKeyword(Lexer* lexer) {
		switch (curChar(lexer)) {
			case 'a':
				return tryTake(lexer, "alias ", "alias\n", NonFunKeyword::alias);
			case 'b': {
				const Opt<const NonFunKeywordAndIndent> res =
					tryTake(lexer, "builtin ", "builtin\n", NonFunKeyword::builtin);
				return has(res)
					? res
					: tryTake(lexer, "builtin-spec ", "builtin-spec\n", NonFunKeyword::builtinSpec);
			}
			case 'r':
				return tryTake(lexer, "record ", "record\n", NonFunKeyword::record);
			case 's':
				return tryTake(lexer, "spec ", "spec\n", NonFunKeyword::spec);
			case 'u':
				return tryTake(lexer, "union ", "union\n", NonFunKeyword::_union);
			default:
				return none<const NonFunKeywordAndIndent>();
		}
	}

	const StructDeclAst::Body::Record parseFields(Lexer* lexer) {
		ArrBuilder<const StructDeclAst::Body::Record::Field> res {};
		Cell<const Opt<const ExplicitByValOrRef>> explicitByValOrRef = none<const ExplicitByValOrRef>();
		Cell<const Bool> isFirstLine = True;
		do {
			const Pos start = curPos(lexer);
			const Sym name = takeName(lexer);
			switch (name.value) {
				case shortSymAlphaLiteralValue("by-val"):
					if (!cellGet(&isFirstLine))
						todo<void>("by-val on later line");
					cellSet<const Opt<const ExplicitByValOrRef>>(
						&explicitByValOrRef,
						some<const ExplicitByValOrRef>(ExplicitByValOrRef::byVal));
					break;
				case shortSymAlphaLiteralValue("by-ref"):
					if (!cellGet(&isFirstLine))
						todo<void>("by-ref on later line");
					cellSet<const Opt<const ExplicitByValOrRef>>(
						&explicitByValOrRef,
						some<const ExplicitByValOrRef>(ExplicitByValOrRef::byRef));
					break;
				default: {
					take(lexer, ' ');
					const Bool isMutable = tryTake(lexer, "mut ");
					const TypeAst type = parseType(lexer);
					add<const StructDeclAst::Body::Record::Field>(
						lexer->arena,
						&res,
						StructDeclAst::Body::Record::Field{range(lexer, start), isMutable, name, type});
				}
			}
			cellSet<const Bool>(&isFirstLine, False);
		} while (takeNewlineOrSingleDedent(lexer) == NewlineOrDedent::newline);
		return StructDeclAst::Body::Record{cellGet(&explicitByValOrRef), finishArr(&res)};
	}

	const Arr<const TypeAst::InstStruct> parseUnionMembers(Lexer* lexer) {
		ArrBuilder<const TypeAst::InstStruct> res {};
		do {
			const Pos start = curPos(lexer);
			const Sym name = takeName(lexer);
			const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
			add<const TypeAst::InstStruct>(
				lexer->arena,
				&res,
				TypeAst::InstStruct{range(lexer, start), name, typeArgs});
		} while (takeNewlineOrSingleDedent(lexer) == NewlineOrDedent::newline);
		return finishArr(&res);
	}

	struct SpecUsesAndSigFlagsAndKwBody {
		const Arr<const SpecUseAst> specUses;
		const Bool noCtx;
		const Bool summon;
		const Bool unsafe;
		const Bool trusted;
		const Opt<const FunBodyAst> body; // 'builtin' or 'extern'
	};

	const SpecUsesAndSigFlagsAndKwBody emptySpecUsesAndSigFlagsAndKwBody() {
		return SpecUsesAndSigFlagsAndKwBody{
			emptyArr<const SpecUseAst>(),
			False,
			False,
			False,
			False,
			none<const FunBodyAst>()};
	}

	struct SpecUsesAndSigFlagsAndKwBodyBuilder {
		ArrBuilder<const SpecUseAst> specUses {};
		Cell<const Bool> noCtx { False };
		Cell<const Bool> summon { False };
		Cell<const Bool> unsafe { False };
		Cell<const Bool> trusted { False };
		Cell<const Bool> builtin { False };
		Cell<const Bool> _extern { False };
	};

	void addSpecOrName(Lexer* lexer, SpecUsesAndSigFlagsAndKwBodyBuilder* builder) {
		const Pos start = curPos(lexer);
		const SymAndIsReserved name = takeNameAllowReserved(lexer);
		if (name.isReserved) {
			const auto setIt = [](Cell<const Bool>* b) -> void {
				if (cellGet(b))
					todo<void>("duplicate");
				cellSet<const Bool>(b, True);
			};

			switch (name.sym.value) {
				case shortSymAlphaLiteralValue("noctx"):
					setIt(&builder->noCtx);
					break;
				case shortSymAlphaLiteralValue("summon"):
					setIt(&builder->summon);
					break;
				case shortSymAlphaLiteralValue("unsafe"):
					setIt(&builder->unsafe);
					break;
				case shortSymAlphaLiteralValue("trusted"):
					setIt(&builder->trusted);
					break;
				case shortSymAlphaLiteralValue("builtin"):
					setIt(&builder->builtin);
					break;
				case shortSymAlphaLiteralValue("extern"):
					setIt(&builder->_extern);
					break;
				default:
					return throwOnReservedName<void>(name.range, name.sym);
			}
		} else {
			const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
			add<const SpecUseAst>(
				lexer->arena,
				&builder->specUses,
				SpecUseAst{range(lexer, start), name.sym, typeArgs});
		}
	}

	const SpecUsesAndSigFlagsAndKwBody finishSpecs(SpecUsesAndSigFlagsAndKwBodyBuilder* builder) {
		if (cellGet(&builder->unsafe) && cellGet(&builder->trusted))
			todo<void>("'unsafe trusted' is redundant");
		if (cellGet(&builder->builtin) && cellGet(&builder->trusted))
			todo<void>("'builtin trusted' is silly as builtin fun has no body");
		if (cellGet(&builder->_extern) && cellGet(&builder->trusted))
			todo<void>("'extern trusted' is silly as extern fun has no body");
		if (cellGet(&builder->_extern)) {
			if (cellGet(&builder->noCtx))
				todo<void>("'noctx extern' is redundant");
			cellSet<const Bool>(&builder->noCtx, True);
		}

		Opt<const FunBodyAst> body = cellGet(&builder->builtin)
			? some<const FunBodyAst>(FunBodyAst{FunBodyAst::Builtin{}})
			: cellGet(&builder->_extern)
			? some<const FunBodyAst>(FunBodyAst{FunBodyAst::Extern{}})
			: none<const FunBodyAst>();
		return SpecUsesAndSigFlagsAndKwBody{
			finishArr(&builder->specUses),
			cellGet(&builder->noCtx),
			cellGet(&builder->summon),
			cellGet(&builder->unsafe),
			cellGet(&builder->trusted),
			body};
	}

	// TODO: handle 'noctx' and friends too! (share code with parseSpecUsesAndSigFlagsAndKwBody)
	const SpecUsesAndSigFlagsAndKwBody parseIndentedSpecUses(Lexer* lexer) {
		takeIndent(lexer);
		SpecUsesAndSigFlagsAndKwBodyBuilder builder {};
		do {
			addSpecOrName(lexer, &builder);
		} while (takeNewlineOrSingleDedent(lexer) == NewlineOrDedent::newline);
		return finishSpecs(&builder);
	}

	const SpecUsesAndSigFlagsAndKwBody parseSpecUsesAndSigFlagsAndKwBody(Lexer* lexer) {
		SpecUsesAndSigFlagsAndKwBodyBuilder builder {};
		while (tryTake(lexer, ' '))
			addSpecOrName(lexer, &builder);
		return finishSpecs(&builder);
	}

	// TODO:RENAME
	struct FunDeclStuff {
		const SpecUsesAndSigFlagsAndKwBody extra;
		const FunBodyAst body;
	};

	const FunDeclAst parseFun(
		Lexer* lexer,
		const Bool isPublic,
		const Pos start,
		const Sym name,
		const Arr<const TypeParamAst> typeParams
	) {
		const SigAstAndMaybeDedent sig = parseSigAfterNameAndSpace(lexer, start, name);
		const FunDeclStuff stuff = [&]() {
			if (has(sig.dedents)) {
				// Started at indent of 0
				assert(force(sig.dedents) == 0);
				const SpecUsesAndSigFlagsAndKwBody extra = tryTake(lexer, "spec")
					? parseIndentedSpecUses(lexer)
					: emptySpecUsesAndSigFlagsAndKwBody();
				const FunBodyAst body = optOr(extra.body, [&]() {
					take(lexer, "body");
					return FunBodyAst{parseFunExprBody(lexer)};
				});
				return FunDeclStuff{extra, body};
			} else {
				const SpecUsesAndSigFlagsAndKwBody extra = parseSpecUsesAndSigFlagsAndKwBody(lexer);
				const FunBodyAst body = optOr(extra.body, [&]() { return FunBodyAst{parseFunExprBody(lexer)}; });
				return FunDeclStuff{extra, body};
			}
		}();
		const SpecUsesAndSigFlagsAndKwBody extra = stuff.extra;
		return FunDeclAst{
			isPublic,
			typeParams,
			sig.sig,
			extra.specUses,
			extra.noCtx,
			extra.summon,
			extra.unsafe,
			extra.trusted,
			stuff.body};
	}

	void parseSpecOrStructOrFun(
		Lexer* lexer,
		const Bool isPublic,
		ArrBuilder<const SpecDeclAst>* specs,
		ArrBuilder<const StructAliasAst>* structAliases,
		ArrBuilder<const StructDeclAst>* structs,
		ArrBuilder<const FunDeclAst>* funs
	) {
		const Pos start = curPos(lexer);
		const Sym name = takeName(lexer);
		const Arr<const TypeParamAst> typeParams = parseTypeParams(lexer);
		take(lexer, ' ');

		const Opt<const NonFunKeywordAndIndent> opKwAndIndent = parseNonFunKeyword(lexer);
		if (has(opKwAndIndent)) {
			const NonFunKeywordAndIndent kwAndIndent = force(opKwAndIndent);
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
				if (!tookIndent)
					todo<void>("always indent alias");
				if (has(purity))
					todo<void>("alias shouldn't have purity");
				const TypeAst::InstStruct target = parseStructType(lexer);
				takeDedent(lexer);
				add<const StructAliasAst>(
					lexer->arena,
					structAliases,
					StructAliasAst{range(lexer, start), isPublic, name, typeParams, target});
			} else if (kw == NonFunKeyword::builtinSpec) {
				if (tookIndent)
					todo<void>("builtin-spec has no body");
				if (has(purity))
					todo<void>("spec shouldn't have purity");
				add<const SpecDeclAst>(
					lexer->arena,
					specs,
					SpecDeclAst{
						range(lexer, start),
						isPublic,
						name,
						typeParams,
						SpecBodyAst{SpecBodyAst::Builtin{}}});
			} else if (kw == NonFunKeyword::spec) {
				if (!tookIndent)
					todo<void>("always indent spec");
				if (has(purity))
					todo<void>("spec shouldn't have purity");
				const Arr<const SigAst> sigs = parseIndentedSigs(lexer);
				add<const SpecDeclAst>(
					lexer->arena,
					specs,
					SpecDeclAst{range(lexer, start), isPublic, name, typeParams, SpecBodyAst{sigs}});
			} else {
				using Body = StructDeclAst::Body;
				const Body body = [&]() {
					switch (kw) {
						case NonFunKeyword::alias:
							return unreachable<const Body>();
						case NonFunKeyword::builtin:
							if (tookIndent)
								todo<void>("shouldn't indent after builtin");
							return Body{Body::Builtin{}};
						case NonFunKeyword::record:
							return Body{tookIndent
								? parseFields(lexer)
								: Body::Record{
									none<const ExplicitByValOrRef>(),
									emptyArr<const Body::Record::Field>()}};
						case NonFunKeyword::_union:
							return tookIndent
								? Body{Body::Union{parseUnionMembers(lexer)}}
								: throwAtChar<const Body>(lexer, ParseDiag{ParseDiag::UnionCantBeEmpty{}});
						default:
							return unreachable<const Body>();
					}
				}();
				add<const StructDeclAst>(
					lexer->arena,
					structs,
					StructDeclAst{range(lexer, start), isPublic, name, typeParams, purity, body});
			}
		} else
			add<const FunDeclAst>(lexer->arena, funs, parseFun(lexer, isPublic, start, name, typeParams));
	}

	const FileAst parseFileMayThrow(Lexer* lexer) {
		skipBlankLines(lexer);
		const Arr<const ImportAst> imports = tryTake(lexer, "import ")
			? parseImports(lexer)
			: emptyArr<const ImportAst>();
		const Arr<const ImportAst> exports = tryTake(lexer, "export ")
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
			parseSpecOrStructOrFun(lexer, isPublic, &specs, &structAliases, &structs, &funs);
		}

		return FileAst{
			imports,
			exports,
			finishArr(&specs),
			finishArr(&structAliases),
			finishArr(&structs),
			finishArr(&funs)
		};
	}
}

const Result<const FileAst, const ParseDiagnostic> parseFile(
	Arena* astArena,
	AllSymbols* allSymbols,
	const NulTerminatedStr source
) {
	try {
		Lexer lexer = createLexer(astArena, allSymbols, source);
		return success<const FileAst, const ParseDiagnostic>(parseFileMayThrow(&lexer));
	} catch (ParseDiagnostic p) {
		return failure<const FileAst, const ParseDiagnostic>(p);
	}
}
