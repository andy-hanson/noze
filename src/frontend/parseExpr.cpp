#include "./parseExpr.h"

#include "../util/arrUtil.h"
#include "./parseType.h" // tryParseTypeArgs

namespace {
	const ExprAst* alloc(Lexer* lexer, const ExprAst e) {
		return nu<const ExprAst>{}(lexer->arena, e);
	}

	struct ArgCtx {
		// Allow things like 'match', 'when', '\' that continue into an indented block.
		const Bool allowBlock;
		// In `a b: c d e`, we parse `a b (c d e) and not `(a b c) d e`, since `: turns on `allowCall`.
		const Bool allowCall;
	};

	struct ExprAndDedent {
		const ExprAst expr;
		const size_t dedents;
	};

	// dedent=none means we didn't see a newline.
	// dedent=0 means a newline was parsed and is on the same indent level.
	struct ExprAndMaybeDedent {
		const ExprAst expr;
		const Opt<const size_t> dedents;
	};

	struct ArgsAndMaybeDedent {
		const Arr<const ExprAst> args;
		const Opt<const size_t> dedent;
	};

	const ExprAndMaybeDedent noDedent(const ExprAst e) {
		return ExprAndMaybeDedent{e, none<const size_t>()};
	}

	const ExprAndMaybeDedent parseExprArg(Lexer* lexer, const ArgCtx ctx);
	const ExprAst parseExprNoBlock(Lexer* lexer);
	const ExprAndDedent parseExprNoLet(Lexer* lexer);
	const ExprAndDedent parseStatementsAndDedent(Lexer* lexer);

	ArgsAndMaybeDedent parseArgs(Lexer* lexer, const ArgCtx ctx) {
		if (!tryTake(lexer, ' '))
			return ArgsAndMaybeDedent{emptyArr<const ExprAst>(), none<const size_t>()};
		else {
			auto dedents = Cell<const Opt<const size_t>>{none<const size_t>()};
			ArrBuilder<const ExprAst> args {};
			do {
				const ExprAndMaybeDedent ad = parseExprArg(lexer, ctx);
				add<const ExprAst>(lexer->arena, &args, ad.expr);
				cellSet<const Opt<const size_t>>(&dedents, ad.dedents);
			} while (!has(cellGet(&dedents)) && tryTake(lexer, ", "));
			return ArgsAndMaybeDedent{finishArr(&args), cellGet(&dedents)};
		}
	}

	const ExprAndDedent parseLetOrThen(Lexer* lexer, const Pos start, const NameAndRange name, const Bool isArrow) {
		const ExprAndDedent initAndDedent = parseExprNoLet(lexer);
		if (initAndDedent.dedents != 0) {
			printf("????? %zd\n", initAndDedent.dedents);
			return throwDiag<const ExprAndDedent>(range(lexer, start), ParseDiag{ParseDiag::LetMustHaveThen{}});
		} else {
			const ExprAst* init = alloc(lexer, initAndDedent.expr);
			const ExprAndDedent thenAndDedent = parseStatementsAndDedent(lexer);
			const ExprAst* then = alloc(lexer, thenAndDedent.expr);
			const ExprAstKind exprKind = isArrow
				? ExprAstKind{ThenAst{LambdaAst::Param{name.range, name.name}, init, then}}
				: ExprAstKind{LetAst{name, init, then}};
			// Since we don't always expect a dedent here,
			// the dedent isn't *extra*, so increment to get the correct number of dedents.
			return ExprAndDedent{ExprAst{range(lexer, start), exprKind}, thenAndDedent.dedents + 1};
		}
	}

	const ExprAndMaybeDedent parseCallOrMessage(Lexer* lexer, const ExprAst target, const Bool allowBlock) {
		const Pos start = curPos(lexer);
		if (tryTake(lexer, '.')) {
			const Sym funName = takeName(lexer);
			const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
			const CallAst call = CallAst{funName, typeArgs, arrLiteral<const ExprAst>(lexer->arena, { target })};
			return noDedent(ExprAst{range(lexer, start), ExprAstKind{call}});
		} else {
			const Bool isMessage = tryTake(lexer, '!');
			const Sym funName = takeName(lexer);
			const Bool colon = tryTake(lexer, ':');
			const Arr<const TypeAst> typeArgs = isMessage ? emptyArr<const TypeAst>() : tryParseTypeArgs(lexer);
			const ArgsAndMaybeDedent args = parseArgs(lexer, ArgCtx{allowBlock, /*allowcall*/ colon});
			const ExprAstKind exprKind = isMessage
				? ExprAstKind{MessageSendAst{alloc(lexer, target), funName, args.args}}
				: ExprAstKind{CallAst{funName, typeArgs, prepend<const ExprAst>(lexer->arena, target, args.args)}};
			return ExprAndMaybeDedent{ExprAst{range(lexer, start), exprKind}, args.dedent};
		}
	}

	const ExprAndMaybeDedent parseCallsAndRecordFieldSets(
		Lexer* lexer,
		const Pos start,
		const ExprAndMaybeDedent ed,
		const Bool allowBlock
	) {
		if (has(ed.dedents))
			return ed;
		else if (tryTake(lexer, " := ")) {
			const ExprAst expr = ed.expr;
			if (!expr.kind.isCall())
				todo<void>("non-struct-field-access to left of ':='");
			const CallAst call = expr.kind.asCall();
			if (!isEmpty(call.typeArgs))
				todo<void>("RecordFieldSet should not have type args");
			if (size(call.args) != 1)
				todo<void>("RecordFieldSet should have exactly 1 arg");
			const ExprAst* target = alloc(lexer, only(call.args));
			const ExprAndMaybeDedent value = parseExprArg(lexer, ArgCtx{allowBlock, /*allowCall*/ True});
			const RecordFieldSetAst sfs = RecordFieldSetAst{target, call.funName, alloc(lexer, value.expr)};
			return ExprAndMaybeDedent{ExprAst{range(lexer, start), ExprAstKind{sfs}}, value.dedents};
		} else if (tryTake(lexer, ' '))
			return parseCallsAndRecordFieldSets(
				lexer,
				start,
				parseCallOrMessage(lexer, ed.expr, allowBlock), allowBlock);
		else
			return ed;
	}

	template <typename Cb>
	const Bool someInOwnBody(const ExprAst body, Cb cb) {
		// Since this is only used checking for 'it' in a braced lambda, any multi-line ast is unreachable
		if (cb(body))
			return True;

		auto recur = [&](const ExprAst sub) {
			return someInOwnBody<Cb>(sub, cb);
		};

		return body.kind.match(
			[&](const CallAst e) {
				return exists(e.args, recur);
			},
			[](const CondAst) {
				return unreachable<const Bool>();
			},
			[&](const CreateArrAst e) {
				return exists(e.args, recur);
			},
			[&](const CreateRecordAst e) {
				return exists(e.args, recur);
			},
			[](const FunAsLambdaAst) {
				return False;
			},
			[](const IdentifierAst) {
				return False;
			},
			[](const LambdaAst) {
				return False;
			},
			[](const LetAst) {
				return unreachable<const Bool>();
			},
			[](const LiteralAst) {
				return False;
			},
			[](const MatchAst) {
				return unreachable<const Bool>();
			},
			[&](const MessageSendAst e) {
				return _or(recur(*e.target), exists(e.args, recur));
			},
			[](const NewActorAst) {
				return unreachable<const Bool>();
			},
			[](const SeqAst) {
				return unreachable<const Bool>();
			},
			[&](const RecordFieldSetAst e) {
				return _or(recur(*e.target), recur(*e.value));
			},
			[](const ThenAst) {
				return unreachable<const Bool>();
			});
	}

	const Bool bodyUsesIt(const ExprAst body) {
		return someInOwnBody(body, [](const ExprAst it) {
			return it.kind.isIdentifier() && symEq(it.kind.asIdentifier().name, shortSymAlphaLiteral("it"));
		});
	}

	const ExprAst tryParseDots(Lexer* lexer, const ExprAst initial) {
		const Pos start = curPos(lexer);
		if (tryTake(lexer, '.')) {
			const Sym name = takeName(lexer);
			const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
			const CallAst call = CallAst{
				name,
				typeArgs,
				arrLiteral<const ExprAst>(lexer->arena, { initial })};
			const ExprAst expr = ExprAst{range(lexer, start), ExprAstKind{call}};
			return tryParseDots(lexer, expr);
		} else
			return initial;
	}

	const ExprAndMaybeDedent parseMatch(Lexer* lexer, const Pos start) {
		take(lexer, ' ');
		const ExprAst* matched = alloc(lexer, parseExprNoBlock(lexer));
		takeIndent(lexer);

		ArrBuilder<const MatchAst::CaseAst> cases {};
		const size_t matchDedents = [&]() {
			for (;;) {
				const Pos startCase = curPos(lexer);
				const Sym structName = takeName(lexer);
				const Opt<const NameAndRange> localName = tryTakeIndent(lexer)
					? none<const NameAndRange>()
					: [&]() {
						take(lexer, ' ');
						const NameAndRange local = takeNameAndRange(lexer);
						takeIndent(lexer);
						return some<const NameAndRange>(local);
					}();
				const ExprAndDedent ed = parseStatementsAndDedent(lexer);
				add<const MatchAst::CaseAst>(
					lexer->arena,
					&cases,
					MatchAst::CaseAst{range(lexer, startCase), structName, localName, alloc(lexer, ed.expr)});
				if (ed.dedents != 0)
					return ed.dedents - 1;
			}
		}();
		const MatchAst match = MatchAst{matched, finishArr(&cases)};
		return ExprAndMaybeDedent{
			ExprAst{range(lexer, start), ExprAstKind{match}},
			some<const size_t>(matchDedents)};
	}

	const ExprAndMaybeDedent parseWhenLoop(Lexer* lexer, const Pos start) {
		if (tryTakeElseIndent(lexer)) {
			const ExprAndDedent elseAndDedent = parseStatementsAndDedent(lexer);
			if (elseAndDedent.dedents == 0)
				todo<void>("can't have any case after 'else'");
			return ExprAndMaybeDedent{elseAndDedent.expr, some<const size_t>(elseAndDedent.dedents - 1)};
		} else {
			const ExprAst condition = parseExprNoBlock(lexer);
			takeIndent(lexer);
			const ExprAndDedent thenAndDedent = parseStatementsAndDedent(lexer);
			if (thenAndDedent.dedents != 0)
				return throwAtChar<const ExprAndMaybeDedent>(lexer, ParseDiag{ParseDiag::WhenMustHaveElse{}});
			const ExprAndMaybeDedent elseAndDedent = parseWhenLoop(lexer, start);
			const CondAst cond = CondAst{
				alloc(lexer, condition),
				alloc(lexer, thenAndDedent.expr),
				alloc(lexer, elseAndDedent.expr)};
			return ExprAndMaybeDedent{
				ExprAst{range(lexer, start), ExprAstKind{cond}},
				elseAndDedent.dedents};
		}
	}

	const ExprAndMaybeDedent parseWhen(Lexer* lexer, const Pos start) {
		takeIndent(lexer);
		return parseWhenLoop(lexer, start);
	}

	const ExprAndMaybeDedent parseActor(Lexer* lexer, const Pos start) {
		const Arr<const NewActorAst::Field> fields = [&]() {
			take(lexer, '(');
			if (tryTake(lexer, ')'))
				return emptyArr<const NewActorAst::Field>();
			else {
				ArrBuilder<const NewActorAst::Field> res {};
				do {
					const Bool isMutable = tryTake(lexer, "mut ");
					const Sym name = takeName(lexer);
					if (!tryTake(lexer, " = "))
						todo<void>("parseNew");
					const ExprAst init = parseExprNoBlock(lexer);
					add<const NewActorAst::Field>(
						lexer->arena,
						&res,
						NewActorAst::Field{isMutable, name, alloc(lexer, init)});
				} while (tryTake(lexer, ", "));
				take(lexer, ')');
				return finishArr(&res);
			}
		}();

		takeIndent(lexer);
		ArrBuilder<const NewActorAst::MessageImpl> messages {};
		const size_t extraDedents = [&]() {
			for (;;) {
				const Sym messageName = takeName(lexer);
				take(lexer, '(');
				const Arr<const NameAndRange> paramNames = [&]() {
					if (tryTake(lexer, ')'))
						return emptyArr<const NameAndRange>();
					else {
						ArrBuilder<const NameAndRange> res {};
						add<const NameAndRange>(lexer->arena, &res, takeNameAndRange(lexer));
						while (tryTake(lexer, ", "))
							add<const NameAndRange>(lexer->arena, &res, takeNameAndRange(lexer));
						take(lexer, ')');
						return finishArr(&res);
					}
				}();
				takeIndent(lexer);
				const ExprAndDedent bodyAndDedent = parseStatementsAndDedent(lexer);
				add<const NewActorAst::MessageImpl>(
					lexer->arena,
					&messages,
					NewActorAst::MessageImpl{messageName, paramNames, alloc(lexer, bodyAndDedent.expr)});
				if (bodyAndDedent.dedents != 0)
					return bodyAndDedent.dedents - 1;
			}
		}();

		const NewActorAst newActor = NewActorAst{fields, finishArr(&messages)};
		return ExprAndMaybeDedent{
			ExprAst{range(lexer, start), ExprAstKind{newActor}},
			some<const size_t>(extraDedents)};
	}

	const ExprAndMaybeDedent parseLambda(Lexer* lexer, const Pos start) {
		ArrBuilder<const LambdaAst::Param> parameters {};
		Cell<const Bool> isFirst { True };
		while (!tryTakeIndent(lexer)) {
			if (cellGet(&isFirst))
				cellSet<const Bool>(&isFirst, False);
			else
				take(lexer, ' ');
			const NameAndRange nr = takeNameAndRange(lexer);
			add<const LambdaAst::Param>(lexer->arena, &parameters, LambdaAst::Param{nr.range, nr.name});
		}
		const ExprAndDedent bodyAndDedent = parseStatementsAndDedent(lexer);
		const LambdaAst lambda = LambdaAst{finishArr(&parameters), alloc(lexer, bodyAndDedent.expr)};
		return ExprAndMaybeDedent{
			ExprAst{range(lexer, start), ExprAstKind{lambda}},
			some<const size_t>(bodyAndDedent.dedents)};
	}

	const ExprAndMaybeDedent parseExprBeforeCall(
		Lexer* lexer,
		const Pos start,
		const ExpressionToken et,
		const ArgCtx ctx
	) {
		auto getRange = [&]() { return range(lexer, start); };
		auto checkBlockAllowed = [&]() {
			if (!ctx.allowBlock)
				throwAtChar<const ExprAndMaybeDedent>(lexer, ParseDiag{ParseDiag::MatchWhenNewMayNotAppearInsideArg{}});
		};

		using Kind = ExpressionToken::Kind;
		switch (et.kind) {
			case Kind::ampersand: {
				const Sym funName = takeName(lexer);
				const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
				return noDedent(ExprAst{getRange(), ExprAstKind{FunAsLambdaAst{funName, typeArgs}}});
			}
			case Kind::lambda:
				checkBlockAllowed();
				return parseLambda(lexer, start);
			case Kind::lbrace: {
				const ExprAst* body = alloc(lexer, parseExprNoBlock(lexer));
				take(lexer, '}');
				const SourceRange range = getRange();
				const Arr<const LambdaAst::Param> params = bodyUsesIt(*body)
					? arrLiteral<const LambdaAst::Param>(
						lexer->arena,
						{ LambdaAst::Param{range, shortSymAlphaLiteral("it")} })
					: emptyArr<const LambdaAst::Param>();
				const ExprAst expr = ExprAst{range, ExprAstKind{LambdaAst{params, body}}};
				return noDedent(tryParseDots(lexer, expr));
			}
			case Kind::literal: {
				const LiteralAst literal = et.asLiteral();
				const ExprAst expr = ExprAst{getRange(), ExprAstKind{literal}};
				return noDedent(tryParseDots(lexer, expr));
			}
			case Kind::lparen: {
				const ExprAst expr = parseExprNoBlock(lexer);
				take(lexer, ')');
				return noDedent(tryParseDots(lexer, expr));
			}
			case Kind::match:
				checkBlockAllowed();
				return parseMatch(lexer, start);
			case Kind::nameAndRange: {
				const Sym name = et.asNameAndRange().name;
				const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
				const Bool tookColon = tryTake(lexer, ':');
				if (tookColon) {
					// Prefix call `foo: bar, baz`
					const ArgsAndMaybeDedent ad = parseArgs(lexer, ctx);
					const CallAst call = CallAst{name, typeArgs, ad.args};
					return ExprAndMaybeDedent{ExprAst{getRange(), ExprAstKind{call}}, ad.dedent};
				} else if (!isEmpty(typeArgs))
					return noDedent(
						ExprAst{getRange(), ExprAstKind{CallAst{name, typeArgs, emptyArr<const ExprAst>()}}});
				else {
					const ExprAst expr = ExprAst{getRange(), ExprAstKind{IdentifierAst{name}}};
					return noDedent(tryParseDots(lexer, expr));
				}
			}
			case Kind::newActor:
				checkBlockAllowed();
				return parseActor(lexer, start);
			case Kind::_new:
			case Kind::newArr:{
				const Opt<const TypeAst> type = tryParseTypeArg(lexer);
				const ArgsAndMaybeDedent ad = parseArgs(lexer, ctx);
				const ExprAstKind ast = et.kind == Kind::_new
					? ExprAstKind{CreateRecordAst{type, ad.args}}
					: ExprAstKind{CreateArrAst{type, ad.args}};
				return ExprAndMaybeDedent{ExprAst{getRange(), ast}, ad.dedent};
			}
			case Kind::when:
				checkBlockAllowed();
				return parseWhen(lexer, start);
			default:
				return unreachable<const ExprAndMaybeDedent>();
		}
	}

	// Note: if dedents is none, may not be at a newline.
	const ExprAndMaybeDedent parseExprWorker(
		Lexer* lexer,
		const Pos start,
		const ExpressionToken et,
		const ArgCtx ctx
	) {
		const ExprAndMaybeDedent ed = parseExprBeforeCall(lexer, start, et, ctx);
		return ctx.allowCall ? parseCallsAndRecordFieldSets(lexer, start, ed, ctx.allowBlock) : ed;
	}

	// This eats an expression, but does not eat any newlines.
	const ExprAst parseExprNoBlock(Lexer* lexer) {
		const Pos start = curPos(lexer);
		const ExpressionToken et = takeExpressionToken(lexer);
		const ExprAndMaybeDedent ed = parseExprWorker(
			lexer,
			start,
			et,
			ArgCtx{/*allowBlock*/ False, /*allowCall*/ True});
		// We set allowBlock to false, so not allowed to take newlines, so can't have dedents.
		assert(!has(ed.dedents));
		return ed.expr;
	}

	const ExprAndMaybeDedent parseExprArg(Lexer* lexer, const ArgCtx ctx) {
		const Pos start = curPos(lexer);
		const ExpressionToken et = takeExpressionToken(lexer);
		return parseExprWorker(lexer, start, et, ctx);
	}

	const ExprAndDedent parseExprNoLet(Lexer* lexer, const Pos start, const ExpressionToken et) {
		const ExprAndMaybeDedent e = parseExprWorker(lexer, start, et, ArgCtx{/*allowBlock*/ True, /*allowCall*/ True});
		const size_t dedents = has(e.dedents)
			? force(e.dedents)
			: takeNewlineOrDedentAmount(lexer);
		return ExprAndDedent{e.expr, dedents};
	}

	const ExprAndDedent parseExprNoLet(Lexer* lexer) {
		const Pos start = curPos(lexer);
		return parseExprNoLet(lexer, start, takeExpressionToken(lexer));
	}

	const ExprAndDedent parseSingleStatementLine(Lexer* lexer) {
		const Pos start = curPos(lexer);
		const ExpressionToken et = takeExpressionToken(lexer);
		if (et.isNameAndRange()) {
			const NameAndRange nr = et.asNameAndRange();
			const Opt<const Bool> isThen =
				tryTake(lexer, " = ") ? some<const Bool>(False)
				: tryTake(lexer, " <- ") ? some<const Bool>(True)
				: none<const Bool>();
			if (has(isThen))
				return parseLetOrThen(lexer, start, nr, force(isThen));
		}
		return parseExprNoLet(lexer, start, et);
	}

	// Return value is number of dedents - 1; the number of *extra* dedents
	const ExprAndDedent parseStatementsAndDedent(Lexer* lexer) {
		const Pos start = curPos(lexer);
		const ExprAndDedent ed = parseSingleStatementLine(lexer);
		auto expr = Cell<const ExprAst>(ed.expr);
		auto dedents = Cell<const size_t>(ed.dedents);
		while (cellGet(&dedents) == 0) {
			const ExprAndDedent ed2 = parseSingleStatementLine(lexer);
			const SeqAst seq = SeqAst{alloc(lexer, cellGet(&expr)), alloc(lexer, ed2.expr)};
			cellSet<const ExprAst>(&expr, ExprAst{range(lexer, start), ExprAstKind{seq}});
			cellSet<const size_t>(&dedents, ed2.dedents);
		}
		return ExprAndDedent{cellGet(&expr), cellGet(&dedents) - 1};
	}
}

const ExprAst parseFunExprBody(Lexer* lexer) {
	takeIndent(lexer);
	const ExprAndDedent ed = parseStatementsAndDedent(lexer);
	assert(ed.dedents == 0);
	return ed.expr;
}
