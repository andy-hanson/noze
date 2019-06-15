#include "./parseExpr.h"

#include "../util/arrUtil.h"
#include "./parseType.h" // tryParseTypeArgs

namespace {
	using ExpressionToken = Lexer::ExpressionToken;

	const ExprAst* alloc(Lexer& lexer, const ExprAst e) {
		return lexer.arena.nu<const ExprAst>()(e);
	}

	struct ArgCtx {
		// Allow things like 'match', 'when', '\' that continue into an indented block.
		const bool allowBlock;
		// In `a b: c d e`, we parse `a b (c d e) and not `(a b c) d e`, since `: turns on `allowCall`.
		const bool allowCall;
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

	const ExprAndMaybeDedent parseExprArg(Lexer& lexer, const ArgCtx ctx);
	const ExprAst parseExprNoBlock(Lexer& lexer);
	const ExprAndDedent parseExprNoLet(Lexer& lexer);
	const ExprAndDedent parseStatementsAndDedent(Lexer& lexer);

	ArgsAndMaybeDedent parseArgs(Lexer& lexer, const ArgCtx ctx) {
		if (!lexer.tryTake(' '))
			return ArgsAndMaybeDedent{emptyArr<const ExprAst>(), none<const size_t>()};
		else {
			auto dedents = Cell<const Opt<const size_t>>{none<const size_t>()};
			ArrBuilder<const ExprAst> args {};
			do {
				const ExprAndMaybeDedent ad = parseExprArg(lexer, ctx);
				args.add(lexer.arena, ad.expr);
				dedents.set(ad.dedents);
			} while (!dedents.get().has() && lexer.tryTake(", "));
			return ArgsAndMaybeDedent{args.finish(), dedents.get()};
		}
	}

	const ExprAndDedent parseLetOrThen(Lexer& lexer, const Pos start, const NameAndRange name, const bool isArrow) {
		const ExprAndDedent initAndDedent = parseExprNoLet(lexer);
		if (initAndDedent.dedents != 0)
			todo<void>("Diagnostic: block can't end in `x = ...`, something must come after");
		const ExprAst* init = alloc(lexer, initAndDedent.expr);
		const ExprAndDedent thenAndDedent = parseStatementsAndDedent(lexer);
		const ExprAst* then = alloc(lexer, thenAndDedent.expr);
		const ExprAstKind exprKind = isArrow
			? ExprAstKind{ThenAst{LambdaAst::Param{name.range, name.name}, init, then}}
			: ExprAstKind{LetAst{name.name, init, then}};
		// Since we don't always expect a dedent here, the dedent isn't *extra*, so increment to get the correct number of dedents.
		return ExprAndDedent{ExprAst{lexer.range(start), exprKind}, thenAndDedent.dedents + 1};
	}

	const ExprAndMaybeDedent parseCallOrMessage(Lexer& lexer, const ExprAst target, const bool allowBlock) {
		const Pos start = lexer.at();
		if (lexer.tryTake('.')) {
			const Str funName = lexer.takeName();
			const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
			const CallAst call = CallAst{funName, typeArgs, arrLiteral<const ExprAst>(lexer.arena, target)};
			return noDedent(ExprAst{lexer.range(start), ExprAstKind{call}});
		} else {
			const bool isMessage = lexer.tryTake('!');
			const Str funName = lexer.takeName();
			const bool colon = lexer.tryTake(':');
			const Arr<const TypeAst> typeArgs = isMessage ? emptyArr<const TypeAst>() : tryParseTypeArgs(lexer);
			const ArgsAndMaybeDedent args = parseArgs(lexer, ArgCtx{allowBlock, /*allowcall*/ colon});
			const ExprAstKind exprKind = isMessage
				? ExprAstKind{MessageSendAst{alloc(lexer, target), funName, args.args}}
				: ExprAstKind{CallAst{funName, typeArgs, prepend<const ExprAst>(lexer.arena, target, args.args)}};
			return ExprAndMaybeDedent{ExprAst{lexer.range(start), exprKind}, args.dedent};
		}
	}

	const ExprAndMaybeDedent parseCalls(Lexer& lexer, const ExprAndMaybeDedent ed, const bool allowBlock) {
		return !ed.dedents.has() && lexer.tryTake(' ')
			? parseCalls(lexer, parseCallOrMessage(lexer, ed.expr, allowBlock), allowBlock)
			: ed;
	}

	template <typename Cb>
	bool someInOwnBody(const ExprAst body, Cb cb) {
		auto r = [&](const ExprAst sub) {
			return someInOwnBody<Cb>(sub, cb);
		};

		return body.kind.match(
			[&](const CallAst e) {
				return exists(e.args, r);
			},
			[](const CondAst) {
				return unreachable<bool>();
			},
			[&](const CreateArrAst e) {
				return exists(e.args, r);
			},
			[&](const CreateRecordAst e) {
				return exists(e.args, r);
			},
			[](const FunAsLambdaAst) {
				return false;
			},
			[](const IdentifierAst) {
				return false;
			},
			[](const LambdaAst) {
				return false;
			},
			[](const LetAst) {
				return unreachable<bool>();
			},
			[](const LiteralAst) {
				return false;
			},
			[](const MatchAst) {
				return unreachable<bool>();
			},
			[&](const MessageSendAst e) {
				return r(*e.target) || exists(e.args, r);
			},
			[](const NewActorAst) {
				return unreachable<bool>();
			},
			[](const SeqAst) {
				return unreachable<bool>();
			},
			[](const ThenAst) {
				return unreachable<bool>();
			});
	}

	bool bodyUsesIt(const ExprAst body) {
		return someInOwnBody(body, [](const ExprAst it) {
			return it.kind.isIdentifier() && strEqLiteral(it.kind.asIdentifier().name, "it");
		});
	}

	const ExprAst tryParseDots(Lexer& lexer, const ExprAst initial) {
		const Pos start = lexer.at();
		if (lexer.tryTake('.')) {
			const Str name = lexer.takeName();
			const CallAst call = CallAst{name, emptyArr<const TypeAst>(), arrLiteral<const ExprAst>(lexer.arena, initial)};
			const ExprAst expr = ExprAst{lexer.range(start), ExprAstKind{call}};
			return tryParseDots(lexer, expr);
		} else
			return initial;
	}

	const ExprAndMaybeDedent parseMatch(Lexer& lexer, const Pos start) {
		lexer.take(' ');
		const ExprAst* matched = alloc(lexer, parseExprNoBlock(lexer));
		lexer.takeIndent();

		ArrBuilder<const MatchAst::CaseAst> cases {};
		const size_t matchDedents = [&]() {
			while (true) {
				const Str structName = lexer.takeName();
				const Opt<const Str> localName = lexer.tryTakeIndent()
					? none<const Str>()
					: [&]() {
						lexer.take(' ');
						const Str localName = lexer.takeName();
						lexer.takeIndent();
						return some<const Str>(localName);
					}();
				const ExprAndDedent ed = parseStatementsAndDedent(lexer);
				cases.add(lexer.arena, MatchAst::CaseAst{structName, localName, alloc(lexer, ed.expr)});
				if (ed.dedents != 0)
					return ed.dedents - 1;
			}
		}();
		const MatchAst match = MatchAst{matched, cases.finish()};
		return ExprAndMaybeDedent{
			ExprAst{lexer.range(start), ExprAstKind{match}},
			some<const size_t>(matchDedents)};
	}

	const ExprAndMaybeDedent parseWhenLoop(Lexer& lexer, const Pos start) {
		if (lexer.tryTakeElseIndent()) {
			const ExprAndDedent elseAndDedent = parseStatementsAndDedent(lexer);
			if (elseAndDedent.dedents == 0)
				todo<void>("can't have any case after 'else'");
			return ExprAndMaybeDedent{elseAndDedent.expr, some<const size_t>(elseAndDedent.dedents - 1)};
		} else {
			const ExprAst condition = parseExprNoBlock(lexer);
			lexer.takeIndent();
			const ExprAndDedent thenAndDedent = parseStatementsAndDedent(lexer);
			if (thenAndDedent.dedents != 0)
				return lexer.throwAtChar<const ExprAndMaybeDedent>(ParseDiag{ParseDiag::WhenMustHaveElse{}});
			const ExprAndMaybeDedent elseAndDedent = parseWhenLoop(lexer, start);
			const CondAst cond = CondAst{alloc(lexer, condition), alloc(lexer, thenAndDedent.expr), alloc(lexer, elseAndDedent.expr)};
			return ExprAndMaybeDedent{
				ExprAst{lexer.range(start), ExprAstKind{cond}},
				elseAndDedent.dedents};
		}
	}

	const ExprAndMaybeDedent parseWhen(Lexer& lexer, const Pos start) {
		lexer.takeIndent();
		return parseWhenLoop(lexer, start);
	}

	const ExprAndMaybeDedent parseActor(Lexer& lexer, const Pos start) {
		const Arr<const NewActorAst::Field> fields = [&]() {
			lexer.take('(');
			if (lexer.tryTake(')'))
				return emptyArr<const NewActorAst::Field>();
			else {
				ArrBuilder<const NewActorAst::Field> res {};
				do {
					const Str name = lexer.takeName();
					if (!lexer.tryTake(" = "))
						todo<void>("parseNew");
					const ExprAst init = parseExprNoBlock(lexer);
					res.add(lexer.arena, NewActorAst::Field{name, alloc(lexer, init)});
				} while (lexer.tryTake(", "));
				lexer.take(')');
				return res.finish();
			}
		}();

		lexer.takeIndent();
		ArrBuilder<const NewActorAst::MessageImpl> messages {};
		const size_t extraDedents = [&]() {
			while (true) {
				const Str messageName = lexer.takeName();
				lexer.take('(');
				const Arr<const NameAndRange> paramNames = [&]() {
					if (lexer.tryTake(')'))
						return emptyArr<const NameAndRange>();
					else {
						ArrBuilder<const NameAndRange> res {};
						res.add(lexer.arena, lexer.takeNameAndRange());
						while (lexer.tryTake(", "))
							res.add(lexer.arena, lexer.takeNameAndRange());
						lexer.take(')');
						return res.finish();
					}
				}();
				lexer.takeIndent();
				const ExprAndDedent bodyAndDedent = parseStatementsAndDedent(lexer);
				messages.add(lexer.arena, NewActorAst::MessageImpl{messageName, paramNames, alloc(lexer, bodyAndDedent.expr)});
				if (bodyAndDedent.dedents != 0)
					return bodyAndDedent.dedents - 1;
			}
		}();

		const NewActorAst newActor = NewActorAst{fields, messages.finish()};
		return ExprAndMaybeDedent{
			ExprAst{lexer.range(start), ExprAstKind{newActor}},
			some<const size_t>(extraDedents)};
	}

	const ExprAndMaybeDedent parseLambda(Lexer& lexer, const Pos start) {
		ArrBuilder<const LambdaAst::Param> parameters {};
		bool isFirst = true;
		while (!lexer.tryTakeIndent()) {
			if (isFirst)
				isFirst = false;
			else
				lexer.take(' ');
			const NameAndRange nr = lexer.takeNameAndRange();
			parameters.add(lexer.arena, LambdaAst::Param{nr.range, nr.name});
		}
		const ExprAndDedent bodyAndDedent = parseStatementsAndDedent(lexer);
		const LambdaAst lambda = LambdaAst{parameters.finish(), alloc(lexer, bodyAndDedent.expr)};
		return ExprAndMaybeDedent{
			ExprAst{lexer.range(start), ExprAstKind{lambda}},
			some<const size_t>(bodyAndDedent.dedents)};
	}

	const ExprAndMaybeDedent parseExprBeforeCall(Lexer& lexer, const Pos start, const ExpressionToken et, const ArgCtx ctx) {
		auto getRange = [&]() { return lexer.range(start); };
		auto checkBlockAllowed = [&]() {
			if (!ctx.allowBlock)
				lexer.throwAtChar<const ExprAndMaybeDedent>(ParseDiag{ParseDiag::MatchWhenNewMayNotAppearInsideArg{}});
		};

		using Kind = ExpressionToken::Kind;
		switch (et.kind) {
			case Kind::actor:
				checkBlockAllowed();
				return parseActor(lexer, start);
			case Kind::ampersand: {
				const Str funName = lexer.takeName();
				const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
				return noDedent(ExprAst{getRange(), ExprAstKind{FunAsLambdaAst{funName, typeArgs}}});
			}
			case Kind::lambda:
				checkBlockAllowed();
				return parseLambda(lexer, start);
			case Kind::lbrace: {
				const ExprAst* body = alloc(lexer, parseExprNoBlock(lexer));
				lexer.take(')');
				const SourceRange range = getRange();
				const Arr<const LambdaAst::Param> params = bodyUsesIt(*body)
					? arrLiteral<const LambdaAst::Param>(lexer.arena, LambdaAst::Param{range, strLiteral("it")})
					: emptyArr<const LambdaAst::Param>();
				const ExprAst expr = ExprAst{range, ExprAstKind{LambdaAst{params, body}}};
				return noDedent(tryParseDots(lexer, expr));
			}
			case Kind::literal: {
				const Str literal = et.asLiteral();
				const ExprAst expr = ExprAst{getRange(), ExprAstKind{LiteralAst{literal}}};
				return noDedent(tryParseDots(lexer, expr));
			}
			case Kind::lparen: {
				const ExprAst expr = parseExprNoBlock(lexer);
				lexer.take(')');
				return noDedent(tryParseDots(lexer, expr));
			}
			case Kind::match:
				checkBlockAllowed();
				return parseMatch(lexer, start);
			case Kind::nameAndRange: {
				const Str name = et.asNameAndRange().name;
				const Arr<const TypeAst> typeArgs = tryParseTypeArgs(lexer);
				const bool tookColon = lexer.tryTake(':');
				if (tookColon) {
					// Prefix call `foo: bar, baz`
					const ArgsAndMaybeDedent ad = parseArgs(lexer, ctx);
					const CallAst call = CallAst{name, typeArgs, ad.args};
					return ExprAndMaybeDedent{ExprAst{getRange(), ExprAstKind{call}}, ad.dedent};
				} else if (!isEmpty(typeArgs))
					return noDedent(ExprAst{getRange(), ExprAstKind{CallAst{name, typeArgs, emptyArr<const ExprAst>()}}});
				else {
					const ExprAst expr = ExprAst{getRange(), ExprAstKind{IdentifierAst{name}}};
					return noDedent(tryParseDots(lexer, expr));
				}
			}
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

	const ExprAndMaybeDedent parseExprWorker(Lexer& lexer, const Pos start, const ExpressionToken et, const ArgCtx ctx) {
		const ExprAndMaybeDedent ed = parseExprBeforeCall(lexer, start, et, ctx);
		return ctx.allowCall ? parseCalls(lexer, ed, ctx.allowBlock) : ed;
	}

	const ExprAst parseExprNoBlock(Lexer& lexer) {
		const Pos start = lexer.at();
		const ExpressionToken et = lexer.takeExpressionToken();
		const ExprAndMaybeDedent ed = parseExprWorker(lexer, start, et, ArgCtx{/*allowBlock*/ false, /*allowCall*/ true});
		assert(!ed.dedents.has());
		return ed.expr;
	}

	const ExprAndMaybeDedent parseExprArg(Lexer& lexer, const ArgCtx ctx) {
		const Pos start = lexer.at();
		const ExpressionToken et = lexer.takeExpressionToken();
		return parseExprWorker(lexer, start, et, ctx);
	}

	const ExprAndDedent parseExprNoLet(Lexer& lexer, const Pos start, const ExpressionToken et) {
		const ExprAndMaybeDedent e = parseExprWorker(lexer, start, et, ArgCtx{/*allowBlock*/ true, /*allowCall*/ true});
		return ExprAndDedent{e.expr, e.dedents.has() ? e.dedents.force() : lexer.takeNewlineOrDedentAmount()};
	}

	const ExprAndDedent parseExprNoLet(Lexer& lexer) {
		const Pos start = lexer.at();
		const ExpressionToken et = lexer.takeExpressionToken();
		return parseExprNoLet(lexer, start, et);
	}

	const ExprAndDedent parseSingleStatementLine(Lexer& lexer) {
		const Pos start = lexer.at();
		const ExpressionToken et = lexer.takeExpressionToken();
		if (et.isNameAndRange()) {
			const NameAndRange nr = et.asNameAndRange();
			const Opt<const bool> isThen =
				lexer.tryTake(" = ") ? some<const bool>(false)
				: lexer.tryTake(" <- ") ? some<const bool>(true)
				: none<const bool>();
			if (isThen.has())
				return parseLetOrThen(lexer, start, nr, isThen.force());
		}
		return parseExprNoLet(lexer, start, et);
	}

	// Return value is number of dedents - 1; the number of *extra* dedents
	const ExprAndDedent parseStatementsAndDedent(Lexer& lexer) {
		const Pos start = lexer.at();
		const ExprAndDedent ed = parseSingleStatementLine(lexer);
		auto expr = Cell<const ExprAst>(ed.expr);
		auto dedents = Cell<const size_t>(ed.dedents);
		while (dedents.get() == 0) {
			const ExprAndDedent ed2 = parseSingleStatementLine(lexer);
			const SeqAst seq = SeqAst{alloc(lexer, expr.get()), alloc(lexer, ed2.expr)};
			expr.set(ExprAst{lexer.range(start), ExprAstKind{seq}});
			dedents.set(ed2.dedents);
		}
		return ExprAndDedent{expr.get(), dedents.get() - 1};
	}
}

const ExprAst parseFunExprBody(Lexer& lexer) {
	lexer.takeIndent();
	const ExprAndDedent ed = parseStatementsAndDedent(lexer);
	assert(ed.dedents == 0);
	return ed.expr;
}


