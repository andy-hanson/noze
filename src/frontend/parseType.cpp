#include "./parseType.h"

namespace {
	const TypeAst parseTypeWorker(Lexer& lexer, const Bool isInner);

	const Arr<const TypeAst> tryParseTypeArgsWorker(Lexer& lexer, const Bool isInner) {
		ArrBuilder<const TypeAst> res {};
		// Require '<>' if parsing type args inside of type args.
		if (!isInner || tryTake(lexer, '<')) {
			for (;;) {
				if (!isInner && !tryTake(lexer, ' '))
					break;
				add<const TypeAst>(lexer.arena, &res, parseTypeWorker(lexer, /*isInner*/ True));
				if (isInner && !tryTake(lexer, ", "))
					break;
			}
			if (isInner)
				take(lexer, '>');
		}
		return finishArr(&res);
	}

	const TypeAst parseTypeWorker(Lexer& lexer, const Bool isInner) {
		const Pos start = curPos(lexer);
		const Bool isTypeParam = tryTake(lexer, '?');
		const Sym name = takeName(lexer);
		const Arr<const TypeAst> typeArgs = tryParseTypeArgsWorker(lexer, isInner);
		if (isTypeParam && !isEmpty(typeArgs))
			return throwAtChar<const TypeAst>(lexer, ParseDiag{ParseDiag::TypeParamCantHaveTypeArgs{}});
		const SourceRange rng = range(lexer, start);
		return isTypeParam
			? TypeAst{TypeAst::TypeParam{rng, name}}
			: TypeAst{TypeAst::InstStruct{rng, name, typeArgs}};
	}
}

const Arr<const TypeAst> tryParseTypeArgs(Lexer& lexer) {
	return tryParseTypeArgsWorker(lexer, /*isInner*/ True);
}

const Opt<const TypeAst> tryParseTypeArg(Lexer& lexer) {
	if (tryTake(lexer, '<')) {
		const TypeAst res = parseTypeWorker(lexer, /*isInner*/ True);
		take(lexer, '>');
		return some<const TypeAst>(res);
	} else
		return none<const TypeAst>();
}

const TypeAst::InstStruct parseStructType(Lexer& lexer) {
	const TypeAst t = parseType(lexer);
	return t.match(
		[](const TypeAst::TypeParam) {
			return todo<const TypeAst::InstStruct>("must be a struct");
		},
		[](const TypeAst::InstStruct i) {
			return i;
		});
}

const TypeAst parseType(Lexer& lexer) {
	return parseTypeWorker(lexer, /*isInner*/ False);
}
