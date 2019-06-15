#include "./parseType.h"

namespace {
	const TypeAst parseTypeWorker(Lexer& lexer, const bool isInner);

	const Arr<const TypeAst> tryParseTypeArgsWorker(Lexer& lexer, const bool isInner) {
		ArrBuilder<const TypeAst> res {};
		// Require '<>' if parsing type args inside of type args.
		if (!isInner || lexer.tryTake('<')) {
			while (true) {
				if (!isInner && !lexer.tryTake(' '))
					break;
				res.add(lexer.arena, parseTypeWorker(lexer, /*isInner*/ true));
				if (isInner && !lexer.tryTake(", "))
					break;
			}
			if (isInner)
				lexer.take('>');
		}
		return res.finish();
	}

	const TypeAst parseTypeWorker(Lexer& lexer, const bool isInner) {
		const Pos start = lexer.at();
		const bool isTypeParam = lexer.tryTake('?');
		const Str name = lexer.takeName();
		const Arr<const TypeAst> typeArgs = tryParseTypeArgsWorker(lexer, isInner);
		if (isTypeParam && !isEmpty(typeArgs))
			return lexer.throwAtChar<const TypeAst>(ParseDiag{ParseDiag::TypeParamCantHaveTypeArgs{}});
		const SourceRange range = lexer.range(start);
		return isTypeParam
			? TypeAst{TypeAst::TypeParam{range, name}}
			: TypeAst{TypeAst::InstStruct{range, name, typeArgs}};
	}
}

const Arr<const TypeAst> tryParseTypeArgs(Lexer& lexer) {
	return tryParseTypeArgsWorker(lexer, /*isInner*/ true);
}

const Opt<const TypeAst> tryParseTypeArg(Lexer& lexer) {
	if (lexer.tryTake('<')) {
		const TypeAst res = parseTypeWorker(lexer, /*isInner*/ true);
		lexer.take('>');
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
	return parseTypeWorker(lexer, true);
}
