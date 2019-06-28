#pragma once

#include "./lexer.h"

const Arr<const TypeAst> tryParseTypeArgs(Lexer& lexer);
const Opt<const TypeAst> tryParseTypeArg(Lexer& lexer);
const TypeAst::InstStruct parseStructType(Lexer& lexer);
const TypeAst parseType(Lexer& lexer);
