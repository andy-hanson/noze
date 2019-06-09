#pragma once

#include "./ast.h"
#include "./lexer.h"

const ExprAst parseFunExprBody(Lexer& lexer);
