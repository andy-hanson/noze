cmake_minimum_required(VERSION 3.9)
project(test2)

set(CMAKE_CXX_STANDARD 17)

add_executable(test2
	src/frontend/check.cpp
	src/frontend/checkCall.cpp
	src/frontend/checkExpr.cpp
	src/frontend/compile.cpp
	src/frontend/inferringType.cpp
	src/frontend/lexer.cpp
	src/frontend/parse.cpp
	src/frontend/parseExpr.cpp
	src/frontend/parseType.cpp
	src/frontend/readOnlyStorage.cpp
	src/frontend/typeFromAst.cpp
	src/concreteModel.cpp
	src/main.cpp
	src/model.cpp
	src/Path.cpp
	src/util.cpp)
target_compile_options(test2 PRIVATE -Wall -Wextra -Werror -ansi -pedantic)

