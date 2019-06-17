# To use this:
# mkdir build
# cd build
# cmake .. -DCMAKE_CXX_COMPILER=clang++-6.0 -DCMAKE_BUILD_TYPE=Debug
# make

cmake_minimum_required(VERSION 3.9)
project(noze)

set(CMAKE_CXX_STANDARD 17)

add_executable(noze
	src/backend/writeToC.cpp
	src/concretize/allConstants.cpp
	src/concretize/builtinInfo.cpp
	src/concretize/concretizeBuiltin.cpp
	src/concretize/concretize.cpp
	src/concretize/concretizeCtx.cpp
	src/concretize/concretizeExpr.cpp
	src/concretize/concretizeUtil.cpp
	src/concretize/getReferencedOnly.cpp
	src/concretize/mangleName.cpp
	src/concretize/writer.cpp
	src/frontend/check.cpp
	src/frontend/checkCall.cpp
	src/frontend/checkExpr.cpp
	src/frontend/frontendCompile.cpp
	src/frontend/inferringType.cpp
	src/frontend/lexer.cpp
	src/frontend/parse.cpp
	src/frontend/parseExpr.cpp
	src/frontend/parseType.cpp
	src/frontend/showDiag.cpp
	src/frontend/typeFromAst.cpp
	src/concreteModel.cpp
	src/compiler.cpp
	src/main.cpp
	src/model.cpp
	src/util.cpp
	src/util/arena.cpp
	src/util/io.cpp
	src/util/lineAndColumnGetter.cpp
	src/util/path.cpp)
target_compile_options(noze PRIVATE -Wall -Wextra -Werror -ansi -pedantic)

