#! /bin/sh

cd libfirm
make
cd ..

mkdir -p bin

# Not -pedantic because I want to use c99 designated initializers
# TODO: -stc=c++20 will support that
# rpath is to ensure that libfirm can be found regardless of the cwd
c++ -Werror -Wextra -Wall -ansi -std=c++17 \
	-g \
	-Ilibfirm/include -Ilibfirm/build/gen/include/libfirm \
	`find src -name *.cpp` \
	-Llibfirm/build/debug -lfirm \
	'-Wl,-rpath,$ORIGIN/../libfirm/build/debug' \
	-o bin/noze
