#! /bin/sh
# Not -pedantic because I want to use c99 designated initializers
# TODO: -stc=c++20 will support that
c++ -Werror -Wextra -Wall -ansi -std=c++17 \
	-g \
	-Ilibfirm/include -Ilibfirm/build/gen/include/libfirm \
	`find src -name *.cpp` libfirm/build/debug/libfirm.so \
	-o noze
