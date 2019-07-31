#! /bin/sh
c++ -Werror -Wextra -Wall -ansi -std=c++17 \
	-Ofast \
	-Ilibfirm/include -Ilibfirm/build/gen/include/libfirm \
	`find src -name *.cpp` libfirm/build/debug/libfirm.so \
	-o noze-optimized
