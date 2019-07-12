#! /bin/sh
clang++-6.0 -Werror -Wextra -Wall -Wno-c99-extensions -ansi -pedantic -std=c++17 -g -Ilibfirm/include -Ilibfirm/build/gen/include/libfirm `find src -name *.cpp` libfirm/build/debug/libfirm.so -o noze
