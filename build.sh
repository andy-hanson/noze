#! /bin/sh
# TODO: get debugging symbols here
c++ -Werror -Wextra -Wall -ansi -pedantic -std=c++17 src/*.cpp src/backend/*.cpp src/concretize/*.cpp src/frontend/*.cpp

# Cmake build instructions:
# mkdir build
# cd build
# cmake .. -DCMAKE_BUILD_TYPE=Debug
# make
