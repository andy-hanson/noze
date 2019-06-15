#! /bin/sh
c++ -Werror -Wextra -Wall -ansi -pedantic -std=gnu++1z src/*.cpp src/backend/*.cpp src/concretize/*.cpp src/frontend/*.cpp
