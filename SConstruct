# To build: `scons`
# To clean: `scons -c`

from os import environ

env = Environment()
env["CXX"] = "clang++-6.0"
# This ensures we get color output
env["ENV"]["TERM"] = environ["TERM"]

env.Program(
	"noze",
	Glob("src/*.cpp") + Glob("src/**/*.cpp"),
	LIBS=[File("libfirm/build/debug/libfirm.so")],
	CPPPATH=["libfirm/include", "libfirm/build/gen/include/libfirm"],
	CPPFLAGS=Split("-Werror -Wextra -Wall -ansi -pedantic -std=c++17 -g"))
