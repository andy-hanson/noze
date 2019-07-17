# To build: `scons`
# To clean: `scons -c`

from os import environ

env = Environment()
# This ensures we get color output
env["ENV"]["TERM"] = environ["TERM"]

env.Program(
	"noze",
	Glob("src/*.cpp") + Glob("src/**/*.cpp"),
	LIBS=[File("libfirm/build/debug/libfirm.so")],
	CPPPATH=["libfirm/include", "libfirm/build/gen/include/libfirm"],
	# Not -pedantic because I want to use c99 designated initializers
	CPPFLAGS=Split("-Werror -Wextra -Wall -ansi -std=c++17 -g"))
