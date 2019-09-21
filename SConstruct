# See doc/developing.md

from os import environ
from os.path import dirname, join, pardir, realpath

env = Environment()
# This ensures we get color output for compile errors
env["ENV"]["TERM"] = environ["TERM"]

# NOTE: These options should be exacly the same as in `build.sh`.
env.Program(
	"bin/noze",
	Glob("src/*.cpp") + Glob("src/**/*.cpp"),
	LIBPATH="libfirm/build/debug",
	LIBS="firm",
	CPPPATH=["libfirm/include", "libfirm/build/gen/include/libfirm"],
	CPPFLAGS=[
		"-Werror",
		"-Wextra",
		"-Wall",
		"-ansi",
		"-std=c++17",
		"-g",
	],
	RPATH=env.Literal(join("\\$$ORIGIN", pardir, "libfirm", "build", "debug")))
