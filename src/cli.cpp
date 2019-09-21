#include "./cli.h"

#include <stdio.h>

#include "./compiler.h"
#include "./util/arrUtil.h"
#include "./util/io.h"
#include "./util/path.h"

namespace {
	struct ProgramDirAndMain {
		const AbsolutePath programDir;
		const Path* mainPath;
	};

	bool isHelp(const Str s) {
		return strEqLiteral(s, "help") || strEqLiteral(s, "-help") || strEqLiteral(s, "--help");
	}

	struct Command {
		struct Build {
			const ProgramDirAndMain programDirAndMain;
		};
		struct Help {
			const Bool isDueToCommandParseError;
		};
		struct HelpBuild {};
		struct HelpRun {};
		// Also builds first
		struct Run {
			const ProgramDirAndMain programDirAndMain;
			const Arr<const Str> programArgs;
		};
		struct Version {};
	private:
		enum class Kind {
			build,
			help,
			helpBuild,
			helpRun,
			run,
			test,
			version,
		};
		const Kind kind;
		union {
			const Build build;
			const Help help;
			const HelpBuild helpBuild;
			const HelpRun helpRun;
			const Run run;
			const Version version;
		};
	public:
		explicit inline Command(const Build _build) : kind{Kind::build}, build{_build} {}
		explicit inline Command(const Help _help) : kind{Kind::help}, help{_help} {}
		explicit inline Command(const HelpBuild _helpBuild) : kind{Kind::helpBuild}, helpBuild{_helpBuild} {}
		explicit inline Command(const HelpRun _helpRun) : kind{Kind::helpRun}, helpRun{_helpRun} {}
		explicit inline Command(const Run _run) : kind{Kind::run}, run{_run} {}
		explicit inline Command(const Version _version) : kind{Kind::version}, version{_version} {}

		template <
			typename CbBuild,
			typename CbHelp,
			typename CbHelpBuild,
			typename CbHelpRun,
			typename CbRun,
			typename CbVersion
		>
		inline auto match(
			CbBuild cbBuild,
			CbHelp cbHelp,
			CbHelpBuild cbHelpBuild,
			CbHelpRun cbHelpRun,
			CbRun cbRun,
			CbVersion cbVersion
		) const {
			switch (kind) {
				case Kind::build:
					return cbBuild(build);
				case Kind::help:
					return cbHelp(help);
				case Kind::helpBuild:
					return cbHelpBuild(helpBuild);
				case Kind::helpRun:
					return cbHelpRun(helpRun);
				case Kind::run:
					return cbRun(run);
				case Kind::version:
					return cbVersion(version);
				default:
					assert(0);
			}
		}
	};

	const AbsolutePath parseCwdRelativePath(Arena* arena, const AbsolutePath cwd, const Str arg) {
		return parseAbsoluteOrRelPath(arena, arg).match(
			[](const AbsolutePath p) {
				return p;
			},
			[&](const RelPath p) {
				return forceOrTodo(resolvePath(arena, cwd, p));
			});
	}

	const ProgramDirAndMain parseProgramDirAndMain(Arena* arena, const AbsolutePath cwd, const Str arg) {
		const AbsolutePath mainAbsolutePath = parseCwdRelativePath(arena, cwd, arg);
		const AbsolutePath dir = forceOrTodo(parent(mainAbsolutePath));
		const Str name = baseName(mainAbsolutePath);
		return ProgramDirAndMain{dir, rootPath(arena, name)};
	}

	const Command parseBuildCommand(Arena* arena, const AbsolutePath cwd, const Arr<const Str> args) {
		if (size(args) == 1 && !isHelp(only(args)))
			return Command{Command::Build{parseProgramDirAndMain(arena, cwd, only(args))}};
		else
			return Command{Command::HelpBuild{}};
	}

	const Command parseRunCommand(Arena* arena, const AbsolutePath cwd, const Arr<const Str> args) {
		if (size(args) == 0 || isHelp(first(args)))
			return Command{Command::HelpRun{}};
		else {
			const ProgramDirAndMain programDirAndMain = parseProgramDirAndMain(arena, cwd, first(args));
			if (size(args) == 1)
				return Command{Command::Run{programDirAndMain, emptyArr<const Str>()}};
			else if (strEqLiteral(at(args, 1), "--"))
				return Command{Command::Run{programDirAndMain, slice(args, 2)}};
			else
				return Command{Command::HelpRun{}};
		}
	}

	const Command parseCommand(Arena* arena, const AbsolutePath cwd, const Arr<const Str> args) {
		// Parse command name
		if (size(args) == 0)
			return Command{Command::Help{True}};
		else {
			const Str arg0 = first(args);
			if (strEqLiteral(arg0, "help") || strEqLiteral(arg0, "--help"))
				return Command{Command::Help{False}};
			else if (strEqLiteral(arg0, "version") || strEqLiteral(arg0, "--version"))
				return Command{Command::Version{}};
			else if (strEqLiteral(arg0, "build"))
				return parseBuildCommand(arena, cwd, tail(args));
			else if (strEqLiteral(arg0, "run"))
				return parseRunCommand(arena, cwd, tail(args));
			// Allow `noze foo.nz args` to translate to `noze run foo.nz -- args`
			else if (endsWith(arg0, strLiteral(".nz")))
				return Command{Command::Run{parseProgramDirAndMain(arena, cwd, arg0), tail(args)}};
			else
				return Command{Command::Help{True}};
		}
	}

	const AbsolutePath climbUpToNoze(const AbsolutePath p) {
		const Opt<const AbsolutePath> par = parent(p);
		if (strEqLiteral(baseName(p), "noze"))
			return p;
		else if (has(par))
			return climbUpToNoze(force(par));
		else
			return todo<const AbsolutePath>("no 'noze' directory in path");
	}

	const AbsolutePath getNozeDirectory(const AbsolutePath pathToThisExecutable) {
		return climbUpToNoze(forceOrTodo(parent(pathToThisExecutable)));
	}

	int helpBuild() {
		printf("Command: noze build [PATH]\n"
			"\tCompiles the program at [PATH] to a '.cpp' and executable file with the same name.\n"
			"\tNo options.\n");
		return 0;
	}

	int helpRun() {
		printf("Command: noze run [PATH]\n"
			"Command: noze run [PATH] -- args\n"
			"\tDoes the same as 'noze build [PATH]', then runs the executable it created.\n"
			"\tNo options.\n"
			"\tArguments after `--` will be sent to the program.");
		return 0;
	}

	int help(const Bool isDueToCommandParseError) {
		printf("Command: noze [PATH ENDING IN '.nz'] args\n"
			"\tSame as `noze run [PATH] -- args\n");
		helpBuild();
		printf("\n");
		helpRun();
		return isDueToCommandParseError ? 1 : 0;
	}

	int go(const CommandLineArgs args) {
		Arena arena {};
		const AbsolutePath nozeDir = getNozeDirectory(args.pathToThisExecutable);
		const Command command = parseCommand(&arena, getCwd(&arena), args.args);
		return command.match(
			[&](const Command::Build b) {
				return build(nozeDir, b.programDirAndMain.programDir, b.programDirAndMain.mainPath, args.environ);
			},
			[&](const Command::Help c) {
				return help(c.isDueToCommandParseError);
			},
			[&](const Command::HelpBuild) {
				return helpBuild();
			},
			[&](const Command::HelpRun) {
				return helpRun();
			},
			[&](const Command::Run r) {
				return buildAndRun(
					nozeDir,
					r.programDirAndMain.programDir,
					r.programDirAndMain.mainPath,
					r.programArgs,
					args.environ);
			},
			[&](const Command::Version) {
				printf("Approximately 0.000\n");
				return 0;
			});
	}
}

int cli(const int argc, CStr const* const argv){
	Arena arena {};
	return go(parseCommandLineArgs(&arena, argc, argv));
}
