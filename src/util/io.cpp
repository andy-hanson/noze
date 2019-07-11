#include "./io.h"

#include <errno.h>
#include <fcntl.h> // O_RDONLY
#include <spawn.h> // posix_spawn
#include <sys/stat.h> // stat
#include <sys/wait.h> // waitpid
#include <unistd.h>

#include "./arrBuilder.h"
#include "./arrUtil.h"

namespace {
	int tryOpen(const AbsolutePath path, const int flags, const int moreFlags) {
		Arena tempArena;
		const int fd = open(pathToCStr(tempArena, path), flags, moreFlags);
		if (fd == -1)
			todo<void>("Can't write to file");
		return fd;
	}

	struct CloseFd {
		const int fd;
		CloseFd(const CloseFd&) = delete;
		~CloseFd() {
			close(fd);
		}
	};

	const Str copyCStrToStr(Arena& arena, const CStr begin) {
		return copyStr(arena, strLiteral(begin));
	}
}

const Bool fileExists(const AbsolutePath path) {
	Arena tempArena;
	struct stat s;
	const int res = stat(pathToCStr(tempArena, path), &s);
	if (res == 0)
		return True;
	else if (res == ENOENT)
		return False;
	else
		return todo<const Bool>("fileExists failed");
}

const Opt<const NulTerminatedStr> tryReadFile(Arena& arena, const AbsolutePath path) {
	using Ret = const Opt<const NulTerminatedStr>;

	Arena tempArena;
	const CStr pathCStr = pathToCStr(tempArena, path);
	const int fd = open(pathCStr, O_RDONLY);
	if (fd == -1) {
		if (errno == ENOENT)
			return none<const NulTerminatedStr>();
		else {
			printf("Failed to open file %s\n", pathCStr);
			return todo<Ret>("fail");
		}
	}

	CloseFd closeFd { fd };

	const off_t size = lseek(fd, 0, SEEK_END);
	if (size == -1)
		return todo<Ret>("lseek failed");

	if (size > 99999)
		return todo<Ret>("size suspiciously large");

	assert(size != 0); // TODO: handle empty files

	// Go back to the beginning so we can read
	const off_t off = lseek(fd, 0, SEEK_SET);
	if (off == -1)
		return todo<Ret>("lseek failed");

	assert(off == 0);

	MutStr res = newUninitializedMutArr<const char>(arena, size + 1); // + 1 for the '\0'
	const ssize_t nBytesRead = read(fd, const_cast<char*>(res.begin()), size);

	if (nBytesRead == -1)
		return todo<Ret>("read failed");

	if (nBytesRead != size)
		return todo<Ret>("nBytesRead not right?");

	setAt<const char>(res, res.size() - 1, '\0');

	return some<const NulTerminatedStr>(NulTerminatedStr{freeze(res)});
}


void writeFileSync(const AbsolutePath path, const Str content) {
	const int fd = tryOpen(path, O_CREAT | O_WRONLY | O_TRUNC, 0b110100100);
	CloseFd closeFd { fd };

	const ssize_t wroteBytes = write(fd, content.begin(), content.size);
	if (wroteBytes != static_cast<ssize_t>(content.size)) {
		if (wroteBytes == -1)
			return todo<void>("writefile failed");
		else
			return todo<void>("writeFile -- didn't write all the bytes?");
	}
}

namespace {
	void printArgs(CStr const* const args) {
		for (CStr const* arg = args; *arg != nullptr; arg++)
			printf("%s ", *arg);
	}

	int spawnAndWaitSync(const CStr executablePath, CStr const* const args, CStr const* const environ) {
		// TODO: KILL (debugging what we execute)
		if (false) {
			printf("Executing: %s ", executablePath);
			printArgs(args);
			printf("\nEnviron: ");
			printArgs(environ);
			printf("\n");
		}

		pid_t pid;
		int status = posix_spawn(
			&pid,
			executablePath,
			nullptr,
			nullptr,
			// https://stackoverflow.com/questions/50596439/can-string-literals-be-passed-in-posix-spawns-argv
			const_cast<char* const*>(args),
			const_cast<char* const*>(environ));
		if (status == 0) {
			const int resPid = waitpid(pid, &status, 0);
			assert(resPid == pid);
			if (!WIFEXITED(status))
				todo<void>("process exited non-normally");
			const int exitCode = WEXITSTATUS(status); // only valid if WIFEXITED
			return exitCode;
		} else
			return todo<int>("posix_spawn failed");
	}

	const KeyValuePair<const Str, const Str> parseEnvironEntry(const CStr entry) {
		CStr keyEnd = entry;
		for (; *keyEnd != '='; keyEnd++)
			assert(*keyEnd != '\0');
		const Str key = arrOfRange(entry, keyEnd);
		// Skip the '='
		const CStr valueBegin = keyEnd + 1;
		CStr valueEnd = valueBegin;
		for (; *valueEnd != '\0'; valueEnd++) {}
		const Str value = arrOfRange(valueBegin, valueEnd);
		return KeyValuePair<const Str, const Str>{key, value};
	}

	const AbsolutePath getCwd(Arena& arena) {
		const size_t maxSize = 256;
		char buff[maxSize];
		const CStr b = getcwd(buff, maxSize);
		if (b == nullptr) {
			return todo<const AbsolutePath>("getcwd failed");
		} else {
			assert(b == buff);
			return parseAbsolutePath(arena, copyCStrToStr(arena, buff));
		}
	}

	const AbsolutePath getPathToThisExecutable(Arena& arena, const AbsolutePath cwd, const Str relativePath) {
		const AbsolutePath res = parseAbsoluteOrRelPath(arena, relativePath).match(
			[&](const AbsolutePath p) {
				return p;
			},
			[&](const RelPath r) {
				return forceOrTodo(resolvePath(arena, cwd, r));
			});
		assert(fileExists(res));
		return res;
	}
}

int spawnAndWaitSync(const AbsolutePath executable, const Arr<const Str> args, const Environ environ) {
	Arena arena {};
	const CStr executableCStr = pathToCStr(arena, executable);

	CStr const* const cArgs = [&]() {
		ArrBuilder<const CStr> cArgs {};
		add<const CStr>(arena, &cArgs, executableCStr);
		for (const Str arg : args)
			add<const CStr>(arena, &cArgs, strToCStr(arena, arg));
		add<const CStr>(arena, &cArgs, nullptr);
		return finishArr(&cArgs).begin();
	}();

	CStr const* const cEnviron = [&]() {
		ArrBuilder<const CStr> cEnviron {};
		for (const KeyValuePair<const Str, const Str> pair : environ)
			add<const CStr>(arena, &cEnviron, strToCStr(arena, cat(arena, pair.key, strLiteral("="), pair.value)));
		add<const CStr>(arena, &cEnviron, nullptr);
		return finishArr(&cEnviron).begin();
	}();

	return spawnAndWaitSync(executableCStr, cArgs, cEnviron);
}

const CommandLineArgs parseArgs(Arena& arena, const int argc, CStr const* const argv) {
	const Arr<const CStr> allArgs = Arr<const CStr>{argv, safeIntToSizeT(argc)};
	const Arr<const Str> args = map<const Str>{}(arena, allArgs, [&](const CStr arg) {
		return strLiteral(arg);
	});
	const AbsolutePath cwd = getCwd(arena);
	// Take the tail because the first one is the executable
	return CommandLineArgs{cwd, getPathToThisExecutable(arena, cwd, first(args)), tail(args), getEnviron(arena)};
}

const Environ getEnviron(Arena& arena) {
	ArrBuilder<const KeyValuePair<const Str, const Str>> res {};
	for (CStr const* env = environ; *env != nullptr; env++)
		add<const KeyValuePair<const Str, const Str>>(arena, &res, parseEnvironEntry(*env));
	return finishArr(&res);
}
