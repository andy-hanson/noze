#include "./io.h"

#include <errno.h>
#include <fcntl.h> // O_RDONLY
#include <spawn.h> // posix_spawn
#include <string.h> // strerror
#include <sys/stat.h> // stat
#include <sys/wait.h> // waitpid
#include <unistd.h>

#include "./arrBuilder.h"
#include "./arrUtil.h"

namespace {
	int tryOpen(const AbsolutePath path, const int flags, const int moreFlags) {
		Arena tempArena {};
		const int fd = open(pathToCStr(&tempArena, path), flags, moreFlags);
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

	const Str copyCStrToStr(Arena* arena, const CStr begin) {
		return copyStr(arena, strLiteral(begin));
	}
}

const Bool fileExists(const AbsolutePath path) {
	Arena tempArena {};
	struct stat s;
	const char* pathCStr = pathToCStr(&tempArena, path);
	const int res = stat(pathCStr, &s);
	if (res == 0)
		return True;
	else if (res == ENOENT)
		return False;
	else {
		fprintf(stderr, "fileExists of %s failed\n", pathCStr);
		return todo<const Bool>("fileExists failed");
	}
}

const Opt<const NulTerminatedStr> tryReadFile(Arena* arena, const AbsolutePath path) {
	using Ret = const Opt<const NulTerminatedStr>;

	Arena tempArena;
	const CStr pathCStr = pathToCStr(&tempArena, path);
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

	const off_t fileSize = lseek(fd, 0, SEEK_END);
	if (fileSize == -1)
		return todo<Ret>("lseek failed");

	if (fileSize > 99999)
		return todo<Ret>("size suspiciously large");

	if (fileSize == 0)
		return some<const NulTerminatedStr>(emptyNulTerminatedStr);

	// Go back to the beginning so we can read
	const off_t off = lseek(fd, 0, SEEK_SET);
	if (off == -1)
		return todo<Ret>("lseek failed");

	assert(off == 0);

	MutStr res = newUninitializedMutArr<const char>(arena, fileSize + 1); // + 1 for the '\0'
	const ssize_t nBytesRead = read(fd, const_cast<char*>(mutArrBegin(&res)), fileSize);

	if (nBytesRead == -1)
		return todo<Ret>("read failed");

	if (nBytesRead != fileSize)
		return todo<Ret>("nBytesRead not right?");

	setAt<const char>(&res, mutArrSize(&res) - 1, '\0');

	return some<const NulTerminatedStr>(NulTerminatedStr{freeze(&res)});
}


void writeFileSync(const AbsolutePath path, const Str content) {
	const int fd = tryOpen(path, O_CREAT | O_WRONLY | O_TRUNC, 0b110100100);
	CloseFd closeFd { fd };

	const ssize_t wroteBytes = write(fd, content.begin(), size(content));
	if (wroteBytes != static_cast<ssize_t>(size(content))) {
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
		const int spawnStatus = posix_spawn(
			&pid,
			executablePath,
			nullptr,
			nullptr,
			// https://stackoverflow.com/questions/50596439/can-string-literals-be-passed-in-posix-spawns-argv
			const_cast<char* const*>(args),
			const_cast<char* const*>(environ));
		if (spawnStatus == 0) {
			int waitStatus;
			const int resPid = waitpid(pid, &waitStatus, 0);
			assert(resPid == pid);
			if (WIFEXITED(waitStatus))
				return WEXITSTATUS(waitStatus); // only valid if WIFEXITED
			else {
				if (WIFSIGNALED(waitStatus))
					return todo<int>("process exited with signal");
				else
					return todo<int>("process exited non-normally");
			}
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

	const size_t maxPathSize = 1024;

	const AbsolutePath getPathToThisExecutable(Arena* arena) {
		char buff[maxPathSize];
		ssize_t size = readlink("/proc/self/exe", buff, maxPathSize);
		if (size < 0)
			todo<void>("posix error");
		return parseAbsolutePath(arena, copyStr(arena, Str{buff, static_cast<const size_t>(size)}));
	}

	// Return should be const, but some posix functions aren't marked that way
	MutCStr const* convertArgs(Arena* arena, const char* executableCStr, const Arr<const Str> args) {
		ArrBuilder<MutCStr> cArgs {};
		// Make a mutable copy
		MutCStr executableCopy = const_cast<char*>(map<char>{}(
			arena,
			nulTerminatedStrLiteral(executableCStr).str,
			[](const char c) { return c; }).begin());
		add<MutCStr>(arena, &cArgs, executableCopy);
		for (const Str arg : args)
			add<MutCStr>(arena, &cArgs, const_cast<char*>(strToCStr(arena, arg)));
		add<MutCStr>(arena, &cArgs, nullptr);
		return finishArr(&cArgs).begin();
	}

	MutCStr const* convertEnviron(Arena* arena, const Environ environ) {
		ArrBuilder<MutCStr> cEnviron {};
		for (const KeyValuePair<const Str, const Str> pair : environ)
			add<MutCStr>(
				arena,
				&cEnviron,
				const_cast<char*>(strToCStr(arena, cat(arena, pair.key, strLiteral("="), pair.value))));
		add<MutCStr>(arena, &cEnviron, nullptr);
		return finishArr(&cEnviron).begin();
	}
}

int spawnAndWaitSync(const AbsolutePath executable, const Arr<const Str> args, const Environ environ) {
	Arena tempArena {};
	const CStr executableCStr = pathToCStr(&tempArena, executable);
	return spawnAndWaitSync(
		executableCStr,
		convertArgs(&tempArena, executableCStr, args),
		convertEnviron(&tempArena, environ));
}

void replaceCurrentProcess(
	const AbsolutePath executable,
	const Arr<const Str> args,
	const Environ environ
) {
	Arena tempArena {};
	const CStr executableCStr = pathToCStr(&tempArena, executable);
	const int err = execvpe(
		executableCStr,
		convertArgs(&tempArena, executableCStr, args),
		convertEnviron(&tempArena, environ));
	// 'execvpe' only returns if we failed to create the process (maybe executable does not exist?)
	assert(err == -1);
	fprintf(stderr, "Failed to launch %s: error %s\n", executableCStr, strerror(errno));
	assert(0);
}

const AbsolutePath getCwd(Arena* arena) {
	char buff[maxPathSize];
	const CStr b = getcwd(buff, maxPathSize);
	if (b == nullptr)
		return todo<const AbsolutePath>("getcwd failed");
	else {
		assert(b == buff);
		return parseAbsolutePath(arena, copyCStrToStr(arena, buff));
	}
}

const CommandLineArgs parseCommandLineArgs(Arena* arena, const int argc, CStr const* const argv) {
	const Arr<const CStr> allArgs = Arr<const CStr>{argv, safeIntToSizeT(argc)};
	const Arr<const Str> args = map<const Str>{}(arena, allArgs, [&](const CStr arg) {
		return strLiteral(arg);
	});
	// Take the tail because the first one is 'noze'
	return CommandLineArgs{getPathToThisExecutable(arena), tail(args), getEnviron(arena)};
}

const Environ getEnviron(Arena* arena) {
	ArrBuilder<const KeyValuePair<const Str, const Str>> res {};
	for (CStr const* env = environ; *env != nullptr; env++)
		add<const KeyValuePair<const Str, const Str>>(arena, &res, parseEnvironEntry(*env));
	return finishArr(&res);
}
