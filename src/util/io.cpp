#include "./io.h"

#include <errno.h>
#include <fcntl.h> // O_RDONLY
#include <unistd.h>

#include "./arrUtil.h"

namespace {
	int tryOpen(const Path* path, const int flags, const int moreFlags) {
		Arena tempArena;
		const NulTerminatedStr s = pathToNulTerminatedStr(tempArena, path);
		const int fd = open(s.begin(), flags, moreFlags);
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

	const NulTerminatedStr copyCharPtrToNulTerminatedStr(Arena& arena, const char* begin) {
		return copyNulTerminatedStr(arena, nulTerminatedStrLiteral(begin));
	}
}

const Opt<const NulTerminatedStr> tryReadFile(Arena& arena, const Path* path) {
	using Ret = const Opt<const NulTerminatedStr>;

	const NulTerminatedStr n = pathToNulTerminatedStr(arena, path);

	const int fd = open(n.begin(), O_RDONLY);
	if (fd == -1) {
		if (errno == ENOENT)
			return none<const NulTerminatedStr>();
		else
			return todo<Ret>("failed to open file");
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

	res.set(res.size() - 1, '\0');

	return some<const NulTerminatedStr>(res.freeze());
}


void writeFileSync(const Path* path, const Str content) {
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

const Path* getCwd(Arena& arena) {
	char buff[256];
	const char* b = getcwd(buff, 256);
	if (b == nullptr) {
		return todo<const Path*>("getcwd failed");
	} else {
		assert(b == buff);
		const NulTerminatedStr str = copyCharPtrToNulTerminatedStr(arena, buff);
		return pathFromNulTerminatedStr(arena, str);
	}
}
