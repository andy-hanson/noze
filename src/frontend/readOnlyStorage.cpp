#include "./readOnlyStorage.h"

#include <errno.h>
#include <fcntl.h> // O_RDONLY
#include <unistd.h>

namespace {
	struct CloseFd {
		const int fd;
		~CloseFd() {
			close(fd);
		}
	};

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
}

const Opt<const NulTerminatedStr> ReadOnlyStorage::tryReadFile(Arena& arena, const Path* path) const {
	return ::tryReadFile(arena, addManyChildren(arena, root, path));
}
