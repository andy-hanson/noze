#include <cstdio>
#include <signal.h>
#include <sys/resource.h> // rlimit
#include <unistd.h> // getcwd

#include "./frontend/ast.h"
#include "./frontend/frontendCompile.h"
#include "./frontend/readOnlyStorage.h"
#include "./frontend/showDiag.h"
#include "./util.h"
#include "./util/arrUtil.h"
#include "./concreteModel.h"

namespace {
	const NulTerminatedStr copyCharPtrToNulTerminatedStr(Arena& arena, const char* begin) {
		return copyNulTerminatedStr(arena, nulTerminatedStrLiteral(begin));
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

	void test() {
		Arena tempArena {};
		Arena modelArena {};

		// Get the current executable file path
		const Path* cwd = getCwd(modelArena);

		const Path* include = childPath(modelArena, cwd, strLiteral("include"));
		const Path* test = childPath(modelArena, cwd, strLiteral("test"));

		const ReadOnlyStorages storages = ReadOnlyStorages{ReadOnlyStorage{include}, ReadOnlyStorage{test}};

		const Result<const Program, const Diagnostics> programResult = frontendCompile(
			modelArena, storages, rootPath(modelArena, strLiteral("a.nz")));

		printf("GOT HERE\n");

		programResult.match(
			[&](const Program program) {
				unused(program);
				todo<void>("!!!");
			},
			[](const Diagnostics diagnostics) {
				printf("GOT HERE 3\n");
				printDiagnostics(diagnostics);
			});
	}

	rlimit getRlimit(const int resource) {
		rlimit r;
		getrlimit(resource, &r);
		return r;
	}

	void setRlimit(const int resource, const rlimit r) {
		const int err = setrlimit(resource, &r);
		if (err == -1)
			todo<void>("setrlimit failed");
		assert(err == 0);
	}

	void reduceSoftLimit(const int resource, const rlim_t lim) {
		rlimit cur = getRlimit(resource);
		assert(lim < cur.rlim_max);
		cur.rlim_cur = lim;
		setRlimit(resource, cur);
	}

	void onSignal(int sig) {
		if (sig == SIGXCPU)
			perror("Hit CPU time limit -- probably an infinite loop somewhere\n");
		exit(sig);
	}


	void setLimits() {
		// Should take less than 1 second, should not consume more than 1 << 28 bytes (256MB).
		reduceSoftLimit(RLIMIT_AS, 1 << 28);
		signal(SIGXCPU, &onSignal);
	}
}

int main(void) {
	setLimits();
	test();
	return 0;
}
