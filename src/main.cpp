#include <cstdio>
#include <cstdlib> // exit
#include <signal.h>
#include <sys/resource.h> // rlimit
#include <unistd.h> // getcwd

#include "./util/arrUtil.h"
#include "./util/io.h"
#include "./concreteModel.h"

#include "./compiler.h"
#include "./cli.h"

namespace {
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

int main(const int argc, CStr const* const argv) {
	setLimits();

	if (true)
		return cli(argc, argv);
	else {
		printf("%zu\n", sizeof(Bool));
		return 0;
	}
}
