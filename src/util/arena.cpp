#include "./arena.h"

#include <cassert>
#include <cstdlib> // malloc

#include "../util.h"

void* Arena::alloc(const size_t n_bytes) {
	if (begin == nullptr) {
		// 4MB
		size_t size = 4 * 1024 * 1024;
		begin = static_cast<byte*>(malloc(size));
		cur = begin;
		end = begin + size;

		// Fill with 0xff for debugging
		for (byte* b = cur; b < end; b++)
			*b = 0xff;
	}

	assert(n_bytes < 99999); // sanity check

	// Since we filled with 0xff, should still be that way!
	byte* res = cur;
	cur = cur + n_bytes;

	if (cur > end)
		todo<void>("Ran out of space!");

	for (byte* b = res; b < cur; b++)
		assert(*b == 0xff);

	return res;
}
