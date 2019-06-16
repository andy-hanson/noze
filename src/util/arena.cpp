#include "./arena.h"

#include <cassert>
#include <cstdlib> // malloc

void* Arena::alloc(const size_t n_bytes) {
	if (begin == nullptr) {
		size_t size = 1024 * 1024;
		begin = static_cast<byte*>(malloc(size));
		cur = begin;
		end = begin + size;

		// Fill with 0xff for debugging
		for (byte* b = cur; b < end; b++)
			*b = 0xff;
	}

	// Since we filled with 0xff, should still be that way!
	byte* res = cur;
	cur = cur + n_bytes;
	assert(cur <= end);

	for (byte* b = res; b < cur; b++)
		assert(*b == 0xff);

	return res;
}
