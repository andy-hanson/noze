#include "./arena.h"

#include <cstdlib> // malloc

namespace {
	// TODO: don't hardcode
	// 4MB
	const size_t ARENA_SIZE = 4 * 1024 * 1024;
}

void* alloc(Arena* arena, const size_t n_bytes) {
	if (arena->begin == nullptr) {
		size_t size = ARENA_SIZE;
		arena->begin = static_cast<byte*>(malloc(size));
		assert(arena->begin != nullptr);
		arena->cur = arena->begin;
		arena->end = arena->begin + size;

		// Fill with 0xff for debugging
		for (byte* b = arena->cur; b < arena->end; b++)
			*b = 0xff;
	}

	assert(n_bytes < 999999); // sanity check

	// Since we filled with 0xff, should still be that way!
	byte* res = arena->cur;
	arena->cur = arena->cur + n_bytes;

	if (arena->cur > arena->end)
		todo<void>("Ran out of space!");

	for (byte* b = res; b < arena->cur; b++)
		assert(*b == 0xff);

	return res;
}

Arena::~Arena() {
	if (begin != nullptr)
		free(begin);
}
