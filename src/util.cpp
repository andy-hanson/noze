#include "./util.h"

void* Arena::alloc(const size_t n_bytes) {
	if (begin == nullptr) {
		size_t size = 1024 * 1024;
		begin = static_cast<byte*>(malloc(size));
		cur = begin;
		end = begin + size;
	}

	byte* res = cur;
	cur = cur + n_bytes;
	assert(cur <= end);
	return res;
}

const Str copyStr(Arena& arena, const Str in) {
	return map<const char>{}(arena, in, [](const char c) { return c; });
}

bool strEq(const Str& a, const Str& b) {
	return (a.size == b.size) &&
		(a.size == 0 || strEq(tail(a), tail(b)));
}
