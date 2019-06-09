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
		(a.size == 0 || strEq(a.tail(), b.tail()));
}

bool strEqLiteral(const Str s, const char* c) {
	return *c == '\0'
		? s.isEmpty()
		: !s.isEmpty() && strEqLiteral(s.tail(), c + 1);
}
