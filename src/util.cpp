#include "./util.h"

#include "./util/arrUtil.h" // arrEq

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

bool strEq(const Str a, const Str b) {
	return arrEq<const char, charEq>(a, b);
}
