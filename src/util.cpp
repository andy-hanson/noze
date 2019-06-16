#include "./util.h"

#include "./util/arrUtil.h"

bool strEq(const Str a, const Str b) {
	return arrEq<const char, charEq>(a, b);
}

Comparison compareStr(const Str a, const Str b) {
	// empty string is < all other strings
	if (isEmpty(a))
		return isEmpty(b) ? Comparison::equal : Comparison::less;
	else if (isEmpty(b))
		return Comparison::greater;
	else {
		const Comparison res = compareChar(a[0], b[0]);
		return res != Comparison::equal ? res : compareStr(tail(a), tail(b));
	}
}

const NulTerminatedStr strToNulTerminatedStr(Arena& arena, const Str s) {
	return cat(arena, s, Str{"\0", 1});
}
