#include "./str.h"

#include "./arrUtil.h"

const Bool strEq(const Str a, const Str b) {
	return arrEq<const char, eq<const char>>(a, b);
}

Comparison compareStr(const Str a, const Str b) {
	// empty string is < all other strings
	if (isEmpty(a))
		return isEmpty(b) ? Comparison::equal : Comparison::less;
	else if (isEmpty(b))
		return Comparison::greater;
	else {
		const Comparison res = comparePrimitive<const char>(first(a), first(b));
		return res != Comparison::equal ? res : compareStr(tail(a), tail(b));
	}
}

const NulTerminatedStr strToNulTerminatedStr(Arena* arena, const Str s) {
	return NulTerminatedStr{cat(arena, s, Str{"\0", 1})};
}

const Str copyStr(Arena* arena, const Str s) {
	return copyArr(arena, s);
}

const Str stripNulTerminator(const NulTerminatedStr n)  {
	return rtail(n.str);
}
