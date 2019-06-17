#include <cassert>
#include <cstdint>
using int64 = int64_t;
struct ctx;
using nat64 = uint64_t;
using _void = uint8_t;
using byte = uint8_t;
struct arr__char;
struct comparison;
struct less {};
struct greater {};
struct equal {};
struct ctx {
	nat64 a;
	nat64 b;
	nat64 c;
};
using ptr__byte = byte*;
using ptr__char = char*;
struct comparison {
	enum class Kind {
		less,
		equal,
		greater,
	};
	Kind kind;
	union {
		less as_less;
		equal as_equal;
		greater as_greater;
	};
	comparison(less value) : kind{Kind::less}, as_less{value} {}
	comparison(equal value) : kind{Kind::equal}, as_equal{value} {}
	comparison(greater value) : kind{Kind::greater}, as_greater{value} {}
};
using fun_ptr2___void__ptr_ctx__ptr__byte = _void (*)(ctx*, ptr__byte);
struct arr__char {
	nat64 size;
	ptr__char data;
};

static_assert(sizeof(int64) == 8, "");
static_assert(sizeof(ctx) == 24, "");
static_assert(sizeof(nat64) == 8, "");
static_assert(sizeof(_void) == 1, "");
static_assert(sizeof(fun_ptr2___void__ptr_ctx__ptr__byte) == 8, "");
static_assert(sizeof(ptr__byte) == 8, "");
static_assert(sizeof(byte) == 1, "");
static_assert(sizeof(arr__char) == 16, "");
static_assert(sizeof(ptr__char) == 8, "");
static_assert(sizeof(char) == 1, "");
static_assert(sizeof(bool) == 1, "");
static_assert(sizeof(comparison) == 8, "");
static_assert(sizeof(less) == 1, "");
static_assert(sizeof(greater) == 1, "");
static_assert(sizeof(equal) == 1, "");

static arr__char _constantArr140056992965779 = arr__char{1, const_cast<char*>("1")};
static arr__char _constantArr140056992965781 = arr__char{30, const_cast<char*>("Hello natively compiled world!")};
static arr__char _constantArr140056992965780 = arr__char{6, const_cast<char*>("%.*s\n\0")};
static arr__char _constantArr140056992965778 = arr__char{1, const_cast<char*>("0")};
static arr__char _constantArr140056992965783 = arr__char{1, const_cast<char*>("2")};
static arr__char _constantArr140056992965784 = arr__char{1, const_cast<char*>("3")};
static arr__char _constantArr140056992965785 = arr__char{1, const_cast<char*>("4")};
static arr__char _constantArr140056992965782 = arr__char{1, const_cast<char*>("5")};
static arr__char _constantArr140056992965787 = arr__char{1, const_cast<char*>("6")};
static arr__char _constantArr140056992965788 = arr__char{1, const_cast<char*>("7")};
static arr__char _constantArr140056992965786 = arr__char{1, const_cast<char*>("8")};
static arr__char _constantArr140056992965789 = arr__char{1, const_cast<char*>("9")};
template <typename T>
T* _alloc(byte* out, T value) {
	T* res = static_cast<T*>(out);
	*res = value;
}

int64 main__int64();
ctx as__ctx__ctx(ctx value);
nat64 as_non_const__nat64__nat64(nat64 value);
ctx* ref_of_val__ptr_ctx__ctx(ctx b);
_void call_with_ctx___void__ptr_ctx_arg1_is_140733410007057(ctx* c);
_void call___void__fun_ptr2___void__ptr_ctx__ptr__byte__ptr_ctx__ptr__byte(fun_ptr2___void__ptr_ctx__ptr__byte f, ctx* p0, ptr__byte p1);
_void other_main___void__asLambda__dynamic(ctx* ctx, _void* _closure);
_void other_main___void(ctx* ctx);
_void print_sync___void__arr__char(arr__char s);
_void print_range___void_arg0_is_5_arg1_is_8_arg2_is_140733410007056(ctx* ctx);
extern "C" _void printf(ptr__char format, nat64 sz, ptr__char data);
_void other_main___void__lambda0(ctx* ctx, nat64 x);
_void print_range___void_arg0_is_6_arg1_is_8_arg2_is_140733410007056(ctx* ctx);
arr__char to_str__arr__char__nat64(ctx* ctx, nat64 n);
_void print_range___void_arg0_is_7_arg1_is_8_arg2_is_140733410007056(ctx* ctx);
bool equalequal__bool__nat64__nat64(nat64 a, nat64 b);
arr__char todo__arr__char();
comparison lessequalgreater__comparison__nat64__nat64(nat64 a, nat64 b);
arr__char hard_fail__arr__char_arg0_is_140056992965790();
int64 main__int64() {
	ctx ctx_by_val = ctx{0, 0, 0};
	ctx* ctx_by_ptr = &(ctx_by_val);
	call_with_ctx___void__ptr_ctx_arg1_is_140733410007057(ctx_by_ptr);
	return 0;
}
_void call_with_ctx___void__ptr_ctx_arg1_is_140733410007057(ctx* c) {
	return (&other_main___void__asLambda__dynamic)(c, nullptr);
}
_void other_main___void__asLambda__dynamic(ctx* ctx, _void* _closure) {
	return other_main___void(ctx);
}
_void other_main___void(ctx* ctx) {
	print_sync___void__arr__char(_constantArr140056992965779);
	print_sync___void__arr__char(_constantArr140056992965781);
	return print_range___void_arg0_is_5_arg1_is_8_arg2_is_140733410007056(ctx);
}
_void print_sync___void__arr__char(arr__char s) {
	return printf(&_constantArr140056992965780.data[0], s.size, s.data);
}
_void print_range___void_arg0_is_5_arg1_is_8_arg2_is_140733410007056(ctx* ctx) {
	other_main___void__lambda0(ctx, 5);
	return print_range___void_arg0_is_6_arg1_is_8_arg2_is_140733410007056(ctx);
}
_void other_main___void__lambda0(ctx* ctx, nat64 x) {
	return print_sync___void__arr__char(to_str__arr__char__nat64(ctx, x));
}
_void print_range___void_arg0_is_6_arg1_is_8_arg2_is_140733410007056(ctx* ctx) {
	other_main___void__lambda0(ctx, 6);
	return print_range___void_arg0_is_7_arg1_is_8_arg2_is_140733410007056(ctx);
}
arr__char to_str__arr__char__nat64(ctx* ctx, nat64 n) {
	if (equalequal__bool__nat64__nat64(n, 0)) {
		return _constantArr140056992965778;
	} else if (equalequal__bool__nat64__nat64(n, 1)) {
		return _constantArr140056992965779;
	} else if (equalequal__bool__nat64__nat64(n, 2)) {
		return _constantArr140056992965783;
	} else if (equalequal__bool__nat64__nat64(n, 3)) {
		return _constantArr140056992965784;
	} else if (equalequal__bool__nat64__nat64(n, 4)) {
		return _constantArr140056992965785;
	} else if (equalequal__bool__nat64__nat64(n, 5)) {
		return _constantArr140056992965782;
	} else if (equalequal__bool__nat64__nat64(n, 6)) {
		return _constantArr140056992965787;
	} else if (equalequal__bool__nat64__nat64(n, 7)) {
		return _constantArr140056992965788;
	} else if (equalequal__bool__nat64__nat64(n, 8)) {
		return _constantArr140056992965786;
	} else if (equalequal__bool__nat64__nat64(n, 9)) {
		return _constantArr140056992965789;
	} else {
		return todo__arr__char();
	}
}
_void print_range___void_arg0_is_7_arg1_is_8_arg2_is_140733410007056(ctx* ctx) {
	other_main___void__lambda0(ctx, 7);
	return 0;
}
bool equalequal__bool__nat64__nat64(nat64 a, nat64 b) {
	comparison matched = lessequalgreater__comparison__nat64__nat64(a, b);
	switch (matched.kind) {
		case comparison::Kind::less:
			return false;
		case comparison::Kind::equal:
			return true;
		case comparison::Kind::greater:
			return false;
		default: assert(0);
	}
}
arr__char todo__arr__char() {
	return hard_fail__arr__char_arg0_is_140056992965790();
}
comparison lessequalgreater__comparison__nat64__nat64(nat64 a, nat64 b) {
	if ((a < b)) {
		return comparison(less{});
	} else if ((b < a)) {
		return comparison(greater{});
	} else {
		return comparison(equal{});
	}
}
arr__char hard_fail__arr__char_arg0_is_140056992965790() {
	assert(0);
}


int main() { return (int) main__int64(); }
