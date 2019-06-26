#include <cassert>
#include <cstdint>
using int64 = int64_t;
struct ctx;
using nat64 = uint64_t;
using _void = uint8_t;
using byte = uint8_t;
struct cell__nat64 {
	nat64 value;
};
struct arr__char;
struct range_nat64 {
	nat64 lo;
	nat64 hi;
};
struct other_main___void__lambda0___closure {
	cell__nat64* sm;
};
struct comparison;
struct less {};
struct greater {};
struct equal {};
struct mut_arr__char;
struct cat__arr__char__arr__char__arr__char__lambda0___closure;
struct cat__arr__char__arr__char__arr__char__lambda1___closure;
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
struct mut_arr__char {
	bool frozen;
	nat64 size;
	nat64 capacity;
	ptr__char data;
};
using fun_ptr2___void__ptr_ctx__ptr__byte = _void (*)(ctx*, ptr__byte);
struct arr__char {
	nat64 size;
	ptr__char data;
};
struct cat__arr__char__arr__char__arr__char__lambda0___closure {
	mut_arr__char* m;
	arr__char a;
};
struct cat__arr__char__arr__char__arr__char__lambda1___closure {
	mut_arr__char* m;
	arr__char a;
	arr__char b;
};

static_assert(sizeof(int64) == 8, "");
static_assert(sizeof(ctx) == 24, "");
static_assert(sizeof(nat64) == 8, "");
static_assert(sizeof(_void) == 1, "");
static_assert(sizeof(fun_ptr2___void__ptr_ctx__ptr__byte) == 8, "");
static_assert(sizeof(ptr__byte) == 8, "");
static_assert(sizeof(byte) == 1, "");
static_assert(sizeof(cell__nat64) == 8, "");
static_assert(sizeof(arr__char) == 16, "");
static_assert(sizeof(ptr__char) == 8, "");
static_assert(sizeof(char) == 1, "");
static_assert(sizeof(range_nat64) == 16, "");
static_assert(sizeof(other_main___void__lambda0___closure) == 8, "");
static_assert(sizeof(bool) == 1, "");
static_assert(sizeof(comparison) == 8, "");
static_assert(sizeof(less) == 1, "");
static_assert(sizeof(greater) == 1, "");
static_assert(sizeof(equal) == 1, "");
static_assert(sizeof(mut_arr__char) == 32, "");
static_assert(sizeof(cat__arr__char__arr__char__arr__char__lambda0___closure) == 24, "");
static_assert(sizeof(cat__arr__char__arr__char__arr__char__lambda1___closure) == 40, "");

static arr__char _constantArr4661500 = arr__char{4, const_cast<char*>("true")};
static arr__char _constantArr4661501 = arr__char{5, const_cast<char*>("false")};
static arr__char _constantArr4661497 = arr__char{1, const_cast<char*>("1")};
static range_nat64 _constant____range_nat64__0 = range_nat64{0, 1000};
static arr__char _constantArr4661502 = arr__char{6, const_cast<char*>("%.*s\n\0")};
static arr__char _constantArr4661496 = arr__char{1, const_cast<char*>("0")};
static arr__char _constantArr4661498 = arr__char{1, const_cast<char*>("2")};
static arr__char _constantArr4661499 = arr__char{1, const_cast<char*>("3")};
static arr__char _constantArr4661504 = arr__char{1, const_cast<char*>("4")};
static arr__char _constantArr4661508 = arr__char{1, const_cast<char*>("5")};
static arr__char _constantArr4661510 = arr__char{1, const_cast<char*>("6")};
static arr__char _constantArr4661511 = arr__char{1, const_cast<char*>("7")};
static arr__char _constantArr4661512 = arr__char{1, const_cast<char*>("8")};
static arr__char _constantArr4661513 = arr__char{1, const_cast<char*>("9")};
template <typename T>
T* _alloc(byte* out, T value) {
	T* res = reinterpret_cast<T*>(out);
	*res = value;
	return res;
}

int64 main__int64();
ctx as__ctx__ctx(ctx value);
nat64 as_non_const__nat64__nat64(nat64 value);
ctx* ref_of_val__ptr_ctx__ctx(ctx b);
_void call_with_ctx___void__ptr_ctx_arg1_is_140732801772801(ctx* c);
_void call___void__fun_ptr2___void__ptr_ctx__ptr__byte__ptr_ctx__ptr__byte(fun_ptr2___void__ptr_ctx__ptr__byte f, ctx* p0, ptr__byte p1);
_void other_main___void__asLambda__dynamic(ctx* ctx, ptr__byte );
_void other_main___void(ctx* ctx);
_void print_sync___void__arr__char(arr__char s);
ptr__byte allocate_bytes__ptr__byte__nat64(ctx* ctx, nat64 size);
_void each___void__range_nat64__other_main___void__lambda0___closure_klb1_is_other_main___void__lambda0(ctx* ctx, range_nat64 r, other_main___void__lambda0___closure f);
arr__char to_str__arr__char__nat64(ctx* ctx, nat64 n);
nat64 get__nat64__ptr_cell__nat64(ctx* ctx, cell__nat64* c);
extern "C" _void printf(ptr__char format, nat64 sz, ptr__char data);
extern "C" ptr__byte malloc(nat64 size);
bool empty__q__bool__range_nat64(ctx* ctx, range_nat64 r);
_void other_main___void__lambda0(ctx* ctx, other_main___void__lambda0___closure _closure, nat64 n);
range_nat64 tail__range_nat64__range_nat64(ctx* ctx, range_nat64 r);
bool equalequal__bool__nat64__nat64(nat64 a, nat64 b);
nat64 div__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
nat64 mod__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
arr__char cat__arr__char__arr__char__arr__char(ctx* ctx, arr__char a, arr__char b);
bool or__bool__bool__bool(bool a, bool b);
bool is_multiple__q__bool__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
_void set___void__ptr_cell__nat64__nat64(ctx* ctx, cell__nat64* c, nat64 v);
nat64 plus__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
_void forbid___void__bool(ctx* ctx, bool condition);
range_nat64 to__range_nat64__nat64__nat64(ctx* ctx, nat64 lo, nat64 hi);
nat64 incr__nat64__nat64(nat64 n);
comparison lessequalgreater__comparison__nat64__nat64(nat64 a, nat64 b);
nat64 unsafe_div__nat64__nat64__nat64(nat64 a, nat64 b);
nat64 unsafe_mod__nat64__nat64__nat64(nat64 a, nat64 b);
mut_arr__char* uninitialized_mut_arr__ptr_mut_arr__char__nat64(ctx* ctx, nat64 size);
_void each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, range_nat64 r, cat__arr__char__arr__char__arr__char__lambda0___closure f);
_void each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, range_nat64 r, cat__arr__char__arr__char__arr__char__lambda1___closure f);
arr__char freeze__arr__char__ptr_mut_arr__char(mut_arr__char* a);
nat64 wrapping_add__nat64__nat64__nat64(nat64 a, nat64 b);
_void assert___void__bool(ctx* ctx, bool condition);
bool and__bool__bool__bool(bool a, bool b);
bool greaterequal__bool__nat64__nat64(nat64 a, nat64 b);
bool not__bool__bool(bool a);
bool lessequal__bool__nat64__nat64(nat64 a, nat64 b);
ptr__char uninitialized_data__ptr__char__nat64(ctx* ctx, nat64 size);
_void cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, cat__arr__char__arr__char__arr__char__lambda0___closure _closure, nat64 i);
_void cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, cat__arr__char__arr__char__arr__char__lambda1___closure _closure, nat64 i);
arr__char unsafe_as_arr__arr__char__ptr_mut_arr__char(mut_arr__char* a);
_void fail___void_arg0_is_4661506(ctx* ctx);
bool less__bool__nat64__nat64(nat64 a, nat64 b);
nat64 wrapping_mul__nat64__nat64__nat64(nat64 a, nat64 b);
ptr__char ptr_cast__ptr__char__ptr__byte(ptr__byte p);
_void set_at___void__ptr_mut_arr__char__nat64__char(ctx* ctx, mut_arr__char* a, nat64 index, char value);
char at__char__arr__char__nat64(ctx* ctx, arr__char a, nat64 index);
_void todo___void();
_void set___void__ptr__char__char(ptr__char p, char value);
ptr__char plus__ptr__char__ptr__char__nat64(ptr__char p, nat64 offset);
char deref__char__ptr__char(ptr__char p);
_void hard_fail___void_arg0_is_4661507();
int64 main__int64() {
	ctx ctx_by_val = ctx{0, 0, 0};
	ctx* ctx_by_ptr = &(ctx_by_val);
	call_with_ctx___void__ptr_ctx_arg1_is_140732801772801(ctx_by_ptr);
	return 0;
}
_void call_with_ctx___void__ptr_ctx_arg1_is_140732801772801(ctx* c) {
	return (&other_main___void__asLambda__dynamic)(c, 0);
}
_void other_main___void__asLambda__dynamic(ctx* ctx, ptr__byte ) {
	return other_main___void(ctx);
}
_void other_main___void(ctx* ctx) {
	print_sync___void__arr__char(_constantArr4661500);
	print_sync___void__arr__char(_constantArr4661501);
	print_sync___void__arr__char(_constantArr4661497);
	
	cell__nat64* sm = _alloc<cell__nat64>(allocate_bytes__ptr__byte__nat64(ctx, 8), cell__nat64{0});
	each___void__range_nat64__other_main___void__lambda0___closure_klb1_is_other_main___void__lambda0(ctx, _constant____range_nat64__0, other_main___void__lambda0___closure{sm});
	
	return print_sync___void__arr__char(to_str__arr__char__nat64(ctx, get__nat64__ptr_cell__nat64(ctx, sm)));
}
_void print_sync___void__arr__char(arr__char s) {
	return printf(&_constantArr4661502.data[0], s.size, s.data);
}
ptr__byte allocate_bytes__ptr__byte__nat64(ctx* ctx, nat64 size) {
	return malloc(size);
}
_void each___void__range_nat64__other_main___void__lambda0___closure_klb1_is_other_main___void__lambda0(ctx* ctx, range_nat64 r, other_main___void__lambda0___closure f) {
	if (empty__q__bool__range_nat64(ctx, r)) {
		return 0;
	} else {
		other_main___void__lambda0(ctx, f, r.lo);
		return each___void__range_nat64__other_main___void__lambda0___closure_klb1_is_other_main___void__lambda0(ctx, tail__range_nat64__range_nat64(ctx, r), f);
	}
}
arr__char to_str__arr__char__nat64(ctx* ctx, nat64 n) {
	if (equalequal__bool__nat64__nat64(n, 0)) {
		return _constantArr4661496;
	} else if (equalequal__bool__nat64__nat64(n, 1)) {
		return _constantArr4661497;
	} else if (equalequal__bool__nat64__nat64(n, 2)) {
		return _constantArr4661498;
	} else if (equalequal__bool__nat64__nat64(n, 3)) {
		return _constantArr4661499;
	} else if (equalequal__bool__nat64__nat64(n, 4)) {
		return _constantArr4661504;
	} else if (equalequal__bool__nat64__nat64(n, 5)) {
		return _constantArr4661508;
	} else if (equalequal__bool__nat64__nat64(n, 6)) {
		return _constantArr4661510;
	} else if (equalequal__bool__nat64__nat64(n, 7)) {
		return _constantArr4661511;
	} else if (equalequal__bool__nat64__nat64(n, 8)) {
		return _constantArr4661512;
	} else if (equalequal__bool__nat64__nat64(n, 9)) {
		return _constantArr4661513;
	} else {
		arr__char hi = to_str__arr__char__nat64(ctx, div__nat64__nat64__nat64(ctx, n, 10));
		arr__char lo = to_str__arr__char__nat64(ctx, mod__nat64__nat64__nat64(ctx, n, 10));
		return cat__arr__char__arr__char__arr__char(ctx, hi, lo);
	}
}
nat64 get__nat64__ptr_cell__nat64(ctx* ctx, cell__nat64* c) {
	return c->value;
}
bool empty__q__bool__range_nat64(ctx* ctx, range_nat64 r) {
	return equalequal__bool__nat64__nat64(r.lo, r.hi);
}
_void other_main___void__lambda0(ctx* ctx, other_main___void__lambda0___closure _closure, nat64 n) {
	if (is_multiple__q__bool__nat64__nat64(ctx, n, 3) || is_multiple__q__bool__nat64__nat64(ctx, n, 5)) {
		return set___void__ptr_cell__nat64__nat64(ctx, _closure.sm, plus__nat64__nat64__nat64(ctx, get__nat64__ptr_cell__nat64(ctx, _closure.sm), n));
	} else {
		return 0;
	}
}
range_nat64 tail__range_nat64__range_nat64(ctx* ctx, range_nat64 r) {
	forbid___void__bool(ctx, empty__q__bool__range_nat64(ctx, r));
	return to__range_nat64__nat64__nat64(ctx, incr__nat64__nat64(r.lo), r.hi);
}
bool equalequal__bool__nat64__nat64(nat64 a, nat64 b) {
	comparison matched = lessequalgreater__comparison__nat64__nat64(a, b);
	switch (matched.kind) {
		case comparison::Kind::less: {
			return false;
		}
		case comparison::Kind::equal: {
			return true;
		}
		case comparison::Kind::greater: {
			return false;
		}
		default: assert(0);
	}
}
nat64 div__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b) {
	forbid___void__bool(ctx, equalequal__bool__nat64__nat64(b, 0));
	if (equalequal__bool__nat64__nat64(b, 0)) {
		return 7;
	} else {
		return a / b;
	}
}
nat64 mod__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b) {
	forbid___void__bool(ctx, equalequal__bool__nat64__nat64(b, 0));
	return a % b;
}
arr__char cat__arr__char__arr__char__arr__char(ctx* ctx, arr__char a, arr__char b) {
	mut_arr__char* m = uninitialized_mut_arr__ptr_mut_arr__char__nat64(ctx, plus__nat64__nat64__nat64(ctx, a.size, b.size));
	each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx, to__range_nat64__nat64__nat64(ctx, 0, a.size), cat__arr__char__arr__char__arr__char__lambda0___closure{m, a});
	each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx, to__range_nat64__nat64__nat64(ctx, 0, b.size), cat__arr__char__arr__char__arr__char__lambda1___closure{m, a, b});
	return freeze__arr__char__ptr_mut_arr__char(m);
}
bool is_multiple__q__bool__nat64__nat64(ctx* ctx, nat64 a, nat64 b) {
	return equalequal__bool__nat64__nat64(mod__nat64__nat64__nat64(ctx, a, b), 0);
}
_void set___void__ptr_cell__nat64__nat64(ctx* ctx, cell__nat64* c, nat64 v) {
	c->value = v;
	return 0;
}
nat64 plus__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b) {
	nat64 res = a + b;
	assert___void__bool(ctx, greaterequal__bool__nat64__nat64(res, a) && greaterequal__bool__nat64__nat64(res, b));
	return res;
}
_void forbid___void__bool(ctx* ctx, bool condition) {
	return assert___void__bool(ctx, !(condition));
}
range_nat64 to__range_nat64__nat64__nat64(ctx* ctx, nat64 lo, nat64 hi) {
	assert___void__bool(ctx, lessequal__bool__nat64__nat64(lo, hi));
	return range_nat64{lo, hi};
}
nat64 incr__nat64__nat64(nat64 n) {
	return n + 1;
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
mut_arr__char* uninitialized_mut_arr__ptr_mut_arr__char__nat64(ctx* ctx, nat64 size) {
	return _alloc<mut_arr__char>(allocate_bytes__ptr__byte__nat64(ctx, 32), mut_arr__char{false, size, size, uninitialized_data__ptr__char__nat64(ctx, size)});
}
_void each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, range_nat64 r, cat__arr__char__arr__char__arr__char__lambda0___closure f) {
	if (empty__q__bool__range_nat64(ctx, r)) {
		return 0;
	} else {
		cat__arr__char__arr__char__arr__char__lambda0(ctx, f, r.lo);
		return each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx, tail__range_nat64__range_nat64(ctx, r), f);
	}
}
_void each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, range_nat64 r, cat__arr__char__arr__char__arr__char__lambda1___closure f) {
	if (empty__q__bool__range_nat64(ctx, r)) {
		return 0;
	} else {
		cat__arr__char__arr__char__arr__char__lambda1(ctx, f, r.lo);
		return each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx, tail__range_nat64__range_nat64(ctx, r), f);
	}
}
arr__char freeze__arr__char__ptr_mut_arr__char(mut_arr__char* a) {
	a->frozen = true;
	return unsafe_as_arr__arr__char__ptr_mut_arr__char(a);
}
_void assert___void__bool(ctx* ctx, bool condition) {
	if (condition) {
		return 0;
	} else {
		return fail___void_arg0_is_4661506(ctx);
	}
}
bool greaterequal__bool__nat64__nat64(nat64 a, nat64 b) {
	return !(less__bool__nat64__nat64(a, b));
}
bool lessequal__bool__nat64__nat64(nat64 a, nat64 b) {
	return !(less__bool__nat64__nat64(b, a));
}
ptr__char uninitialized_data__ptr__char__nat64(ctx* ctx, nat64 size) {
	ptr__byte bptr = allocate_bytes__ptr__byte__nat64(ctx, size * 1);
	return reinterpret_cast<char*>(bptr);
}
_void cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, cat__arr__char__arr__char__arr__char__lambda0___closure _closure, nat64 i) {
	return set_at___void__ptr_mut_arr__char__nat64__char(ctx, _closure.m, i, at__char__arr__char__nat64(ctx, _closure.a, i));
}
_void cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, cat__arr__char__arr__char__arr__char__lambda1___closure _closure, nat64 i) {
	return set_at___void__ptr_mut_arr__char__nat64__char(ctx, _closure.m, plus__nat64__nat64__nat64(ctx, i, _closure.a.size), at__char__arr__char__nat64(ctx, _closure.b, i));
}
arr__char unsafe_as_arr__arr__char__ptr_mut_arr__char(mut_arr__char* a) {
	return arr__char{a->size, a->data};
}
_void fail___void_arg0_is_4661506(ctx* ctx) {
	return todo___void();
}
bool less__bool__nat64__nat64(nat64 a, nat64 b) {
	comparison matched = lessequalgreater__comparison__nat64__nat64(a, b);
	switch (matched.kind) {
		case comparison::Kind::less: {
			return true;
		}
		case comparison::Kind::equal: {
			return false;
		}
		case comparison::Kind::greater: {
			return false;
		}
		default: assert(0);
	}
}
_void set_at___void__ptr_mut_arr__char__nat64__char(ctx* ctx, mut_arr__char* a, nat64 index, char value) {
	assert___void__bool(ctx, less__bool__nat64__nat64(index, a->size));
	return [&]() { *(a->data + index) = value; return 0; }();
}
char at__char__arr__char__nat64(ctx* ctx, arr__char a, nat64 index) {
	assert___void__bool(ctx, less__bool__nat64__nat64(index, a.size));
	return *(a.data + index);
}
_void todo___void() {
	return hard_fail___void_arg0_is_4661507();
}
_void hard_fail___void_arg0_is_4661507() {
	assert(0);
}


int main() { return (int) main__int64(); }
