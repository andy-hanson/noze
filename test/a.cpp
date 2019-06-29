#include <cassert>
#include <cstdint>
using int64 = int64_t;
struct ctx;
using nat64 = uint64_t;
using _void = uint8_t;
using byte = uint8_t;
struct arr__char;
struct stream__nat64;
struct fun0__iterator__nat64;
struct iterator__nat64;
struct fun0__opt__nat64;
struct opt__nat64;
struct none {};
struct some__nat64 {
	nat64 value;
};
struct to_stream__stream__nat64__iter_arg0_is_0__lambda0___closure;
struct range_nat64 {
	nat64 lo;
	nat64 hi;
};
struct filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0___closure;
struct comparison;
struct less {};
struct greater {};
struct equal {};
struct cell__nat64 {
	nat64 value;
};
struct filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0___closure;
struct mut_arr__char;
struct cat__arr__char__arr__char__arr__char__lambda0___closure;
struct cat__arr__char__arr__char__arr__char__lambda1___closure;
struct iter__iterator__nat64__range_nat64__lambda0___closure {
	cell__nat64* n;
	range_nat64 r;
};
struct ctx {
	nat64 a;
	nat64 b;
	nat64 c;
};
using ptr__byte = byte*;
using ptr__char = char*;
struct opt__nat64 {
	enum class Kind {
		none,
		some__nat64,
	};
	Kind kind;
	union {
		none as_none;
		some__nat64 as_some__nat64;
	};
	opt__nat64(none value) : kind{Kind::none}, as_none{value} {}
	opt__nat64(some__nat64 value) : kind{Kind::some__nat64}, as_some__nat64{value} {}
};
struct to_stream__stream__nat64__iter_arg0_is_0__lambda0___closure {
	range_nat64 t;
};
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
using fun_ptr2__opt__nat64__ptr_ctx__ptr__byte = opt__nat64 (*)(ctx*, ptr__byte);
struct cat__arr__char__arr__char__arr__char__lambda0___closure {
	mut_arr__char* m;
	arr__char a;
};
struct cat__arr__char__arr__char__arr__char__lambda1___closure {
	mut_arr__char* m;
	arr__char a;
	arr__char b;
};
struct fun0__opt__nat64 {
	fun_ptr2__opt__nat64__ptr_ctx__ptr__byte fun_ptr;
	ptr__byte closure;
};
struct iterator__nat64 {
	fun0__opt__nat64 get_next;
};
struct filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0___closure {
	iterator__nat64 itr;
};
using fun_ptr2__iterator__nat64__ptr_ctx__ptr__byte = iterator__nat64 (*)(ctx*, ptr__byte);
struct fun0__iterator__nat64 {
	fun_ptr2__iterator__nat64__ptr_ctx__ptr__byte fun_ptr;
	ptr__byte closure;
};
struct stream__nat64 {
	fun0__iterator__nat64 get_iter;
};
struct filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0___closure {
	stream__nat64 in;
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
static_assert(sizeof(stream__nat64) == 16, "");
static_assert(sizeof(fun0__iterator__nat64) == 16, "");
static_assert(sizeof(fun_ptr2__iterator__nat64__ptr_ctx__ptr__byte) == 8, "");
static_assert(sizeof(iterator__nat64) == 16, "");
static_assert(sizeof(fun0__opt__nat64) == 16, "");
static_assert(sizeof(fun_ptr2__opt__nat64__ptr_ctx__ptr__byte) == 8, "");
static_assert(sizeof(opt__nat64) == 16, "");
static_assert(sizeof(none) == 1, "");
static_assert(sizeof(some__nat64) == 8, "");
static_assert(sizeof(bool) == 1, "");
static_assert(sizeof(to_stream__stream__nat64__iter_arg0_is_0__lambda0___closure) == 16, "");
static_assert(sizeof(range_nat64) == 16, "");
static_assert(sizeof(filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0___closure) == 16, "");
static_assert(sizeof(comparison) == 8, "");
static_assert(sizeof(less) == 1, "");
static_assert(sizeof(greater) == 1, "");
static_assert(sizeof(equal) == 1, "");
static_assert(sizeof(cell__nat64) == 8, "");
static_assert(sizeof(filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0___closure) == 16, "");
static_assert(sizeof(mut_arr__char) == 32, "");
static_assert(sizeof(cat__arr__char__arr__char__arr__char__lambda0___closure) == 24, "");
static_assert(sizeof(cat__arr__char__arr__char__arr__char__lambda1___closure) == 40, "");
static_assert(sizeof(iter__iterator__nat64__range_nat64__lambda0___closure) == 24, "");

static arr__char _constantArr4665367 = arr__char{14, const_cast<char*>("hello ya world")};
static arr__char _constantArr4665368 = arr__char{6, const_cast<char*>("%.*s\n\0")};
static range_nat64 _constant____range_nat64__0 = range_nat64{0, 1000};
static arr__char _constantArr4665370 = arr__char{1, const_cast<char*>("0")};
static arr__char _constantArr4665371 = arr__char{1, const_cast<char*>("1")};
static arr__char _constantArr4665373 = arr__char{1, const_cast<char*>("2")};
static arr__char _constantArr4665372 = arr__char{1, const_cast<char*>("3")};
static arr__char _constantArr4665377 = arr__char{1, const_cast<char*>("4")};
static arr__char _constantArr4665376 = arr__char{1, const_cast<char*>("5")};
static arr__char _constantArr4665379 = arr__char{1, const_cast<char*>("6")};
static arr__char _constantArr4665380 = arr__char{1, const_cast<char*>("7")};
static arr__char _constantArr4665381 = arr__char{1, const_cast<char*>("8")};
static arr__char _constantArr4665382 = arr__char{1, const_cast<char*>("9")};
static none _constant____none__0 = none{};
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
_void call_with_ctx___void__ptr_ctx_arg1_is_4216033(ctx* c);
_void call___void__fun_ptr2___void__ptr_ctx__ptr__byte__ptr_ctx__ptr__byte(fun_ptr2___void__ptr_ctx__ptr__byte f, ctx* p0, ptr__byte p1);
_void other_main___void__asLambda__dynamic(ctx* ctx, ptr__byte );
_void other_main___void(ctx* ctx);
_void print_sync___void__arr__char(arr__char s);
stream__nat64 to_stream__stream__nat64__iter_arg0_is_0(ctx* ctx);
stream__nat64 filter__stream__nat64__stream__nat64_arg1_is_4216032(ctx* ctx, stream__nat64 in);
nat64 sum__nat64__stream__nat64__iter(ctx* ctx, stream__nat64 t);
_void assert___void__bool(ctx* ctx, bool condition);
bool equalequal__bool__nat64__nat64(nat64 a, nat64 b);
arr__char to_str__arr__char__nat64(ctx* ctx, nat64 n);
extern "C" _void printf(ptr__char format, nat64 sz, ptr__char data);
iterator__nat64 to_stream__stream__nat64__iter_arg0_is_0__lambda0__dynamic(ctx* ctx, to_stream__stream__nat64__iter_arg0_is_0__lambda0___closure* _closure);
ptr__byte allocate_bytes__ptr__byte__nat64(ctx* ctx, nat64 size);
iterator__nat64 filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic(ctx* ctx, filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0___closure* _closure);
nat64 sum_helper__nat64__iterator__nat64(ctx* ctx, iterator__nat64 i);
iterator__nat64 iter__iterator__nat64__stream__nat64(ctx* ctx, stream__nat64 s);
_void fail___void_arg0_is_4665374(ctx* ctx);
comparison lessequalgreater__comparison__nat64__nat64(nat64 a, nat64 b);
nat64 div__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
nat64 mod__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
arr__char cat__arr__char__arr__char__arr__char(ctx* ctx, arr__char a, arr__char b);
iterator__nat64 iter__iterator__nat64__range_nat64(ctx* ctx, range_nat64 r);
extern "C" ptr__byte malloc(nat64 size);
opt__nat64 filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0__dynamic(ctx* ctx, filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0___closure* _closure);
opt__nat64 next__opt__nat64__iterator__nat64(ctx* ctx, iterator__nat64 i);
nat64 plus__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
iterator__nat64 call__iterator__nat64__fun0__iterator__nat64(ctx* ctx, fun0__iterator__nat64 f);
_void todo___void();
_void forbid___void__bool(ctx* ctx, bool condition);
nat64 unsafe_div__nat64__nat64__nat64(nat64 a, nat64 b);
nat64 unsafe_mod__nat64__nat64__nat64(nat64 a, nat64 b);
mut_arr__char* uninitialized_mut_arr__ptr_mut_arr__char__nat64(ctx* ctx, nat64 size);
_void each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, range_nat64 r, cat__arr__char__arr__char__arr__char__lambda0___closure f);
range_nat64 to__range_nat64__nat64__nat64(ctx* ctx, nat64 lo, nat64 hi);
_void each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, range_nat64 r, cat__arr__char__arr__char__arr__char__lambda1___closure f);
arr__char freeze__arr__char__ptr_mut_arr__char(mut_arr__char* a);
opt__nat64 iter__iterator__nat64__range_nat64__lambda0__dynamic(ctx* ctx, iter__iterator__nat64__range_nat64__lambda0___closure* _closure);
opt__nat64 filter_helper__opt__nat64__iterator__nat64_arg1_is_4216032(ctx* ctx, iterator__nat64 i);
opt__nat64 call__opt__nat64__fun0__opt__nat64(ctx* ctx, fun0__opt__nat64 f);
nat64 wrapping_add__nat64__nat64__nat64(nat64 a, nat64 b);
bool and__bool__bool__bool(bool a, bool b);
bool greaterequal__bool__nat64__nat64(nat64 a, nat64 b);
iterator__nat64 call_with_ctx__iterator__nat64__ptr_ctx__fun0__iterator__nat64(ctx* c, fun0__iterator__nat64 f);
ctx* get_ctx__ptr_ctx(ctx* ctx);
_void hard_fail___void_arg0_is_4665375();
bool not__bool__bool(bool a);
ptr__char uninitialized_data__ptr__char__nat64(ctx* ctx, nat64 size);
_void each_recur___void__iterator__nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, iterator__nat64 i, cat__arr__char__arr__char__arr__char__lambda0___closure f);
bool lessequal__bool__nat64__nat64(nat64 a, nat64 b);
_void each_recur___void__iterator__nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, iterator__nat64 i, cat__arr__char__arr__char__arr__char__lambda1___closure f);
arr__char unsafe_as_arr__arr__char__ptr_mut_arr__char(mut_arr__char* a);
nat64 get__nat64__ptr_cell__nat64(ctx* ctx, cell__nat64* c);
nat64 get_and_incr__nat64__ptr_cell__nat64(ctx* ctx, cell__nat64* c);
bool other_main___void__lambda0(ctx* ctx, nat64 it);
opt__nat64 call_with_ctx__opt__nat64__ptr_ctx__fun0__opt__nat64(ctx* c, fun0__opt__nat64 f);
bool less__bool__nat64__nat64(nat64 a, nat64 b);
iterator__nat64 call__iterator__nat64__fun_ptr2__iterator__nat64__ptr_ctx__ptr__byte__ptr_ctx__ptr__byte(fun_ptr2__iterator__nat64__ptr_ctx__ptr__byte f, ctx* p0, ptr__byte p1);
nat64 wrapping_mul__nat64__nat64__nat64(nat64 a, nat64 b);
ptr__char ptr_cast__ptr__char__ptr__byte(ptr__byte p);
_void cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, cat__arr__char__arr__char__arr__char__lambda0___closure _closure, nat64 i);
_void cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, cat__arr__char__arr__char__arr__char__lambda1___closure _closure, nat64 i);
_void set___void__ptr_cell__nat64__nat64(ctx* ctx, cell__nat64* c, nat64 v);
nat64 incr__nat64__nat64(nat64 n);
bool or__bool__bool__bool(bool a, bool b);
bool is_multiple__q__bool__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
opt__nat64 call__opt__nat64__fun_ptr2__opt__nat64__ptr_ctx__ptr__byte__ptr_ctx__ptr__byte(fun_ptr2__opt__nat64__ptr_ctx__ptr__byte f, ctx* p0, ptr__byte p1);
_void set_at___void__ptr_mut_arr__char__nat64__char(ctx* ctx, mut_arr__char* a, nat64 index, char value);
char at__char__arr__char__nat64(ctx* ctx, arr__char a, nat64 index);
_void set___void__ptr__char__char(ptr__char p, char value);
ptr__char plus__ptr__char__ptr__char__nat64(ptr__char p, nat64 offset);
char deref__char__ptr__char(ptr__char p);
int64 main__int64() {
	ctx ctx_by_val = ctx{0, 0, 0};
	ctx* ctx_by_ptr = &(ctx_by_val);
	call_with_ctx___void__ptr_ctx_arg1_is_4216033(ctx_by_ptr);
	return 0;
}
_void call_with_ctx___void__ptr_ctx_arg1_is_4216033(ctx* c) {
	return (&other_main___void__asLambda__dynamic)(c, 0);
}
_void other_main___void__asLambda__dynamic(ctx* ctx, ptr__byte ) {
	return other_main___void(ctx);
}
_void other_main___void(ctx* ctx) {
	print_sync___void__arr__char(_constantArr4665367);
	stream__nat64 rgs = to_stream__stream__nat64__iter_arg0_is_0(ctx);
	stream__nat64 filtered = filter__stream__nat64__stream__nat64_arg1_is_4216032(ctx, rgs);
	nat64 x = sum__nat64__stream__nat64__iter(ctx, filtered);
	assert___void__bool(ctx, equalequal__bool__nat64__nat64(x, 233168));
	print_sync___void__arr__char(to_str__arr__char__nat64(ctx, x));
	
	return 0;
}
_void print_sync___void__arr__char(arr__char s) {
	return printf(&_constantArr4665368.data[0], s.size, s.data);
}
stream__nat64 to_stream__stream__nat64__iter_arg0_is_0(ctx* ctx) {
	return stream__nat64{fun0__iterator__nat64{
		reinterpret_cast<fun_ptr2__iterator__nat64__ptr_ctx__ptr__byte>(to_stream__stream__nat64__iter_arg0_is_0__lambda0__dynamic), reinterpret_cast<ptr__byte>(_alloc<to_stream__stream__nat64__iter_arg0_is_0__lambda0___closure>(allocate_bytes__ptr__byte__nat64(ctx, 16), to_stream__stream__nat64__iter_arg0_is_0__lambda0___closure{_constant____range_nat64__0}))}};
}
stream__nat64 filter__stream__nat64__stream__nat64_arg1_is_4216032(ctx* ctx, stream__nat64 in) {
	return stream__nat64{fun0__iterator__nat64{
		reinterpret_cast<fun_ptr2__iterator__nat64__ptr_ctx__ptr__byte>(filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic), reinterpret_cast<ptr__byte>(_alloc<filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0___closure>(allocate_bytes__ptr__byte__nat64(ctx, 16), filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0___closure{in}))}};
}
nat64 sum__nat64__stream__nat64__iter(ctx* ctx, stream__nat64 t) {
	return sum_helper__nat64__iterator__nat64(ctx, iter__iterator__nat64__stream__nat64(ctx, t));
}
_void assert___void__bool(ctx* ctx, bool condition) {
	if (condition) {
		return 0;
	} else {
		return fail___void_arg0_is_4665374(ctx);
	}
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
arr__char to_str__arr__char__nat64(ctx* ctx, nat64 n) {
	if (equalequal__bool__nat64__nat64(n, 0)) {
		return _constantArr4665370;
	} else if (equalequal__bool__nat64__nat64(n, 1)) {
		return _constantArr4665371;
	} else if (equalequal__bool__nat64__nat64(n, 2)) {
		return _constantArr4665373;
	} else if (equalequal__bool__nat64__nat64(n, 3)) {
		return _constantArr4665372;
	} else if (equalequal__bool__nat64__nat64(n, 4)) {
		return _constantArr4665377;
	} else if (equalequal__bool__nat64__nat64(n, 5)) {
		return _constantArr4665376;
	} else if (equalequal__bool__nat64__nat64(n, 6)) {
		return _constantArr4665379;
	} else if (equalequal__bool__nat64__nat64(n, 7)) {
		return _constantArr4665380;
	} else if (equalequal__bool__nat64__nat64(n, 8)) {
		return _constantArr4665381;
	} else if (equalequal__bool__nat64__nat64(n, 9)) {
		return _constantArr4665382;
	} else {
		arr__char hi = to_str__arr__char__nat64(ctx, div__nat64__nat64__nat64(ctx, n, 10));
		arr__char lo = to_str__arr__char__nat64(ctx, mod__nat64__nat64__nat64(ctx, n, 10));
		return cat__arr__char__arr__char__arr__char(ctx, hi, lo);
	}
}
iterator__nat64 to_stream__stream__nat64__iter_arg0_is_0__lambda0__dynamic(ctx* ctx, to_stream__stream__nat64__iter_arg0_is_0__lambda0___closure* _closure) {
	return iter__iterator__nat64__range_nat64(ctx, _closure->t);
}
ptr__byte allocate_bytes__ptr__byte__nat64(ctx* ctx, nat64 size) {
	return malloc(size);
}
iterator__nat64 filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic(ctx* ctx, filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0___closure* _closure) {
	iterator__nat64 itr = iter__iterator__nat64__stream__nat64(ctx, _closure->in);
	return iterator__nat64{fun0__opt__nat64{
		reinterpret_cast<fun_ptr2__opt__nat64__ptr_ctx__ptr__byte>(filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0__dynamic), reinterpret_cast<ptr__byte>(_alloc<filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0___closure>(allocate_bytes__ptr__byte__nat64(ctx, 16), filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0___closure{itr}))}};
}
nat64 sum_helper__nat64__iterator__nat64(ctx* ctx, iterator__nat64 i) {
	opt__nat64 matched = next__opt__nat64__iterator__nat64(ctx, i);
	switch (matched.kind) {
		case opt__nat64::Kind::none: {
			return 0;
		}
		case opt__nat64::Kind::some__nat64: {
			some__nat64 s = matched.as_some__nat64;
			return plus__nat64__nat64__nat64(ctx, s.value, sum_helper__nat64__iterator__nat64(ctx, i));
		}
		default: assert(0);
	}
}
iterator__nat64 iter__iterator__nat64__stream__nat64(ctx* ctx, stream__nat64 s) {
	return call__iterator__nat64__fun0__iterator__nat64(ctx, s.get_iter);
}
_void fail___void_arg0_is_4665374(ctx* ctx) {
	return todo___void();
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
iterator__nat64 iter__iterator__nat64__range_nat64(ctx* ctx, range_nat64 r) {
	cell__nat64* n = _alloc<cell__nat64>(allocate_bytes__ptr__byte__nat64(ctx, 8), cell__nat64{r.lo});
	return iterator__nat64{fun0__opt__nat64{
		reinterpret_cast<fun_ptr2__opt__nat64__ptr_ctx__ptr__byte>(iter__iterator__nat64__range_nat64__lambda0__dynamic), reinterpret_cast<ptr__byte>(_alloc<iter__iterator__nat64__range_nat64__lambda0___closure>(allocate_bytes__ptr__byte__nat64(ctx, 24), iter__iterator__nat64__range_nat64__lambda0___closure{n, r}))}};
}
opt__nat64 filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0__dynamic(ctx* ctx, filter__stream__nat64__stream__nat64_arg1_is_4216032__lambda0__dynamic__lambda0___closure* _closure) {
	return filter_helper__opt__nat64__iterator__nat64_arg1_is_4216032(ctx, _closure->itr);
}
opt__nat64 next__opt__nat64__iterator__nat64(ctx* ctx, iterator__nat64 i) {
	return call__opt__nat64__fun0__opt__nat64(ctx, i.get_next);
}
nat64 plus__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b) {
	nat64 res = a + b;
	assert___void__bool(ctx, greaterequal__bool__nat64__nat64(res, a) && greaterequal__bool__nat64__nat64(res, b));
	return res;
}
iterator__nat64 call__iterator__nat64__fun0__iterator__nat64(ctx* ctx, fun0__iterator__nat64 f) {
	return call_with_ctx__iterator__nat64__ptr_ctx__fun0__iterator__nat64(ctx, f);
}
_void todo___void() {
	return hard_fail___void_arg0_is_4665375();
}
_void forbid___void__bool(ctx* ctx, bool condition) {
	return assert___void__bool(ctx, !(condition));
}
mut_arr__char* uninitialized_mut_arr__ptr_mut_arr__char__nat64(ctx* ctx, nat64 size) {
	return _alloc<mut_arr__char>(allocate_bytes__ptr__byte__nat64(ctx, 32), mut_arr__char{false, size, size, uninitialized_data__ptr__char__nat64(ctx, size)});
}
_void each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, range_nat64 r, cat__arr__char__arr__char__arr__char__lambda0___closure f) {
	return each_recur___void__iterator__nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx, iter__iterator__nat64__range_nat64(ctx, r), f);
}
range_nat64 to__range_nat64__nat64__nat64(ctx* ctx, nat64 lo, nat64 hi) {
	assert___void__bool(ctx, lessequal__bool__nat64__nat64(lo, hi));
	return range_nat64{lo, hi};
}
_void each___void__range_nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, range_nat64 r, cat__arr__char__arr__char__arr__char__lambda1___closure f) {
	return each_recur___void__iterator__nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx, iter__iterator__nat64__range_nat64(ctx, r), f);
}
arr__char freeze__arr__char__ptr_mut_arr__char(mut_arr__char* a) {
	a->frozen = true;
	return unsafe_as_arr__arr__char__ptr_mut_arr__char(a);
}
opt__nat64 iter__iterator__nat64__range_nat64__lambda0__dynamic(ctx* ctx, iter__iterator__nat64__range_nat64__lambda0___closure* _closure) {
	if (equalequal__bool__nat64__nat64(get__nat64__ptr_cell__nat64(ctx, _closure->n), _closure->r.hi)) {
		return opt__nat64(_constant____none__0);
	} else {
		return opt__nat64(some__nat64{get_and_incr__nat64__ptr_cell__nat64(ctx, _closure->n)});
	}
}
opt__nat64 filter_helper__opt__nat64__iterator__nat64_arg1_is_4216032(ctx* ctx, iterator__nat64 i) {
	opt__nat64 matched = next__opt__nat64__iterator__nat64(ctx, i);
	switch (matched.kind) {
		case opt__nat64::Kind::none: {
			return opt__nat64(_constant____none__0);
		}
		case opt__nat64::Kind::some__nat64: {
			some__nat64 s = matched.as_some__nat64;
			nat64 v = s.value;
			if (other_main___void__lambda0(ctx, v)) {
				return opt__nat64(some__nat64{v});
			} else {
				return filter_helper__opt__nat64__iterator__nat64_arg1_is_4216032(ctx, i);
			}
		}
		default: assert(0);
	}
}
opt__nat64 call__opt__nat64__fun0__opt__nat64(ctx* ctx, fun0__opt__nat64 f) {
	return call_with_ctx__opt__nat64__ptr_ctx__fun0__opt__nat64(ctx, f);
}
bool greaterequal__bool__nat64__nat64(nat64 a, nat64 b) {
	return !(less__bool__nat64__nat64(a, b));
}
iterator__nat64 call_with_ctx__iterator__nat64__ptr_ctx__fun0__iterator__nat64(ctx* c, fun0__iterator__nat64 f) {
	return f.fun_ptr(c, f.closure);
}
_void hard_fail___void_arg0_is_4665375() {
	assert(0);
}
ptr__char uninitialized_data__ptr__char__nat64(ctx* ctx, nat64 size) {
	ptr__byte bptr = allocate_bytes__ptr__byte__nat64(ctx, size * 1);
	return reinterpret_cast<char*>(bptr);
}
_void each_recur___void__iterator__nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, iterator__nat64 i, cat__arr__char__arr__char__arr__char__lambda0___closure f) {
	opt__nat64 matched = next__opt__nat64__iterator__nat64(ctx, i);
	switch (matched.kind) {
		case opt__nat64::Kind::none: {
			return 0;
		}
		case opt__nat64::Kind::some__nat64: {
			some__nat64 s = matched.as_some__nat64;
			cat__arr__char__arr__char__arr__char__lambda0(ctx, f, s.value);
			return each_recur___void__iterator__nat64__cat__arr__char__arr__char__arr__char__lambda0___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda0(ctx, i, f);
		}
		default: assert(0);
	}
}
bool lessequal__bool__nat64__nat64(nat64 a, nat64 b) {
	return !(less__bool__nat64__nat64(b, a));
}
_void each_recur___void__iterator__nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, iterator__nat64 i, cat__arr__char__arr__char__arr__char__lambda1___closure f) {
	opt__nat64 matched = next__opt__nat64__iterator__nat64(ctx, i);
	switch (matched.kind) {
		case opt__nat64::Kind::none: {
			return 0;
		}
		case opt__nat64::Kind::some__nat64: {
			some__nat64 s = matched.as_some__nat64;
			cat__arr__char__arr__char__arr__char__lambda1(ctx, f, s.value);
			return each_recur___void__iterator__nat64__cat__arr__char__arr__char__arr__char__lambda1___closure_klb1_is_cat__arr__char__arr__char__arr__char__lambda1(ctx, i, f);
		}
		default: assert(0);
	}
}
arr__char unsafe_as_arr__arr__char__ptr_mut_arr__char(mut_arr__char* a) {
	return arr__char{a->size, a->data};
}
nat64 get__nat64__ptr_cell__nat64(ctx* ctx, cell__nat64* c) {
	return c->value;
}
nat64 get_and_incr__nat64__ptr_cell__nat64(ctx* ctx, cell__nat64* c) {
	nat64 res = get__nat64__ptr_cell__nat64(ctx, c);
	set___void__ptr_cell__nat64__nat64(ctx, c, incr__nat64__nat64(res));
	return res;
}
bool other_main___void__lambda0(ctx* ctx, nat64 it) {
	return is_multiple__q__bool__nat64__nat64(ctx, it, 3) || is_multiple__q__bool__nat64__nat64(ctx, it, 5);
}
opt__nat64 call_with_ctx__opt__nat64__ptr_ctx__fun0__opt__nat64(ctx* c, fun0__opt__nat64 f) {
	return f.fun_ptr(c, f.closure);
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
_void cat__arr__char__arr__char__arr__char__lambda0(ctx* ctx, cat__arr__char__arr__char__arr__char__lambda0___closure _closure, nat64 i) {
	return set_at___void__ptr_mut_arr__char__nat64__char(ctx, _closure.m, i, at__char__arr__char__nat64(ctx, _closure.a, i));
}
_void cat__arr__char__arr__char__arr__char__lambda1(ctx* ctx, cat__arr__char__arr__char__arr__char__lambda1___closure _closure, nat64 i) {
	return set_at___void__ptr_mut_arr__char__nat64__char(ctx, _closure.m, plus__nat64__nat64__nat64(ctx, i, _closure.a.size), at__char__arr__char__nat64(ctx, _closure.b, i));
}
_void set___void__ptr_cell__nat64__nat64(ctx* ctx, cell__nat64* c, nat64 v) {
	c->value = v;
	return 0;
}
nat64 incr__nat64__nat64(nat64 n) {
	return n + 1;
}
bool is_multiple__q__bool__nat64__nat64(ctx* ctx, nat64 a, nat64 b) {
	return equalequal__bool__nat64__nat64(mod__nat64__nat64__nat64(ctx, a, b), 0);
}
_void set_at___void__ptr_mut_arr__char__nat64__char(ctx* ctx, mut_arr__char* a, nat64 index, char value) {
	assert___void__bool(ctx, less__bool__nat64__nat64(index, a->size));
	return [&]() { *(a->data + index) = value; return 0; }();
}
char at__char__arr__char__nat64(ctx* ctx, arr__char a, nat64 index) {
	assert___void__bool(ctx, less__bool__nat64__nat64(index, a.size));
	return *(a.data + index);
}


int main() { return (int) main__int64(); }
