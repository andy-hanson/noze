#include <cassert>
#include <cstdint>
using int64 = int64_t;
struct ctx;
using nat64 = uint64_t;
using _void = uint8_t;
using byte = uint8_t;
struct mut_dict__nat64__nat64;
struct mut_arr__nat64;
using ptr__nat64 = nat64*;
struct dict__nat64__nat64;
struct arr__nat64 {
	nat64 size;
	ptr__nat64 data;
};
struct fun1__nat64__nat64;
struct other_main___void__lambda1___closure {
	nat64 uno;
};
struct other_main___void__lambda2___closure {
	nat64 uno;
	nat64 dos;
};
struct arr__char;
struct opt__nat64;
struct none {};
struct some__nat64 {
	nat64 value;
};
struct range_nat64 {
	nat64 lo;
	nat64 hi;
};
struct map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure;
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
struct mut_dict__nat64__nat64 {
	mut_arr__nat64* keys;
	mut_arr__nat64* values;
};
struct mut_arr__nat64 {
	bool frozen;
	nat64 size;
	nat64 capacity;
	ptr__nat64 data;
};
struct dict__nat64__nat64 {
	arr__nat64 keys;
	arr__nat64 values;
};
using fun_ptr3__nat64__ptr_ctx__ptr__byte__nat64 = nat64 (*)(ctx*, ptr__byte, nat64);
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
struct fun1__nat64__nat64 {
	fun_ptr3__nat64__ptr_ctx__ptr__byte__nat64 fun_ptr;
	ptr__byte closure;
};
struct arr__char {
	nat64 size;
	ptr__char data;
};
struct map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure {
	mut_arr__nat64* out;
	fun1__nat64__nat64 mapper;
	arr__nat64 a;
};

static_assert(sizeof(int64) == 8, "");
static_assert(sizeof(ctx) == 24, "");
static_assert(sizeof(nat64) == 8, "");
static_assert(sizeof(_void) == 1, "");
static_assert(sizeof(fun_ptr2___void__ptr_ctx__ptr__byte) == 8, "");
static_assert(sizeof(ptr__byte) == 8, "");
static_assert(sizeof(byte) == 1, "");
static_assert(sizeof(mut_dict__nat64__nat64) == 16, "");
static_assert(sizeof(mut_arr__nat64) == 32, "");
static_assert(sizeof(bool) == 1, "");
static_assert(sizeof(ptr__nat64) == 8, "");
static_assert(sizeof(dict__nat64__nat64) == 32, "");
static_assert(sizeof(arr__nat64) == 16, "");
static_assert(sizeof(fun1__nat64__nat64) == 16, "");
static_assert(sizeof(fun_ptr3__nat64__ptr_ctx__ptr__byte__nat64) == 8, "");
static_assert(sizeof(other_main___void__lambda1___closure) == 8, "");
static_assert(sizeof(other_main___void__lambda2___closure) == 16, "");
static_assert(sizeof(arr__char) == 16, "");
static_assert(sizeof(ptr__char) == 8, "");
static_assert(sizeof(char) == 1, "");
static_assert(sizeof(opt__nat64) == 16, "");
static_assert(sizeof(none) == 1, "");
static_assert(sizeof(some__nat64) == 8, "");
static_assert(sizeof(range_nat64) == 16, "");
static_assert(sizeof(map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure) == 40, "");
static_assert(sizeof(comparison) == 8, "");
static_assert(sizeof(less) == 1, "");
static_assert(sizeof(greater) == 1, "");
static_assert(sizeof(equal) == 1, "");

static arr__char _constantArr4659043 = arr__char{6, const_cast<char*>("%.*s\n\0")};
static arr__char _constantArr4659031 = arr__char{1, const_cast<char*>("0")};
static arr__char _constantArr4659032 = arr__char{1, const_cast<char*>("1")};
static arr__char _constantArr4659036 = arr__char{1, const_cast<char*>("2")};
static arr__char _constantArr4659037 = arr__char{1, const_cast<char*>("3")};
static arr__char _constantArr4659035 = arr__char{1, const_cast<char*>("4")};
static arr__char _constantArr4659038 = arr__char{1, const_cast<char*>("5")};
static arr__char _constantArr4659039 = arr__char{1, const_cast<char*>("6")};
static arr__char _constantArr4659040 = arr__char{1, const_cast<char*>("7")};
static arr__char _constantArr4659041 = arr__char{1, const_cast<char*>("8")};
static arr__char _constantArr4659042 = arr__char{1, const_cast<char*>("9")};
static none _constant____none__0 = none();
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
_void call_with_ctx___void__ptr_ctx_arg1_is_140720922906881(ctx* c);
_void call___void__fun_ptr2___void__ptr_ctx__ptr__byte__ptr_ctx__ptr__byte(fun_ptr2___void__ptr_ctx__ptr__byte f, ctx* p0, ptr__byte p1);
_void other_main___void__asLambda__dynamic(ctx* ctx, ptr__byte );
_void other_main___void(ctx* ctx);
mut_dict__nat64__nat64 empty_mut_dict__mut_dict__nat64__nat64(ctx* ctx);
_void add___void__mut_dict__nat64__nat64__nat64__nat64(ctx* ctx, mut_dict__nat64__nat64 m, nat64 key, nat64 value);
dict__nat64__nat64* freeze__ptr_dict__nat64__nat64__mut_dict__nat64__nat64(ctx* ctx, mut_dict__nat64__nat64 m);
fun1__nat64__nat64 as_non_const__fun1__nat64__nat64__fun1__nat64__nat64(fun1__nat64__nat64 value);
nat64 other_main___void__lambda0__dynamic(ctx* ctx, ptr__byte , nat64 it);
nat64 plus__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
nat64 other_main___void__lambda1__dynamic(ctx* ctx, other_main___void__lambda1___closure* _closure, nat64 it);
ptr__byte allocate_bytes__ptr__byte__nat64(ctx* ctx, nat64 size);
nat64 other_main___void__lambda2__dynamic(ctx* ctx, other_main___void__lambda2___closure* _closure, nat64 it);
dict__nat64__nat64* map_values__ptr_dict__nat64__nat64__ptr_dict__nat64__nat64__fun1__nat64__nat64(ctx* ctx, dict__nat64__nat64* d, fun1__nat64__nat64 mapper);
_void print_sync___void__arr__char(arr__char s);
arr__char to_str__arr__char__nat64(ctx* ctx, nat64 n);
nat64 at__nat64__ptr_dict__nat64__nat64__nat64(ctx* ctx, dict__nat64__nat64* d, nat64 key);
mut_arr__nat64* empty_mut_arr__ptr_mut_arr__nat64(ctx* ctx);
_void forbid___void__bool(ctx* ctx, bool condition);
bool has__q__bool__mut_dict__nat64__nat64__nat64(ctx* ctx, mut_dict__nat64__nat64 d, nat64 key);
_void add___void__ptr_mut_arr__nat64__nat64(ctx* ctx, mut_arr__nat64* a, nat64 value);
arr__nat64 freeze__arr__nat64__ptr_mut_arr__nat64(mut_arr__nat64* a);
nat64 wrapping_add__nat64__nat64__nat64(nat64 a, nat64 b);
extern "C" ptr__byte malloc(nat64 size);
arr__nat64 map__arr__nat64__arr__nat64__fun1__nat64__nat64(ctx* ctx, arr__nat64 a, fun1__nat64__nat64 mapper);
extern "C" _void printf(ptr__char format, nat64 sz, ptr__char data);
bool equalequal__bool__nat64__nat64(nat64 a, nat64 b);
arr__char todo__arr__char();
opt__nat64 get__opt__nat64__ptr_dict__nat64__nat64__nat64(ctx* ctx, dict__nat64__nat64* d, nat64 key);
nat64 todo__nat64();
mut_arr__nat64* uninitialized_mut_arr__ptr_mut_arr__nat64_arg0_is_0(ctx* ctx);
_void assert___void__bool(ctx* ctx, bool condition);
bool not__bool__bool(bool a);
bool has__q__bool__ptr_dict__nat64__nat64__nat64(ctx* ctx, dict__nat64__nat64* d, nat64 key);
dict__nat64__nat64* unsafe_as_dict__ptr_dict__nat64__nat64__mut_dict__nat64__nat64(ctx* ctx, mut_dict__nat64__nat64 m);
nat64 if__nat64__bool__nat64__nat64(bool cond, nat64 if_true, nat64 if_false);
nat64 times__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
ptr__nat64 uninitialized_data__ptr__nat64__nat64(ctx* ctx, nat64 size);
_void copy_data_from___void__ptr__nat64__ptr__nat64__nat64(ctx* ctx, ptr__nat64 to, ptr__nat64 from, nat64 len);
bool less__bool__nat64__nat64(nat64 a, nat64 b);
_void set___void__ptr__nat64__nat64(ptr__nat64 p, nat64 value);
ptr__nat64 plus__ptr__nat64__ptr__nat64__nat64(ptr__nat64 p, nat64 offset);
nat64 incr__nat64__nat64(nat64 n);
arr__nat64 unsafe_as_arr__arr__nat64__ptr_mut_arr__nat64(mut_arr__nat64* a);
mut_arr__nat64* uninitialized_mut_arr__ptr_mut_arr__nat64__nat64(ctx* ctx, nat64 size);
_void each___void__range_nat64__map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure_klb1_is_map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0(ctx* ctx, range_nat64 r, map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure f);
range_nat64 to__range_nat64__nat64__nat64(ctx* ctx, nat64 lo, nat64 hi);
comparison lessequalgreater__comparison__nat64__nat64(nat64 a, nat64 b);
arr__char hard_fail__arr__char_arg0_is_4659034();
opt__nat64 get_recursive__opt__nat64__arr__nat64__arr__nat64__nat64__nat64(ctx* ctx, arr__nat64 keys, arr__nat64 values, nat64 idx, nat64 key);
nat64 hard_fail__nat64_arg0_is_4659034();
ptr__nat64 uninitialized_data__ptr__nat64_arg0_is_0(ctx* ctx);
_void fail___void_arg0_is_4659033(ctx* ctx);
bool has__q__bool__opt__nat64(opt__nat64 a);
nat64 wrapping_mul__nat64__nat64__nat64(nat64 a, nat64 b);
ptr__nat64 ptr_cast__ptr__nat64__ptr__byte(ptr__byte p);
nat64 deref__nat64__ptr__nat64(ptr__nat64 p);
ptr__nat64 incr__ptr__nat64__ptr__nat64(ptr__nat64 p);
nat64 decr__nat64__nat64(ctx* ctx, nat64 n);
bool empty__q__bool__range_nat64(ctx* ctx, range_nat64 r);
_void map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0(ctx* ctx, map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure _closure, nat64 i);
range_nat64 tail__range_nat64__range_nat64(ctx* ctx, range_nat64 r);
bool lessequal__bool__nat64__nat64(nat64 a, nat64 b);
nat64 at__nat64__arr__nat64__nat64(ctx* ctx, arr__nat64 a, nat64 index);
ptr__byte allocate_bytes__ptr__byte_arg0_is_0(ctx* ctx);
_void todo___void();
bool empty__q__bool__opt__nat64(opt__nat64 a);
nat64 ___nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b);
_void set_at___void__ptr_mut_arr__nat64__nat64__nat64(ctx* ctx, mut_arr__nat64* a, nat64 index, nat64 value);
nat64 call__nat64__fun1__nat64__nat64__nat64(ctx* ctx, fun1__nat64__nat64 f, nat64 p0);
_void hard_fail___void_arg0_is_4659034();
bool greater__bool__nat64__nat64(nat64 a, nat64 b);
nat64 wrapping_sub__nat64__nat64__nat64(nat64 a, nat64 b);
nat64 call_with_ctx__nat64__ptr_ctx__fun1__nat64__nat64__nat64(ctx* c, fun1__nat64__nat64 f, nat64 p0);
ctx* get_ctx__ptr_ctx(ctx* ctx);
nat64 call__nat64__fun_ptr3__nat64__ptr_ctx__ptr__byte__nat64__ptr_ctx__ptr__byte__nat64(fun_ptr3__nat64__ptr_ctx__ptr__byte__nat64 f, ctx* p0, ptr__byte p1, nat64 p2);
int64 main__int64() {
	ctx ctx_by_val = ctx{0, 0, 0};
	ctx* ctx_by_ptr = &(ctx_by_val);
	call_with_ctx___void__ptr_ctx_arg1_is_140720922906881(ctx_by_ptr);
	return 0;
}
_void call_with_ctx___void__ptr_ctx_arg1_is_140720922906881(ctx* c) {
	return (&other_main___void__asLambda__dynamic)(c, 0);
}
_void other_main___void__asLambda__dynamic(ctx* ctx, ptr__byte ) {
	return other_main___void(ctx);
}
_void other_main___void(ctx* ctx) {
	mut_dict__nat64__nat64 m = empty_mut_dict__mut_dict__nat64__nat64(ctx);
	add___void__mut_dict__nat64__nat64__nat64__nat64(ctx, m, 1, 1);
	add___void__mut_dict__nat64__nat64__nat64__nat64(ctx, m, 2, 2);
	add___void__mut_dict__nat64__nat64__nat64__nat64(ctx, m, 3, 3);
	dict__nat64__nat64* d = freeze__ptr_dict__nat64__nat64__mut_dict__nat64__nat64(ctx, m);
	fun1__nat64__nat64 identity = fun1__nat64__nat64{
		reinterpret_cast<fun_ptr3__nat64__ptr_ctx__ptr__byte__nat64>(other_main___void__lambda0__dynamic), reinterpret_cast<ptr__byte>(0)};
		nat64 uno = 1;
		nat64 dos = plus__nat64__nat64__nat64(ctx, uno, uno);
		fun1__nat64__nat64 add_one = fun1__nat64__nat64{
			reinterpret_cast<fun_ptr3__nat64__ptr_ctx__ptr__byte__nat64>(other_main___void__lambda1__dynamic), reinterpret_cast<ptr__byte>(_alloc<other_main___void__lambda1___closure>(allocate_bytes__ptr__byte__nat64(ctx, 8), other_main___void__lambda1___closure{uno}))};
			fun1__nat64__nat64 add_three = fun1__nat64__nat64{
				reinterpret_cast<fun_ptr3__nat64__ptr_ctx__ptr__byte__nat64>(other_main___void__lambda2__dynamic), reinterpret_cast<ptr__byte>(_alloc<other_main___void__lambda2___closure>(allocate_bytes__ptr__byte__nat64(ctx, 16), other_main___void__lambda2___closure{uno, dos}))};
				dict__nat64__nat64* d2 = map_values__ptr_dict__nat64__nat64__ptr_dict__nat64__nat64__fun1__nat64__nat64(ctx, d, identity);
				dict__nat64__nat64* d3 = map_values__ptr_dict__nat64__nat64__ptr_dict__nat64__nat64__fun1__nat64__nat64(ctx, d, add_one);
				dict__nat64__nat64* d4 = map_values__ptr_dict__nat64__nat64__ptr_dict__nat64__nat64__fun1__nat64__nat64(ctx, d, add_three);
				print_sync___void__arr__char(to_str__arr__char__nat64(ctx, at__nat64__ptr_dict__nat64__nat64__nat64(ctx, d2, 2)));
				print_sync___void__arr__char(to_str__arr__char__nat64(ctx, at__nat64__ptr_dict__nat64__nat64__nat64(ctx, d3, 2)));
				return print_sync___void__arr__char(to_str__arr__char__nat64(ctx, at__nat64__ptr_dict__nat64__nat64__nat64(ctx, d4, 2)));
}
mut_dict__nat64__nat64 empty_mut_dict__mut_dict__nat64__nat64(ctx* ctx) {
	return mut_dict__nat64__nat64{empty_mut_arr__ptr_mut_arr__nat64(ctx), empty_mut_arr__ptr_mut_arr__nat64(ctx)};
}
_void add___void__mut_dict__nat64__nat64__nat64__nat64(ctx* ctx, mut_dict__nat64__nat64 m, nat64 key, nat64 value) {
	forbid___void__bool(ctx, has__q__bool__mut_dict__nat64__nat64__nat64(ctx, m, key));
	add___void__ptr_mut_arr__nat64__nat64(ctx, m.keys, key);
	return add___void__ptr_mut_arr__nat64__nat64(ctx, m.values, value);
}
dict__nat64__nat64* freeze__ptr_dict__nat64__nat64__mut_dict__nat64__nat64(ctx* ctx, mut_dict__nat64__nat64 m) {
	return _alloc<dict__nat64__nat64>(allocate_bytes__ptr__byte__nat64(ctx, 32), dict__nat64__nat64{freeze__arr__nat64__ptr_mut_arr__nat64(m.keys), freeze__arr__nat64__ptr_mut_arr__nat64(m.values)});
}
nat64 other_main___void__lambda0__dynamic(ctx* ctx, ptr__byte , nat64 it) {
	return it;
}
nat64 plus__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b) {
	return a + b;
}
nat64 other_main___void__lambda1__dynamic(ctx* ctx, other_main___void__lambda1___closure* _closure, nat64 it) {
	return plus__nat64__nat64__nat64(ctx, it, _closure->uno);
}
ptr__byte allocate_bytes__ptr__byte__nat64(ctx* ctx, nat64 size) {
	return malloc(size);
}
nat64 other_main___void__lambda2__dynamic(ctx* ctx, other_main___void__lambda2___closure* _closure, nat64 it) {
	return plus__nat64__nat64__nat64(ctx, plus__nat64__nat64__nat64(ctx, it, _closure->uno), _closure->dos);
}
dict__nat64__nat64* map_values__ptr_dict__nat64__nat64__ptr_dict__nat64__nat64__fun1__nat64__nat64(ctx* ctx, dict__nat64__nat64* d, fun1__nat64__nat64 mapper) {
	arr__nat64 new_values = map__arr__nat64__arr__nat64__fun1__nat64__nat64(ctx, d->values, mapper);
	return _alloc<dict__nat64__nat64>(allocate_bytes__ptr__byte__nat64(ctx, 32), dict__nat64__nat64{d->keys, new_values});
}
_void print_sync___void__arr__char(arr__char s) {
	return printf(&_constantArr4659043.data[0], s.size, s.data);
}
arr__char to_str__arr__char__nat64(ctx* ctx, nat64 n) {
	if (equalequal__bool__nat64__nat64(n, 0)) {
		return _constantArr4659031;
	} else if (equalequal__bool__nat64__nat64(n, 1)) {
		return _constantArr4659032;
	} else if (equalequal__bool__nat64__nat64(n, 2)) {
		return _constantArr4659036;
	} else if (equalequal__bool__nat64__nat64(n, 3)) {
		return _constantArr4659037;
	} else if (equalequal__bool__nat64__nat64(n, 4)) {
		return _constantArr4659035;
	} else if (equalequal__bool__nat64__nat64(n, 5)) {
		return _constantArr4659038;
	} else if (equalequal__bool__nat64__nat64(n, 6)) {
		return _constantArr4659039;
	} else if (equalequal__bool__nat64__nat64(n, 7)) {
		return _constantArr4659040;
	} else if (equalequal__bool__nat64__nat64(n, 8)) {
		return _constantArr4659041;
	} else if (equalequal__bool__nat64__nat64(n, 9)) {
		return _constantArr4659042;
	} else {
		return todo__arr__char();
	}
}
nat64 at__nat64__ptr_dict__nat64__nat64__nat64(ctx* ctx, dict__nat64__nat64* d, nat64 key) {
	opt__nat64 matched = get__opt__nat64__ptr_dict__nat64__nat64__nat64(ctx, d, key);
	switch (matched.kind) {
		case opt__nat64::Kind::none: {
			return todo__nat64();
		}
		case opt__nat64::Kind::some__nat64: {
			some__nat64 s = matched.as_some__nat64;
			return s.value;
		}
		default: assert(0);
	}
}
mut_arr__nat64* empty_mut_arr__ptr_mut_arr__nat64(ctx* ctx) {
	return uninitialized_mut_arr__ptr_mut_arr__nat64_arg0_is_0(ctx);
}
_void forbid___void__bool(ctx* ctx, bool condition) {
	return assert___void__bool(ctx, !(condition));
}
bool has__q__bool__mut_dict__nat64__nat64__nat64(ctx* ctx, mut_dict__nat64__nat64 d, nat64 key) {
	return has__q__bool__ptr_dict__nat64__nat64__nat64(ctx, unsafe_as_dict__ptr_dict__nat64__nat64__mut_dict__nat64__nat64(ctx, d), key);
}
_void add___void__ptr_mut_arr__nat64__nat64(ctx* ctx, mut_arr__nat64* a, nat64 value) {
	if (equalequal__bool__nat64__nat64(a->size, a->capacity)) {
		ptr__nat64 old_data = a->data;
		a->capacity = (equalequal__bool__nat64__nat64(a->size, 0) ? 4 : times__nat64__nat64__nat64(ctx, a->size, 2));
		a->data = uninitialized_data__ptr__nat64__nat64(ctx, a->capacity);
		copy_data_from___void__ptr__nat64__ptr__nat64__nat64(ctx, a->data, old_data, a->size);
	} else {
		
	}
	assert___void__bool(ctx, less__bool__nat64__nat64(a->size, a->capacity));
	[&]() { *(a->data + a->size) = value; return 0; }();
	a->size = incr__nat64__nat64(a->size);
	return 0;
}
arr__nat64 freeze__arr__nat64__ptr_mut_arr__nat64(mut_arr__nat64* a) {
	a->frozen = true;
	return unsafe_as_arr__arr__nat64__ptr_mut_arr__nat64(a);
}
arr__nat64 map__arr__nat64__arr__nat64__fun1__nat64__nat64(ctx* ctx, arr__nat64 a, fun1__nat64__nat64 mapper) {
	mut_arr__nat64* out = uninitialized_mut_arr__ptr_mut_arr__nat64__nat64(ctx, a.size);
	each___void__range_nat64__map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure_klb1_is_map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0(ctx, to__range_nat64__nat64__nat64(ctx, 0, a.size), map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure{out, mapper, a});
	return freeze__arr__nat64__ptr_mut_arr__nat64(out);
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
arr__char todo__arr__char() {
	return hard_fail__arr__char_arg0_is_4659034();
}
opt__nat64 get__opt__nat64__ptr_dict__nat64__nat64__nat64(ctx* ctx, dict__nat64__nat64* d, nat64 key) {
	return get_recursive__opt__nat64__arr__nat64__arr__nat64__nat64__nat64(ctx, d->keys, d->values, 0, key);
}
nat64 todo__nat64() {
	return hard_fail__nat64_arg0_is_4659034();
}
mut_arr__nat64* uninitialized_mut_arr__ptr_mut_arr__nat64_arg0_is_0(ctx* ctx) {
	return _alloc<mut_arr__nat64>(allocate_bytes__ptr__byte__nat64(ctx, 32), mut_arr__nat64{false, 0, 0, uninitialized_data__ptr__nat64_arg0_is_0(ctx)});
}
_void assert___void__bool(ctx* ctx, bool condition) {
	if (condition) {
		return 0;
	} else {
		return fail___void_arg0_is_4659033(ctx);
	}
}
bool has__q__bool__ptr_dict__nat64__nat64__nat64(ctx* ctx, dict__nat64__nat64* d, nat64 key) {
	return has__q__bool__opt__nat64(get__opt__nat64__ptr_dict__nat64__nat64__nat64(ctx, d, key));
}
dict__nat64__nat64* unsafe_as_dict__ptr_dict__nat64__nat64__mut_dict__nat64__nat64(ctx* ctx, mut_dict__nat64__nat64 m) {
	return _alloc<dict__nat64__nat64>(allocate_bytes__ptr__byte__nat64(ctx, 32), dict__nat64__nat64{unsafe_as_arr__arr__nat64__ptr_mut_arr__nat64(m.keys), unsafe_as_arr__arr__nat64__ptr_mut_arr__nat64(m.values)});
}
nat64 times__nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b) {
	return a * b;
}
ptr__nat64 uninitialized_data__ptr__nat64__nat64(ctx* ctx, nat64 size) {
	ptr__byte bptr = allocate_bytes__ptr__byte__nat64(ctx, size * 8);
	return reinterpret_cast<nat64*>(bptr);
}
_void copy_data_from___void__ptr__nat64__ptr__nat64__nat64(ctx* ctx, ptr__nat64 to, ptr__nat64 from, nat64 len) {
	if (equalequal__bool__nat64__nat64(len, 0)) {
		return 0;
	} else {
		[&]() { *(to) = *(from); return 0; }();
		return copy_data_from___void__ptr__nat64__ptr__nat64__nat64(ctx, incr__ptr__nat64__ptr__nat64(to), incr__ptr__nat64__ptr__nat64(from), decr__nat64__nat64(ctx, len));
	}
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
nat64 incr__nat64__nat64(nat64 n) {
	return n + 1;
}
arr__nat64 unsafe_as_arr__arr__nat64__ptr_mut_arr__nat64(mut_arr__nat64* a) {
	return arr__nat64{a->size, a->data};
}
mut_arr__nat64* uninitialized_mut_arr__ptr_mut_arr__nat64__nat64(ctx* ctx, nat64 size) {
	return _alloc<mut_arr__nat64>(allocate_bytes__ptr__byte__nat64(ctx, 32), mut_arr__nat64{false, size, size, uninitialized_data__ptr__nat64__nat64(ctx, size)});
}
_void each___void__range_nat64__map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure_klb1_is_map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0(ctx* ctx, range_nat64 r, map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure f) {
	if (empty__q__bool__range_nat64(ctx, r)) {
		return 0;
	} else {
		map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0(ctx, f, r.lo);
		return each___void__range_nat64__map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure_klb1_is_map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0(ctx, tail__range_nat64__range_nat64(ctx, r), f);
	}
}
range_nat64 to__range_nat64__nat64__nat64(ctx* ctx, nat64 lo, nat64 hi) {
	assert___void__bool(ctx, lessequal__bool__nat64__nat64(lo, hi));
	return range_nat64{lo, hi};
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
arr__char hard_fail__arr__char_arg0_is_4659034() {
	assert(0);
}
opt__nat64 get_recursive__opt__nat64__arr__nat64__arr__nat64__nat64__nat64(ctx* ctx, arr__nat64 keys, arr__nat64 values, nat64 idx, nat64 key) {
	if (equalequal__bool__nat64__nat64(idx, keys.size)) {
		return opt__nat64(_constant____none__0);
	} else if (equalequal__bool__nat64__nat64(key, at__nat64__arr__nat64__nat64(ctx, keys, idx))) {
		return opt__nat64(some__nat64{at__nat64__arr__nat64__nat64(ctx, values, idx)});
	} else {
		return get_recursive__opt__nat64__arr__nat64__arr__nat64__nat64__nat64(ctx, keys, values, incr__nat64__nat64(idx), key);
	}
}
nat64 hard_fail__nat64_arg0_is_4659034() {
	assert(0);
}
ptr__nat64 uninitialized_data__ptr__nat64_arg0_is_0(ctx* ctx) {
	ptr__byte bptr = allocate_bytes__ptr__byte_arg0_is_0(ctx);
	return reinterpret_cast<nat64*>(bptr);
}
_void fail___void_arg0_is_4659033(ctx* ctx) {
	return todo___void();
}
bool has__q__bool__opt__nat64(opt__nat64 a) {
	return !(empty__q__bool__opt__nat64(a));
}
ptr__nat64 incr__ptr__nat64__ptr__nat64(ptr__nat64 p) {
	return p + 1;
}
nat64 decr__nat64__nat64(ctx* ctx, nat64 n) {
	return ___nat64__nat64__nat64(ctx, n, 1);
}
bool empty__q__bool__range_nat64(ctx* ctx, range_nat64 r) {
	return equalequal__bool__nat64__nat64(r.lo, r.hi);
}
_void map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0(ctx* ctx, map__arr__nat64__arr__nat64__fun1__nat64__nat64__lambda0___closure _closure, nat64 i) {
	return set_at___void__ptr_mut_arr__nat64__nat64__nat64(ctx, _closure.out, i, call__nat64__fun1__nat64__nat64__nat64(ctx, _closure.mapper, at__nat64__arr__nat64__nat64(ctx, _closure.a, i)));
}
range_nat64 tail__range_nat64__range_nat64(ctx* ctx, range_nat64 r) {
	forbid___void__bool(ctx, empty__q__bool__range_nat64(ctx, r));
	return to__range_nat64__nat64__nat64(ctx, incr__nat64__nat64(r.lo), r.hi);
}
bool lessequal__bool__nat64__nat64(nat64 a, nat64 b) {
	return !(less__bool__nat64__nat64(b, a));
}
nat64 at__nat64__arr__nat64__nat64(ctx* ctx, arr__nat64 a, nat64 index) {
	assert___void__bool(ctx, less__bool__nat64__nat64(index, a.size));
	return *(a.data + index);
}
ptr__byte allocate_bytes__ptr__byte_arg0_is_0(ctx* ctx) {
	return malloc(0);
}
_void todo___void() {
	return hard_fail___void_arg0_is_4659034();
}
bool empty__q__bool__opt__nat64(opt__nat64 a) {
	opt__nat64 matched = a;
	switch (matched.kind) {
		case opt__nat64::Kind::none: {
			none n = matched.as_none;
			return true;
		}
		case opt__nat64::Kind::some__nat64: {
			some__nat64 s = matched.as_some__nat64;
			return false;
		}
		default: assert(0);
	}
}
nat64 ___nat64__nat64__nat64(ctx* ctx, nat64 a, nat64 b) {
	assert___void__bool(ctx, greater__bool__nat64__nat64(a, b));
	return a - b;
}
_void set_at___void__ptr_mut_arr__nat64__nat64__nat64(ctx* ctx, mut_arr__nat64* a, nat64 index, nat64 value) {
	assert___void__bool(ctx, less__bool__nat64__nat64(index, a->size));
	return [&]() { *(a->data + index) = value; return 0; }();
}
nat64 call__nat64__fun1__nat64__nat64__nat64(ctx* ctx, fun1__nat64__nat64 f, nat64 p0) {
	return call_with_ctx__nat64__ptr_ctx__fun1__nat64__nat64__nat64(ctx, f, p0);
}
_void hard_fail___void_arg0_is_4659034() {
	assert(0);
}
bool greater__bool__nat64__nat64(nat64 a, nat64 b) {
	return !(lessequal__bool__nat64__nat64(a, b));
}
nat64 call_with_ctx__nat64__ptr_ctx__fun1__nat64__nat64__nat64(ctx* c, fun1__nat64__nat64 f, nat64 p0) {
	return f.fun_ptr(c, f.closure, p0);
}


int main() { return (int) main__int64(); }
