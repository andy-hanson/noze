#include <cstdio>

#include "./frontend/ast.h"
#include "./util.h"
#include "./util/arrUtil.h"
#include "./concreteModel.h"

int handle(TypeAst const& t) {
	return t.match(
		[](TypeAst::TypeParam _) {
			unused(_);
			return 1;
		},
		[](TypeAst::InstStruct _) {
			unused(_);
			return 2;
		}
	);
}

void test() {
	Arena arena;

	const SourceRange range = SourceRange{1, 2};
	const Str name = strLiteral("abc");
	const TypeAst t = TypeAst{TypeAst::TypeParam{range, name}};
	const int i = handle(t);
	printf("handled: %d\n", i);

	const Arr<int> a = arrLiteral<int>(arena, 1, 2);
	const Arr<int> b = map<int>()(arena, a, [](int x) { return x + 1; });

	for (const size_t i : Range{b.size}) {
		printf("arr elem: %d\n", b[i]);
	}
}

///c->kind.match(
//	[&](const Constant::Array a) {
//		// static arr_nat64 _constantArr124 = arr_nat64(4, (cast(nat64[3]) [1, 2, 3]).ptr);
//		// Strings are special:
//		// static arr_char _constantArr123 = arr_char(5, (cast(char[5])"hello").ptr);
//	})

struct arr_char {
	size_t size;
	char* begin;
};

static arr_char _constantArr123 = arr_char{5, const_cast<char*>("hello")};

using nat64 = Nat64;

struct arr_nat64 {
	size_t size;
	nat64* begin;
};

static nat64 _constantArr124Backing[3] = {1, 2, 3};
static arr_nat64 _constantArr124 = arr_nat64{3, _constantArr124Backing};


int main(void) {
	Arena arena;

	const Path* p = Path::root(arena, strLiteral("hello"));
	NulTerminatedStr s = pathToNulTerminatedStr(arena, p);

	printf("Path is %s\n", s._begin);

	for (const size_t i : Range{5, 10}) {
		printf("i = %ld\n", i);
	}

	return 0;
}
