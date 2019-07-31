# code style

If the source code conflicts with this document that should be considered a TODO, not an exception.

## Limiting C++

Noze is written using a limited subset of C++. It's more like C with templates.

* Exceptions circumvent the type system.
  You'll end up throwing exceptions you don't catch, or catching exceptions you don't throw, so no exceptions.
  Except for the one exception :) which is in `lexer.h` which throws parse errors.
  It was too inconvenient to handle every possible parse error since nearly everything in the parser can fail.
  Hypothetically we could add a diagnostic and continue parsing,
  but that tends to produce long streams of parse errors where only the first is meaningful.

* Don't write destructors.

* Don't write assignment operators.

* Don't write copy constructors.
  You may disable them as in `StructName(const StructName&) = delete;`,
  in which case there should also be `StructName(StructName&&) = default;`.
  This is useful to prevent accidental copying of mutable structs.

* No `new`, use `nu<T>{}(arena, ...args...)` from `arena.h` instead.

* `Arena` is special in that it has a destructor.
  All allocation should be done through `Arena`, so other types don't need destructors.

  Writing to the wrong `Arena` could be dangerous;
  luckily, a function almost always writes to only one arena, which should be its first parameter.
  It doesn't need access to the arena it's reading from, just the one it's writing to.

  The type of something determines its arena, e.g. something from `model.h` goes in the model arena.

  Be sure to remember to `copyStr` when moving a `Str` from one phase to the next.
  `Sym` doesn't have this problem as the `AllSymbols` has its own arena and should be alive for the whole compilation.

* Use `struct` instead of `class`, and only include fields and optionally a constructor.
  Don't write a constructor if it would just initialize every field,
  since struct initialization syntax `MyStruct{1, 2}` already does that.
  Do write a constructor that just initializes every field if you can also think of some assertions.
  Assertions in constructors are better than assertions in other functions
  because an assertion in a normal function only guarantees a property in that one function.
  An assertion in a constructor guarantees a property everywhere that type appears.
  (This assumes the fields are `const`; if not, the assertion should be done again whenever mutating the fields.)

* Constructors may *only* do a few things:
  - Set each field to a value of one of the parameters
  - Default-initialize a field
  - Assert that this instance is valid

  It would probably be a good idea to eliminate constructors and instead use a function called `new_MyType`.
  Even types like `MutArr`  (and `MutDict` and `MutSet` which wrap it) and `Arena` follow this rule --
  their constructors default-initialize all fields to 0 / nullptr
  and allocation isn't done until the first push / add / alloc.

* Only use headers that are C compatible and cross-platform. This means no `std::`.
  Use the data structures defined in `util` which are analogous to those from noze.

* Don't use references (`Foo&`).
  Generally just pass things by value, even if they're large.
  Remember, we don't use copy constructors, so passing `Arr` by value just copies the pointer, not the data.
  Same for `Dict` and `Set`.
  (Pass `MutArr`, `MutDict`, `MutSet` by pointer since these are mutable.)
  Pass something by pointer if:
  * It's mutable
  * Its address matters (e.g. `StructDecl`s are often compared by address, as are `StructInst`s)
* It's very large (like `CommonTypes`)

* Don't use the preprocessor (except `#include`) unless absolutely necessary.
  So far it has never been.
  Only use `#include` at the top-level to import header files.

* Don't use overloading, give all functions unique names.

* `nullptr` -- Use `Opt` instead.

* A pointer should be considered to point to exactly one value.
  Don't use `nullptr`, use `Opt` instead.
  Use `Arr` for pointers to potentially more than one value.

The following C++ features *are* allowed:

* Templates, either struct or function templates.

* Lambdas, for use in functions like `map`.

  Lambda literals should only appear as an argument to a call.
  This means do *not* use a lambda as a nested function just for the syntactic convenience of
  closing over parameters and not having to write them out;
  use a separate (un-nested) function instead and explicitly pass in exactly what's needed.

  Lambdas should be *temporary*, never stored anywhere.
  Never store data hidden behind a function, store data as normal data and pass it to a function when necessary.
  (If there are multiple functions that might be associated with the data, use a union.)

  Lambdas should always be of a type parameter type (rather than `std::function`) whose name starts with `Cb`.



## Unions

We use the following verbose pattern for unions in C++:

The noze code:

```nz
Foo record
  x nat
Bar record
  y nat
FooBar union
  Foo
  Bar
```

Becomes:

```cpp
struct Foo {
  const size_t x;
};
struct Bar {
  const size_t y;
};
struct FooBar {
private:
  enum class Kind {
    foo,
    bar,
  };
  const Kind kind;
  union {
    const Foo foo;
    const Bar bar;
  }
public:
  explicit inline FooBar(const Foo _foo) : kind{Kind::foo}, foo{_foo} {}
  explicit inline FooBar(const Bar _bar) : kind{Kind::bar}, bar{_bar} {}

  // Optional convenience functions may be added here like:
  inline bool isFoo() const {
    return kind == Kind::foo;
  }
  inline Foo asFoo() const {
    assert(isFoo());
    return foo;
  }

  // `match` should always be present.
  // `isFoo()` should only be used when `foo` is the only interesting case and there are more than 2 union members.
  // Else always prefer `match`.
  template <
    typename CbFoo,
    typename CbBar
  >
  inline auto match(
    CbFoo cbFoo,
    CbBar cbBar
  ) const {
    switch (kind) {
      case Kind::foo:
        return cbFoo(foo);
      case Kind::bar:
        return cbBar(bar);
      default:
        assert(0);
    }
  }
}
```

This pattern is verbose on the implementer of the type,
but easy on the user who just writes `FooBar{Foo{42}}` to create an instance, and uses it like:

```nz
size_t value(const FooBar fb) {
	return fb.match(
		[](const Foo f) {
			return f.x;
		},
		[](const Bar b) {
			return b.y;
		});
}
```


## More style guidelines:

* Use `const` wherever possible.
  This means struct fields, parameters, return types and local variables should all be `const` if possible.
  - This means *never* declare a variable and assign it later.
    All variables should be assigned and declared in one step.
  	Use an IIFE if necessary, or better, just use a separate function.
    (An IIFE is using `[&]() { some_side_effect(); return some_expression; }()`
    which enables you to have multiple statements within one expression.)
  - Instead of using a mutable variable in a loop, use `for (const size_t i : Range{n})`.
    (Or use something like `map` as appropriate.)
  - Data structures that overwrite structs use the `overwriteConst` function,
    since the assignment operator is deleted when some field is `const`.
    This is safe since we avoid using copy constructors anywhere.

* Use `assert` generously, especially in constructors.
  Especially if you "know" it should never possibly fire.

* Don't add redundant fields to structs for efficiency, just recalculate it again each time.
  (At this point, don't do anything for efficiency unless there's a really big win.
  I'll worry about efficiency when there is a test case that takes more than 10 seconds.
  Any less and performance improvements are hard to measure.)

* Prefer to provide a function with as little information as necessary to get its result.
  This means the user of the function can quickly know what it *doesn't* depend on.
  It also means we can avoid calculating information that isn't actually needed.

  This does not mean limiting the number of parameters without regard to th
  scope of information reachable from those parameters.
  Instead of a single `WholeKitchenSink` parameter,
  prefer separate `Bowl` and `Spoon` parameters if those are all that is needed.

* Break up functions into smaller functions as much as possible,
  as long as those smaller functions can be given meaningful names.


### Implementation files

* Every implementation (`.cpp`) file should begin with `#include "./myname.h`,
  then by a blank line, where this is `myname.cpp`.

* It should then have global imports sorted alphabetically, then a blank line.

* It should then have relative imports, sorted first by number of parent directories (more first), then alphabetically.
  Put a blank line between each group of imports with the same number of parent directories.

* It should then be followed by `namespace {`, an anonymous namespace that will contain all helper functions.
  (Omit if there are no helper functions.)
  Implementations of functions from the header should go at the bottom and follow the same order as the header.

E.g.:

```cpp
// add.cpp
#include "./add.h"

#include <unistd.h>

#include "../alpha/zardoz"
#include "../yak.h"

#include "./monkey/watch.h"

namespace {
	size_t doAdd(const size_t a, const size_t b) {
		return a + b;
	}
}

size_t add(const size_t a, const size_t b) {
	return doAdd(a, b);
}
```


### Header files

* Every header file should begin with `#pragma once`, followed by a blank line.
  Then any necessary imports, using the same style as implementation files.

* Mark a function `inline` iff it is in a header file and is not a template.

* Implement a function in a header file iff it is trivial or is a template.

* Don't put a function in a header file that will only be used in the implementation file.

* Often you should put the type declaration for the output of an operation
  in a separate header file from the function definition that outputs it.
  This is because the next component will only care about the output, not how it was generated.


### Bool

Use `Bool` for booleans instead of `bool`.
`Bool` implicitly converts *to* `bool` but not *from* it,
since many other types implicitly convert to `bool` which has caused me much sorrow.

Instead of using operators like `||` you may need to use the `_or` macro which returns a `Bool`.
Similarly, replace `&&` with `_and` and `!` with `_not`.
This is not necessary inside an `if` but is when returning a `Bool` from a function or when declaring a `Bool` local.
