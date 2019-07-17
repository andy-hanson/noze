# overview

A few interesting aspects of noze:

* It is strongly typed and expression-based. It has no subtyping or casts and only a few kinds of implicit conversions, making it safe.
* There normally is no global state. A normal function can access only its arguments.
  The exception is `summon` functions, which can only be called by other `summon` functions.
  Similarly, a normal function can not perform I/O, except through an argument of interface types.
  A normal function with no interface parameters performs no I/O, and a normal function with no mutable parameters is a pure function.
* It's designed for parallelism. Interfaces always return `fut` (futures) allowing them to run in parallel, and the type system tracks what data can safely be send to an interface.

## syntax

A noze file consists of many top-level declarations. There are no nested declarations. (Local variables may appear in nested expressions but those are not considered declarations for this purpose.)
Each declaration is declared like `name type`, with an indented block following.
The language is fairly strict with whitespace -- an indentation must be exactly one tab, and leading / trailing spaces on a line are illegal. "Extra" whitespaces in random places are also illegal as are missing whitespaces where they are normally expected, such as after a comma.
It is also a syntax error to have multiple spaces in a row or to fail to put spaces around an operator (which is parsed much like a regular name).


Noze is an expression-based language -- it has only expressions and no concept of a statement.
A function body is an expression and no `return` is necessary.
Functions that return "nothing" return `void`, which is an empty type.

#### names

A name may be *either* alpha *or* an operator. Mixing alpha and operator characters is not allowed, e.g. `a/b` is not a valid name.
The compiler treads alpha and operator names the same -- the difference exists only in the lexer.

An alpha name consists of `a-z`, `0-9`, hyphens `-`, and may optionally end in a `?`. Upper-case letters and underscores are illegal. The name must begin with `a-z`.

An operator name consists of a sequence the characters `+-*/<>=`. You can build any operator you want, e.g. `+/<`. Try not to abuse this!
`=` is not an operator though, it is a keyword. But `==` is an operator, an ordinary name.
There is no `!=` operator since `!` is used for other purposes (see the section on interfaces).

Despite having an alpha / operator distinction, there *must* be spaces around operators. `a+b` is a syntax error, it must be `a + b`.


#### call syntax

The most common expression is a call expression, so there are a number of ways to write it for convenience.
Common operators such as `+` are just call expressions.
Special functions like `and` and `if` are also just call expressions,
though the compiler treats them specially as their arguments are lazily evaluated.


Normally, prefer to write call expressions using infix syntax, like `x + y`.
Note this is not binary only, so you can write `x +` for a 1-argument function or or `x + y, z` for a 3-argument function.
There is no operator precedence, and calls usually associate to the left,
so `x + y * z` is the same as `(x + y) * z`.
Alpha names are treated the same as operator names by the parser, so you could write `x plus y` too.


If you put a colon `:` after the function name, that makes this the last call on the line -- any other calls happen within the arguments to this call. So `x +: y * z` is the same as `x + (y * z)`.

If you prefer a prefix syntax for a call, you can put the function name first and then a colon: `+: x, *: y, z` is the same as `x + (y * z)`.

A function with no arguments can be called without parentheses.
Noze has no global variables, and thus no global constants, instead just use a function of no arguments.

A function of one argument can be written with a `.`. `x.abs + y` is equivalent to `(x abs) + y`.
The `.` is tightly binding, so `x + y.abs` is parsed as `x + (y.abs)` rather than `(x + y).abs`.


#### comments

A regular comment is a line beginning with `| ` (the `|` must be followed by a space).
One may also replace `|` with `region` for a comment indicating that the following code forms a logical group.
A line may not mix comment and non-comment content -- a comment must go on its own line.


## type

There are only a few basic kinds of types -- [records](#record), [unions](#union), and [interfaces](#iface).
There are also type parameters, though those of course must resolve to one of the above eventually. See the section on [templates](#templates) for those.


### record

A record type is declared like so:

```nz
point-2d record
	x float
	y float
```

A record is created with a `new` expression.
Where the type is not clear from context, this may be provided with type argument syntax.

```nz
zero point-2d()
	| Type is obvious from context (return type of the function)
	new 0, 0

zero2 point-2d()
	| Need to provide the type
	p = new<point-2d> 0, 0
	p
```

For each field of the record, it's as if there is a function with that field's name, taking a record and returning the field value. So you can access the `x` value of a point with `p x` (or `p.x`).

A field can be mutable:

```nz
mutable-point2d record mut
	x mut float
	y mut float
```

(Why the `mut` at the top? See the section on [#purity](purity) later.)

You can change the value of a field using `:=`, as in `p.x := 0`. (There is no `+=` or the like.)

Normally the compiler is free to choose whether a record is by-value or by-reference. However, a mutable record is always by-reference, so you can be guaranteed mutations you make are seen everywhere rather than just mutating a copy.




### union

A union type is declared like so:

```nz
circle record
	radius float

square record
	width float

shape union
	circle
	square
```

The members of a union can be records or interfaces, but not other unions.

A member of the union implicitly converts to the union, and this is actually the only way to create a union value.
Where there is no expected type, you can create it using the function `as` which is just the identify function: `my-circle as<shape>` will have the correct type `shape` rather than `circle`.

Note that this is a *conversion* and not a subtyping relationship.
It's not that a circle "is-a" shape.
So a `circle` argument can be passed to a `shape` parameter because the compiler will fill in the conversion,
but an `arr circle` will not convert to an `arr shape`. (`arr` is a template type, see the section on [templates](#templates))

Unlike some other languages with unions, the members of the union are independent of the union. They can be used alone, or in other unions.
It's planned for a smaller union to implicitly convert to a bigger union that contains all its members (e.g. shape could convert to a union of circle, square, or triangle), but this is not implemented.

The only thing you can do with a union is to use a `match` expression to handle each of its members:

```nz
area float(s shape)
	match s
		circle c
			pi * c.radius.squared

		square s
			s.width.squared
```

The match cases must match the order the types were declared in the union.
Each match case consists of a type name, an optional name of a local variable of that type, then an indented block providing a result for that type.

There are no enum types, but a union of empty records can be used instead. (This is why the match case's local variable name is optional -- it's useless for an empty type.)


### iface

WARNING: Interfaces are not finished yet (though they should get past the type-checker at least), this section is merely a plan.

An interface type is declared like so:

```nz
printer iface
	print void(s str)
```

An interface has a set of signatures which may be implemented by a new actor.

```nz
stdout-printer printer() summon
	new-actor
		print(s)
			s print-sync
```

And called like:

```nz
call-it fut void(p printer)
	p !print "hello"
```

The implementation of an interface will run in parallel, meaning the caller of the interface will keep running rather than wait for the implementation to run.
All interface methods implicitly return a `fut` (a future) -- interfaces are never synchronous.

Interfaces in noze are very different from those in Java -- interfaces are not implemented by types, they are implemented by `new-actor` expressions.
There is no subtyping involved -- an interface is a concrete type.
Interfaces cannot currently subtype each other.

In the above example the `new-actor` doesn't use anything from the outer context. A `new-actor` can also close over outer variables, though unlike lambdas this must be explicit.

```nz
printer-to-mut-arr printer(m mut-arr char) summon
	new-actor(mut m)
		print(s)
			m push-all s
```

This actor closes over `m`, which is a `mut-arr` and is not thread safe.
So, it will be tagged with the same ID as the current actor (the one that called `printer-to-mut-arr`), so that it can't run in parallel with that.
The actor may still run in parallel to its *caller* though.

For interfaces with a single method, you might just use a `send-fun` type and implement it using a [lambda](#lambdas).

Interfaces are also the normal way to perform I/O (without `summon`). Since I/O operations may take a long time, it's useful that interfaces always return `fut`.
The exceptional functions that run synchronously usually have names that end in `sync` (and are usually `summon`).
However, it's not considered unsafe behavior for a function to take a long time, since that's possible without performing any I/O at all -- to handle that interrupts should be added to the runtime.


### arr

`arr` is technically a record type consisting of a pointer and size. But the compiler treats it specially by having a `new-arr` expression:

```nz
one-two-three arr nat()
	new-arr 1, 2, 3
```

As with `new`, when there is no expected type you can provide a type argument.

```nz
one-two-three arr nat()
	a = new-arr<nat> 1, 2, 3
	a
```

The compiler can treat arr literals as constants (so long as every element is a constant), which avoids the allocation.





## other expressions

#### `when` expression

First, there is a function `if` that is suitable for small conditional expressions, as in `a < b if a, b`.
For longer conditional expressions there is a multi-line form:

```nz
when
	x < 0
		"negative"
	x > 0
		"positive"
	else
		"zero"
```

This is equivalent to `x < 0 if: "negative", x > 0 if: "positive", "zero"`.

With the exception of the `else`, each condition may be any (one-line) expression.
The `else` *must* be present.


#### literals

For a literal expression like `123` or `"hello"`:

* If there is no expected type, it's a string.
* If there is an expected type, it's a call to a function named `literal` taking that string.

The syntax for literals is that it must either be `[0-9]+(\.[0-9]+)`, or a quoted string literal. String literals support escapes similar to C.

Calling a `literal` function ensures that we don't need to have suffixes on number literals to indicate their type, and users can define their own number types.
It also means that converting a string to a number is in the runtime rather than the compiler.
Other languages solve this problem with implicit conversion between number types, but this is a common source of bugs and doesn't allow for number types bigger than the compiler's default.


#### lambdas

Noze has two different kinds of dynamically-invokable function types:

`fun`: This is a `mut` type for a function that will be synchronously invoked.
  It's `mut` because the function might close over `mut` objects.
  This is the more common kind of fun, used to e.g. map over an array.
`send-fun`: This is a `sendable` type that acts like an interface.
  It may close over `mut` objects, but like with `new-actor`, it won't be run synchronously, it will run in the appropriate context with exclusive access to any `mut` things it closes over.

The same syntactical kind of expression handles both depending on the expected type, and looks like this:

```nz
n-times void(n nat, f fun1 void nat)
	f call n
	n == 0 if: pass, n.decr n-times f

count-down void() summon
	10 n-times \i
		i.to-str print-sync
```

This could also be written as `10 n-times {it.to-str print-sync}`.
`{}` marks special shorthand for a lambda -- if it contains the identifier `it` it takes that as one parameter, otherwise it has no parameters.

If a lambda would be equivalent to an existing function, you can write `&f` where `f` is the function name.

There is no way to explicitly provide a parameter type in a lambda; an expected type is mandatory. (You can always use `as<fun1<void, nat>>:` to provide an expected type.)

### Arrows

A common operation on `fut`s is `then`:

```nz
plus-one fut nat(f fut nat)
```




## functions

A normal function declaration looks like:

```nz
add-em nat(a nat, b nat)
	a + b
```

The function is written name-first, type-second: the name is `add-em` and the return type is `nat`.
Parameters work the same way.


## function attributes

A normal function has no attributes. It cannot perform I/O except through interfaces and cannot do dangerous things like pointer arithmetic.

Function attributes share the same syntax as [specs](#specs) -- they are written after the parentheses on a function declaration.

#### summon

A `summon` function is allowed to perform I/O without being passed an interface -- it "summon"s the ability to perform I/O out of thin air.

```nz
say-hi void() summon
	print-sync "hello world"
```

Only `summon` functions can call `summon` functions. (Meaning, `main` will have to be summon for your program to do anything.)
However, a `summon` function can return an interface, and normal functions can perform I/O through that interface.

#### unsafe / trusted

`unsafe` functions are used to implement the runtime. These can do things like pointer arithmetic or allocating uninitialized memory.
Only `unsafe` or `trusted` functions can call unsafe functions.
Any function can call a `trusted` function.

A safe function should never be able to access memory that was not validly passed in, mutate immutable values, perform I/O without `summon`, fail with a hard assertion failure (catchable exceptions are OK) or other bad stuff.

The runtime has some additional safety properties, such that all `fut`s must be resolved eventually, and so should hide its implementation details or make them `unsafe`. For example, `new-unresolved-fut` is `unsafe` since the user might forget to resolve it, but `then` is safe since the new `fut` will always be resolved if the previous one was.

This is so regardless of any input passed in. So for example, `at`, which accesses an element of an array, must check that the index is valid and throw an exception.
All operations on `ptr` are unsafe. Theoretically we could make just dereferencing unsafe, or just pointer arithmetic unsafe, but it's simpler to just make it all unsafe.


#### noctx

`noctx` functions are similarly used to implement the runtime.
Normal functions have an implicit `ctx` parameter that lets them use the runtime to do things like allocate memory, throw exceptions, create new actors, and enqueue new tasks.
The vast majority of functions require a ctx, even `+` since that may throw an exception on overflow.
`noctx` functions effectively have no runtime, making it possible to implement the runtime in noze itself.


#### extern

`extern` functions are implicitly `unsafe` and `summon`. These are like `extern` functions in C -- they should be implemented in a library that is linked in.
(Currently there is no way to specify libraries to link, so the only `extern` functions are from the C standard library.)


## purity

Every type (except type parameters) has a purity. The three values of purity are `data`, `sendable`, and `mut`.

`data` is ordinary immutable data. This is safe to share with other parallel tasks.
`sendable` is not immutable, but has been specially designed to be safe to send to another parallel task.
`mut` is ordinary mutable data, not parallel-safe.

`data` is more pure than `sendable`, which is more pure than `mut`.

The purity specifier goes after specifying the kind of type, e.g. `r record mut` or `u union sendable`. `data` is the default.
You can't specify purity for an interfaces because those are all `sendable`.

The constituents of a type must be at least as pure as that type. This applies transitively.

```nz
i-am-mutable record mut
	x mut nat

what-am-i record
	m i-am-mutable
```

The above example is a compile error: `what-am-i` should be marked `mut` even though it has no `mut` fields itself, because `mut` data is eventually reachable through it.

(It may also be marked `force-sendable` which is used internally by the runtime to create the primitive sendable types.)

Everything in an interface must be sendable -- meaning the return types and parameter types of all of its signatures.


## templates

Instead of using a particular type, a function may specify a type parameter to be filled in by the caller.

```nz
choose ?t(b bool, a ?t, b ?t)
	b if a, b

call-on-nat nat(b bool)
	b choose 1, 2

call-on-float float(b bool)
	b choose 1.0, 2.0
```

Type arguments can be provided explicitly, as in `b choose<float> 1.0, 2.0`.

For a function, there's no need to declare your intention to use type parameters, you just start using them.
You can choose to specify them explicitly though, as in `choose<?t> ?t(b bool, a ?t, b ?t)`.
This can be useful for controlling their order (which affects the syntax when type arguments are provided explicitly).

A type can also be a template. In this case type parameters must be listed explicitly.

```nz
pair<?t> record
	a ?t
	b ?t

get-nats pair nat()
	new 1, 2
```

Type arguments to a type are normally just provided with a space in between.
But to provide nested type arguments to a type argument angle brackets must be used, as in `pair pair<nat>`.

Unlike other languages, a template is type-checked *abstractly*, once, rather than once per instantiation.
Type arguments are *opaque*, meaning nothing about the type is known.
This means you can't do things that only work on particular types, like access properties or `match` on them.
A template will be checked to work for *any* type `?t`, even ones you don't happen to be using.
This means you can't have `x + 1` where x is `?t`, even if you happen to only instantiate it with numeric types.
However, you can use **specs** to ensure that some needed functions will exist.


## specs

Often a template function needs to rely on some functionality existing for a type.
It can specify what functions it expects to exist using a spec.

```nz
my-spec<?t> spec
	zero ?t()
	+ ?t(a ?t, b ?t)

my-sum ?t(a arr ?t) my-spec<?t>
	when
		a empty?
			zero
		else
			a.first + a.tail.my-sum

six nat()
	(new-arr<nat> 1, 2, 3) my-sum
```

The implementations for the spec functions are provided by the *caller* and don't need to exist in the spec user's scope.
This provision is done implicitly by looking up functions with the same name and checking if their signature matches the spec signature.
(Parameter names don't have to match.)
Spec signatures shouldn't be marked with attributes like `summon`; they can always be implemented by a `summon` function if the provider of the spec implementation (`six` in this example) is `summon`, even if the user of the spec implementation (`my-sum` in this example) is not.
Specs don't constrain a *type*, they constrain the functions that the caller must have in scope.
Specs can take multiple or no type parameters.
Different callers might have different implementations of the spec for the same type, depending on what functions they have in scope.


## modules

A noze program consists of multiple files.
But when you run a program you specify only the main file.
That file must name all other modules it wants to import, except for `include.nz` which is always included.

When module A imports module B, all declarations in B become visible in A. This is not transitive, so B can import things without affecting its callers.
Module import cycles are not allowed.
If two functions depend on each other but for some reason you want them in different modules, you could use a spec to allow each to declare their dependency on the other without a direct import. You could also define them to take a lambda (or interface) instead of a direct dependency.

Import syntax looks like so:

```nz
import a .b ..c.d
```

There can only be at most one import statement and it must be the first non-comment line in a file.

This will import files from:

* `a`: This is a global import. Currently global imports can only import things from the `noze/include` directory, but it's planned to support other global import paths too.
* `.b`: This is a local import. The compiler will expect a file `./b.nz` to exist or it will be a compile error.
* `..c.d`: This is also a local import from `../c/d.nz`. The number of leading `.` is the number of parent directories to climb, minus one. Subsequent `.`s act as directory separators.
  `...c.d` would import from `../../c/d.nz`.


### private

Halfway through a module you can add the `private` keyword:

```nz
f nat()
	g + g

private

g nat()
	1
```

Other modules will import `f` (and all things above `private`) but not `g` (and all things below `private`).

