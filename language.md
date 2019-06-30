## Overview

A few interesting aspects of noze:

* There is no global state. A function can access only its arguments.
* Normally, functions cannot have side-effects, except through interfaces. (See the section on interfaces). A normal function with no interface parameters performs no I/O, and a function with no mutable parameters is a pure function. (A "normal" function is a non-`summon` non-`unsafe` one -- see the section on function attributes.)


## Expressions

#### Call expression

Call expressions are normally written in infix syntax, like `x + y`.
There is no operator precedence, and calls usually associate to the left,
so `x + y * z` is the same as `(x + y) * z`.

To indicate that there are no subsequent calls, add a colon after the function name:
`x +: y * z` is the same as `x + (y * z)`.

If you prefer a prefix syntax for a call, you can put the function name first and then a colon: `+: x, *: y, z` is the same as `x + (y * z)`.

A function with no arguments can be called without parentheses.
Noze has no construct for constants, instead just use a function of no arguments.

A function of one argument can be written with a `.`. `x.abs + y` is equivalent to `(x abs) + y`.


#### `when` expression

First, there is a function `if` that is suitable for small conditional expressions, as in `a < b if a, b`.
For longer conditional expressions there is a multi-line form:

```
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






## Types

### Common types


### Record types

A record type is declared like so:

```
point-2d record
	x float64
	y float64
```

A record is created with a `new` expression.
Where the type is not clear from context, this may be provided with type argument syntax.

```
zero point-2d()
	| Type is obvious from context
	new 0, 0
```

```
zero2 point-2d()
	| Need to provide the type
	p = new<point-2d> 0, 0
	p
```

For each field of the record, it's as if there is a function with that field's name, taking a record and returning the field value. So you can access the `x` value of a point with `p x` (or `p.x`).

A field can be mutable:

```
mutable-point2d record mut
	x mut float64
	y mut float64
```

(For the `mut` at the top, see the section on purity later.)

You can change the value of a field using `:=`, as in `p.x := 0`. (There is no `+=` or the like.)

Normally the compiler is free to choose whether a record is by-value or by-reference. However, a mutable record is always by-reference, so you can be guaranteed mutations you make are seen everywhere.


### Union types

A union type is declared like so:

```
circle record
	radius float64

square record
	width float64

shape union
	circle
	square
```

A member of the union implicitly converts to the union, and this is actually the only way to create a union value.
Where there is no expected type, you can create it using the function `as` which is just the identify function: `my-circle as<shape>` will have the correct type `shape` rather than `circle`.

Note that this is a *conversion* -- it's not that a circle "is-a" shape. So a `circle` argument can be passed to a `shape` parameter, but an `arr circle` will not convert to an `arr shape`. (`arr` is a template type, there will be a section on those later.)

Note that unlike some other languages with unions, the members of the union are independent of the union. They can be used alone, or in other unions.
It's planned for a smaller union to implicitly convert to a bigger union that contains all its members (e.g. one with circle, square, or triangle), but this is not implemented.

To get the members of the union back, use `match`:

```
area float64(s shape)
	match s
		circle c
			pi * c.radius.squared

		square s
			s.width.squared
```


### Interface types

Note: type-checking is implemented for these, but code generation is not.

An interface type is declared like so:

```
printer iface
	print void(s str)
```

An interface is a set of signatures which may be implemented by an interface implementation.

```
stdout-printer printer() summon
	new-actor
		print(s)
			s print-sync
```

And called like:

```
callit fut void(p printer)
	p !print "hello"
```

The implementation of an interface will run in parallel, meaning the caller of the interface will keep running rather than wait for the implementation to run.
All interface methods implicitly return a `fut` (a future) -- interfaces are never synchronous.

Note that interfaces in noze are very different from those in Java -- interfaces are not implemented by types, they are implemented by interface-implementation expressions. There is no subtyping involved -- an interface is itself a particular type. Interfaces cannot subtype each other either.

Interfaces are also the normal way to perform I/O (without `summon`). Most I/O operations could take an arbitrarily long amount of time, so it's nice to know that they generally run in parallel. Functions that run synchronously usually have names that end in `sync` (and are usually `summon`). But note that it's not considered unsafe behavior for a function to take a long time, since that's possible without performing any I/O at all -- to handle that interrupts should be added to the language.


## Functions

Mention: how expression syntax works

Describe all the expressions


## Function attributes

A normal function has no attributes. This function cannot perform I/O except through interfaces and cannot do dangerous things like pointer arithmetic.

Function attributes share the same syntax as specs -- they are written after the parentheses on a function declaration.

A `summon` function is allowed to perform I/O without being passed an interface -- it summons the ability to perform I/O out of thin air.

```
say-hi void() summon
	print-sync "hello world"
```

Usually a `summon` function should return an interface, and the rest of the code should use that interface. Only `summon` functions can call `summon` functions. (Meaning, `main` will have to be summon for your program to do anything.)

`unsafe` functions are used to implement the runtime. Only `unsafe` or `trusted` functions can call unsafe functions. Any function can call a `trusted` function.

`noctx` functions are similarly used to implement the runtime. Normal functions have an implicit `ctx` parameter that lets them use the runtime to do things like allocate memory, throw exceptions, create new actors, and enqueue new tasks. `noctx` functions effectively have no runtime, making it possible to implement the runtime in noze itself.

`extern` functions are implicitly `unsafe` and `summon`. These are like `extern` functions in C -- they should be implemented in a library that is linked in.
(Currently there is no way to specify libraries to link, so the only `extern` functions are from the C standard library.)


## Purity

TODO:MORE
An interface can only use `sendable` types.


## Templates

Mention: how to write your own templates,
how to explicitly provide type arguments




## Specs





