# compiler design

The compiler consists of a few well-defined stages:

* The source code text.

* The AST, defined in `ast.h`.
  This comes from *parsing* the source code text.
  There is no separate lexing phase, and the parser often operates directly on characters in the source code.
  There is a separate `lexer.h` file containing many useful utilities for operating on text, but there's no strict boundary between the parser and source text.
  The AST consists of only the information that is available by direct parsing -- the parser does not attempt to resolve what names refer to.

* The model, defined in `model.h`.
  This comes from *type-checking* the AST.
  This stage is the prettiest in that it best represents what the programmer wrote and meant.
  The AST lacks information about what names refer to, and the concrete model is an expansion of the model that contains many more functions than were directly written by the programmer.
  The process of getting the model does all type checking -- there should be no compile errors from that point.
  If a linter were added it would likely operate on the model.

* The concrete model, defined in `concreteModel.h`.
  This comes from *concretizing* the model.
  Unlike the model, the concrete model does not have templates. Instead it contains separate instantiations for everything.
  The concrete model is designed to be easy to generate code from, not to represent what the programmer meant.
  For example, expressions may be replaced by constants, and there is no notion of modules.
  (If one were trying to compile noze to a higher-level language like Java bytecode, one would use the model directly, not the concrete model.)

* Finally, C source code is produced. It's also planned to produce a libFIRM IRP and then directly generate machine code.


Each stage gets its own arena allocator, and the previous stage is discarded as soon as the current stage is available. This means that when using the model, you never have to go back to the AST to look up some information -- the model should contain everything. This means that there are often three similar type definitions, e.g. there's a `StructDeclAst`, then a `StructDecl`, then a `ConcreteStruct`.


## parser

The parser is pretty straightforward due to the language's syntax -- there is no lookahead anywhere.
There is no single tokenizer function, instead the parser usually knows exactly what it's looking for and tries to parse that, and anything else is an error.
The parser state consists only of the current index in the source text, the current indentation, and whatever stack of parser functions are currently being called.

The AST uses `Sym` instead of strings wherever possible. The `Sym` type is specially designed to represent valid noze identifiers compactly and doesn't work on arbitrary strings. Since there are less than 64 valid characters in an alpha identifier, up to 10 characters can be stored in the latter 60 bits of a 64-bit word. (Operators are even more compact since there are only 7 valid characters.)
For a longer string, we first look up in a dict to see if we've already converted it to a `Sym` -- if so we use that.
Otherwise, we store it in the dict. This ensures that there is only *one* copy of any string, meaning two `Syms` can be compared by word equality without dereferencing the pointer.

The parser is designed to exit at the first parse error.
The compiler then just reports that diagnostic and avoids type-checking.
This avoids having many cascading diagnostics from a single error.
Similarly, when a module has a diagnostic, modules that depend on it aren't checked.


## type-checker

### bogus

When type-checking something fails, we add a diagnostic, but we usually still have to return something. So we return something called `Bogus`.
There is a `Bogus` possibility for most things, including `Type` and `Expr`.
When the compiler sees that something is bogus, it knows that a diagnostic has already been added for it, so it avoids adding even more and usually returns bogus again.
For example, a call expression where one of the arguments is bogus will not report an error about failing to choose an overload.



### type-checking structs, specs, fun headers (`check.cpp`)

First, I should explain what `StructDecl` and `StructInst` are.
`StructDecl` is the struct that was actually declared. If the struct was a template, this will contain the template's own type parameters.
`StructInst` is a struct instantiation, with type parameters filled in (possibly by something else's type parameters).
Even a non-template struct will get a single `StructInst`.

When we create a `StructInst`, we also make a copy of the `StructDecl`'s body, with its type parameters replaced with the type arguments.

Each `StructDecl` contains a list of all insts -- this means that a given `StructInst` is created only once. Then two `StructInst`s are equal iff their pointers are equal.

`FunDecl` / `FunInst` and `SpecDecl` / `SpecInst` work the same way.


Since noze supoprts circular dependencies *within* a module, type-checking a module must take a few steps:

* First, we *create* all `StructDecl`s and `StructAlias`es, without filling in their bodies.
* Then, we create a dict of all structs (and aliases) in the current module.
* We create a MutArr `delayStructInsts` which will contain the StructInsts whose bodies could not be filled in because the corresponding `StructDecl` does not have a body yet either. (The `StructInst` creation was not delayed, only its body.)
* For each struct alias, we can now fill in what it aliases -- this does not depend on the body.
* We can check all spec decls without any fancy delaying, since specs depend on structs but not on other specs, and structs can't depend on specs. (But `delayStructInsts` is still used in this phase.)
* Now we can fill in the struct bodies. Filling in struct A's body may depend on struct B, but doesn't depend on struct B's body, so this can be done in any order.
* Now we can fill in all `StructInst` bodies that were delayed by `delayStructInsts`.

We the do a similar thing for functions: First we create all the functions without their bodies filled in, then fill in their bodies.

### type-checking function bodies (`checkExpr.cpp`)

Since noze is an expression-based language, a function body is a single expression, not a series of statements.
(Multiple lines in a row are actually nested `Seq` expressions.)
So we can simply the-check each expression recursively.


#### expected type

When checking an expression, we have an `Expected` representing the expected type (if any).
`Expected` is basically a wrapper around a `Opt<Type>`, and also stores any type arguments that we may be inferring if in a call.

In most situations the expected type is known:

```nz
f nat()
	<<some expression>>
```

In the above example we know that `<<some expression>>` is an `nat`, so `Expected` would have its type filled in already and the expression's job is to satisfy that.
We also have an expected type when providing an argument to a call (where all overloads have the parameter type in common).
Sometimes the expected type will start as `none`:

```nz
f nat()
	x = <<some expression>>
	...
```

Here `<<some expression>>` has no expected type. Its job is then to write its actual type to `expected`.

Now in the following:

```nz
f nat()
	x = when
		<<some condition>>
			<<expression a>>
		else
			<<expression b>>
```

(Here, `<<some condition>>` has an expected type of `bool` because `when` expects that.)
When evaluating `<<expression a>>`, we'll be inferring a type and write it to the `expected`.
Then when evaluating `<<expression b>>`, the same `expected` is used, and type will already be filled in -- this time we'll have an expected type.

Having an expected type is useful for resolving overloads based on return type.
Many expressions such as literals, lambdas, `new-actor`, `new`, `new-arr` also work best with an expected type.

Most expressions are straightforward to check given the above -- not calls!


#### type-checking calls (`checkCall.cpp`)

Calls are particularly complicated due to overloading and templates. Also, we want each argument to have an expected type whenever possible.

The basic algorithm is this:

* To start, all we know is the function name, the number of arguments, and optionally the expected return type (if there's an expected type when checking this call).
* Our first step is to filter on the first two -- we look up all functions with the correct name in the current module and all imported modules, and if they have the correct number of parameters, we add them to a list of candidates.
  - In this step we also look for any specs on the current function.
* Each candidate consists of a function declaration or spec use, and also a set of inferring type arguments. These are `SingleInferringType`s -- much like `Expected`, it's a mutable `Opt<Type>` that we'll fill in when we get to it.
* If an expected type exists:
  - Compare each candidate's return type to the expected type, and remove candidates that can't match.
  - The process of matching types also fills in the candidate's inferring type arguments.
* Then, we'll check each argument, in order. Before checking an argument we see if all remaining candidates agree on a parameter type -- if so, the argument will have an expected type.
* After checking the argument, if it didn't have an expected type, we'll do another type-matching of each candidate's parameter type and the actual argument type.
  - This filters out candidates that didn't expect the given argument type, increasing the chance that future arguments will have an expected type.
* We repeat this for every argument.
* At the end, there should be one candidate remaining.
  - If there are more than one, add a diagnostic listing them. Noze does not have template specialization, so it's an error if there are functions taking `int` and `?t`.
  - If there are no candidates remaining, re-fetch all candidates and add a diagnostic listing all of them.
* Then we must fill in any specs on the final candidate.
  Finding an implementation for a spec is much like checking a call, but simpler because there are no argument expressions to check.
* Finally, we check the candidate's function flags like `summon` and add diagnostics if we aren't allowed to call it.

These rules mean that specs and function flags have no effect on overload resolution. You can't have both `f void()` and `f void() noctx` and expect the compiler to choose between them for you.


## concretize

The concretize step takes a model containing templates and produces a concrete program with specialized instances of each function.
In addition to filling in type parameters, it does other kinds of specialization:

* When an expression evaluates to a constant, it replaces the expression with the constant.
* When an expression is a lambda, it does not create a dynamically-invoked function; it creates the closure. If this is invoked directly it avoids the virtual call and heap allocation.
* It also sometimes specializes a function based on an argument that is a constant. Currently this is pretty conservative, only specializing when every argument is constant.

This means that each expression has an associated optional `KnownLambdaBody`. When we call the lambda and it has a `KnownLambdaBody`, we'll specialize the body to a `ConcreteFun` of its own to call directly.
(A constant lambda works similarly.)


## builtin

Noze has no operators, but it does have functions whose body can't be expressed in noze itself. These are called `builtin` functions.

These are the most primitive functions such as `wrap-add` -- they do not rely on context and are typically a single machine instruction.

Similarly, types such as `int` are builtins -- they aren't composed of other types.
