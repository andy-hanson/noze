This file contains things that are *not* implemented but planned.
Most of these will probably take a lot of work. Vats / GC are a priority as these are central to the runtime.

## Vats

A noze program consists of several vats.
Vats are somewhat like processes in an operating system, in that they can have limited access to resources (memory / time). Unlike with an operating system, the vat system does not limit access to I/O; instead interfaces do.

Unlike memory protection between processes in an operating system, vats also do not require virtual addressing to safely separate memory, instead relying on the language's own safety. This makes them suitable for simple systems with no OS or virtual addressing hardware, e.g. a TLB.

Vats are useful for establishing priority of tasks and ensuring that one task can't overwhelm the system.
A *vat* consists of:
* The capacity to use a number of threads. (Threads aren't dedicated to any particular vat, but a vat can set a minimum / maximum number of threads. The total of all vats' minimum number of threads should not exceed the number of processors on the system.)
* A collection of tasks. This is not a queue, and all tasks in a vat are equal -- when it's time to run a new task, one is randomly chosen.
* An allocator / garbage collector, which may have vat-specific settings such as maximum allowed memory.

Although vats have separate garbage collection, they do not have segregated memory -- they can all look at objects the others have allocated. Vats communicate using `sendable` things like interfaces and futures. This means that we'll have to use a non-moving garbage collector, as garbage collection in one vat shouldn't prevent other vats from proceeding.

Vats allow you to have realtime requirements without having to write your entire program in a constrained way. If you have a high-priority task, you can put it in its own vat. This guarantees it:

* A minimum number of threads, to ensure that this vat is always able to run.
* Since it has its own collection of tasks, it won't be pre-empted by anything else.
* Since it has its own garbage collector, it will only pay for what it allocates. It may also be `nogc` ensuring that its tasks are never blocked by garbage collection.


## I/O

Unlike threads and memory, it's not the core runtime's job to restrict access to I/O; it's the particular I/O library's job.
There is no single solution good for all kinds of I/O, this needs to be considered for each I/O library added.

The language does this already by normally only allowing one to do I/O through and interface;
the provider of that interface could set a limit, such as only allowing a certain number of calls.
Different iface instances could be provided to different users so that each user will have their own limits.

I/O libraries should be written with the principle of least authority in mind; the interface for reading files should only have access to a certain directory, the interface for making HTTP requests should only have access to certain URLs. Very few APIs should be limitless by default.


## GC

The garbage collector must be non-moving to support vats looking at each other's memory while running concurrently.

In contrast to other garbage-collected languages, noze still has unsafe code. So it should be possible to add a `dealloc` function which tells the garbage collector early that some memory is no longer used. Potentially we could add a garbage collector setting where the vat promises that every task will `dealloc` everything

It will also be non-interrupting. Since noze code consists of many small tasks, we can wait until it's almost time for a GC, then set a flag that prevents running of new tasks.
(The threads that would be running those tasks can, instead of waiting, go service some other vat.)
When no more tasks are running, then we do a GC. Then our roots consist only of the tasks that have not yet run, as well as any external roots.

An external root is created when vat A sends an object to vat B. Vat B needs to remember its external roots from each other vat. This set need only include the *root* objects looked at, not everything ultimately reachable.

When a vat does garbage collection, it will start creating a new set of external roots from it to each other vat, which will hopefully be smaller than before. It won't update them immediately, instead it will swap them in at the end of the GC.

When a vat does GC, it only walks through its own objects. When it walks to an object, that object might be in static memory, in which case it should be ignored; static memory is immutable and thus can never point back into vat memory. That object might be in a different vat, in which case we'll add that to the new set of external roots we're building for that vat. Only if that object is in the same vat do we recursively walk over its children.

Marking objects means marking their corresponding bits in a bitset. 1 word corresponds to 1 bit; all objects should be word-aligned. The bitset is recreated in a GC, but we keep it and mark it more as we allocate. Manual deallocation can also unset bits.

Due to this design, there's no reason arbitrary parts of memory couldn't be `malloc`ed by unsafe code. This would have the same type as a normal object; the garbage collector will simply ignore anything that's not part of a known vat. (Unfortunately this means the GC won't detect memory corruption by default, though we could still try to detect whether a pointer looks valid or not.)

Since detecting what vat memory belongs to will be a common operation, it needs to be efficient. Luckily a virtual address usually has 48 usable bits, which is enough to encode the vat ID in there -- say there's a maximum of 255 vats. Static could be the only one allowed to have objects starting with 0x00..., vat 1 would have objects starting with 0x01..., vat 2 would have 0x02..., etc. Most objects that the GC traverses over (and only the GC cares about which vat an object belongs to) will be in the current vat or in static memory, so we would have those two as the fast path, then fall back to the slow path where we need to add an external root for another vat.

This design does not take care of circular references between vats. However, those are pretty hard to create -- mutable objects are generally not sendable. `fut` is, though, and it's possible that vat A's `fut` will be resolved by an object containing a pointer to vat B's `fut` which will be resolved by an object containing a pointer to the original `fut`. Vat A would never free its fut since vat B points to it, and the same goes for vat B. Solving this problem would require inter-vat cycle detection; perhaps some core runtime thread would look for these. This would require a vat during GC to mark which objects were reachable by itself as opposed to which ones were reachable by external roots only. Then if the cycle-detector sees that if vat A's objects are external-only, and so are vat B's, then it could unmark all of them. This is a hard problem though, and won't come up often in real code. So I'll hold of on trying to solve this.


### Stack allocation

Since noze has no globals, it's often easy to tell when a function can't possibly leak something.
If a function takes a type `t` and returns `u`, and the type `u` recursively can never contain a `t`, then the parameter of type `t` can never leak. I suspect this will cover 90% of cases.
So, if we see a `new` expression:
* If it's passed to a parameter and that parameter does not leak: we can stack-allocate it.
* If it's assigned to a local variable and that local variable does not have any use that leaks: we can stack-allocate it.
* Finally, if it's passed to a function that *does* leak: We can still check if that function's return value leaks, if not, we can stack-allocate this.

We might want add a `nogc` function attribute (the D language has the same keyword); these functions would be required to only use stack-allocated memory, or manually allocate and deallocate memory. (Manual allocation could still use the GC heap, or it could use malloc.) These would not be `noctx` as they could still throw exceptions and add tasks (tasks can be manually deallocated then they are taken out of the tasks collection).


## Reflection

I imagine reflection looking like so:

```nz
to-json void(p printer, t ?t)
	match type-of<?t>
		record r
			r.fields each \field
				... do something with the field ...
		union u
			...
		iface i
			...
```

`type-of<?t>` will evaluate to a *constant* value of type `type`, which is a union type.
A `type` is a record, union, or iface, but never a type parameter because we concretize.

The compiler can already handle constants well; since `type-of<?t>` is a constant, we can eliminate all but one branch of the `match`.
Similarly, `r.fields` should be a constant, so we should be able to unroll the `each` loop.
This means that the generated could should ultimately resumble hand-written code.

The ultimate goal is to be able to write a function that takes any iface as input, and hosts it at a URL.
To be able to do this, it needs to be able to either serialize, or also host, any parameter to the iface.
This will be hard to write, but would ultimately allow noze to be used for distributed computation.


## Linting

The compiler should have a builtin linter.
Once the `model` is generated, we walk over it a second time and add additional errors.
For example:

* Private function is never used
* Parameter or local variable is never used
* Unnecessary type arguments provided

There should also be an optional global-linting mode. This will consider your whole codebase and warn about *public* things that are never used, such as a field on a record that is created but never read from. This would need to distinguish between your own code and third-party libraries, since you probably wont' be using every public export of a third-party library, but you should be using all of your own code. (Even if you are writing a library yourself, you should be testing everything, thus using everything.)


## Debug printing

There should be a `debug` expression that allows non-`summon` functions to have side effects:

```nz
incr int(x int)
	debug
		summon-print "calling incr"
	x + 1
```

The D language has a similar feature to this, using the same keyword.

When these statements exist you should have to use `noze run --debug`, which also disables linting.
Otherwise it should be a lint error to leave debugging code in.

This allows one to call `summon` even in a non-`summon` function.
The compiler should not optimize `debug` statements away, so either the function has to be considered as-if `summon`, or we should simply never optimize things away when `--debug` is on.

