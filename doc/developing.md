See [implementation](./implementation.md) for information about the code.


## Incremental builds

You can optionally use [`scons`](https://scons.org/) for quick rebuilds.
This does not build/rebuild libFIRM for you.
The scons build is specified in `SConstruct`.


## Test

`./test.sh` runs `scons` then `test/test.nz` which contains all the tests.

See `./test.sh --help` for more info.


## Debugging


### Debugging the compiler

`lldb -- noze run test/test.nz`


### Debugging output code

Currently there is no mapping from the output code back to the original source, so you'll have to debug C.

```sh
noze build test/test.nz
lldb -- test/test
```

To break on exceptions: `breakpoint set --func-regex throw`.
