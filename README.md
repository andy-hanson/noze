### Building

First set up libfirm:

```
cd libfirm
make
cd ..
```

You can do a simple build using `./build.sh`, or an incremental build using `scons`.

For incremental builds with `cmake`, see the instructions at the top of `CMakeLists.txt`.
	Note: When editing a header file you should always `make clean`, as cmake does not recompile everything correctly.

### Running

`noze run test/a.nz -- arg`

`noze test` is a shorthand for `noze run test/a.nz`.
