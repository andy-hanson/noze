### Building

First set up libfirm:

```
cd libfirm
make
cd ..
```

You can do a simple build using `./build-once.sh`, or incremental builds using `scons`.

### Running

`noze run test/a.nz -- arg`

`noze test` is a shorthand for `noze run test/a.nz`.
