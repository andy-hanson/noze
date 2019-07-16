The readme describes the respository -- see [doc/language.md](doc/language.md) for a description of the language.

### Building

First set up libfirm:

```
cd libfirm
make
cd ..
```

You can do a simple build using `./build-once.sh`, or incremental builds using `scons`.
libfirm is currently not used at runtime, noze compiles to C instead. But libfirm support is planned and you can't build without it.



### Running


* `noze build foo.nz` compiles the file `foo.nz` and either prints compile errors, or writes to C source code `foo.c` and an executable file `foo`.
* `noze run foo.nz` does the same as build, then runs the resulting executable (if the build succeeded).
* `noze run foo.nz -- arg` provides an argument `arg` to the program. The `--` is mandatory.
* `noze test` is shorthand to run the test program in `test/a.nz`.

The directory of the file you pass in must be the root of the whole program -- you can't import from any parent directory of that. (Excluding the modules in `noze/include` which are always available.)


### Editing

To install the Sublime Text syntax:

bash:

```
ln -s `pwd`/noze.tmLanguage ~/.config/sublime-text-3/Packages/User/noze.tmLanguage
```

fish:

```
ln -s (pwd)/noze.tmLanguage ~/.config/sublime-text-3/Packages/User/noze.tmLanguage
```
