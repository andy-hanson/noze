The readme describes the repository as a whole.

See [doc/language.md](doc/language.md) for a description of the language
and [doc/implementation.md](doc/implementation.md) for an overview of the code.

WARN: The language is not usable yet. Most programs you might try writing will result in a compiler crash on a `todo`.


### Building

First set up libFIRM:

```sh
cd libfirm
make
cd ..
```

You can then do a simple build using `./build-once.sh`, or incremental builds using `scons`.
You can do an optimized build using `./build-optimized.sh` --
this is not recommended as it removes assertions, making it less safe.
(libFIRM is currently not used at runtime, noze compiles to C instead.
But libfirm support is planned and you can't build without it.)


#### Linting

```sh
sudo luarocks install lfs lfs moonscript
./lint.moon
```


### Running


* `noze build foo.nz` compiles the file `foo.nz` and either prints compile errors,
  or writes to C source code `foo.c` and an executable file `foo`.
* `noze run foo.nz` does the same as build, then runs the resulting executable (if the build succeeded).
* `noze run foo.nz -- arg` provides an argument `arg` to the program. The `--` is mandatory.
* `noze test` is shorthand to run the test program in `test/a.nz`.

The directory of the file you pass in must be the root of the whole program --
you can't import from any parent directory of that.
(Excluding the modules in `noze/include` which are always available.)


### Editing

To install the Sublime Text syntax:

```sh
bash -llc 'ln -s `pwd`/noze.sublime-syntax ~/.config/sublime-text-3/Packages/User/noze.sublime-syntax'
bash -llc 'ln -s `pwd`/syntax_test_noze.nz ~/.config/sublime-text-3/Packages/User/syntax_test_noze.nz'
```
