The readme describes the respository -- see language.md for a description of the language.

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

`noze run test/a.nz -- arg`

`noze test` is a shorthand for `noze run test/a.nz`.


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
