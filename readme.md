The readme describes the repository as a whole.

See [doc/language.md](doc/language.md) for a description of the language
and [doc/implementation.md](doc/implementation.md) for an overview of the code.

WARN: The language is not usable yet!
Most programs you might try writing will result in a compiler crash on a `todo`.


### Build and install

The only prerequisite for building is a C++ compiler aliased to `c++`.

Build with `./build.sh`.

This builds libFIRM, then builds noze itself.
(libFIRM is currently not used at runtime, noze compiles to C instead.
But libfirm support is planned and you can't build without it.)

Then add `bin` to your PATH.
WARN: `bin/node` depends on `include` and `libfirm` being in its grandparent directory.


### Usage

See `noze help` for all commands.


### Editing

It's recommended to install the Sublime Text syntax.
This will help you get used to the language.
It also supports go-to-definition, which includes all builtin functions (including things like `+`),
so you can go to their definitions to read documentation.

```sh
bash -llc 'ln -s `pwd`/noze.sublime-syntax ~/.config/sublime-text-3/Packages/User/noze.sublime-syntax'
```
