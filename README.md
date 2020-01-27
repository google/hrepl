# hrepl

`hrepl` is a tool that lets you interactively develop Haskell code using
[Bazel](https://haskell.build).  It uses Bazel to compile your code's
dependencies, and then loads your modules into an interpreter.

This is not an officially supported Google product.

- [Overview](#overview)
- [Using Build Targets](#using-build-targets)
- [Forwarding Command-line Flags](#forwarding-command-line-flags)

## Overview

To use `hrepl`, first `cd` into this repository, build the `//hrepl` target and
save the resulting binary somewhere convenient:

```shell
$ bazel build //hrepl
$ cp --no-preserve=mode bazel-bin/hrepl/hrepl ~/.local/bin
$ chmod +x ~/.local/bin/hrepl
```

You should also use a recent enough verson of `rules_haskell` in your
project's `WORKSPACE` file.  (See this repository's `WORKSPACE` file for an
example.)

Then, to load one or more targets in the interpreter, run `hrepl` within
your own Bazel project.  You may specify the Bazel label(s) of any Haskell libraries,
binaries or tests. For example, in the `rules_haskell` repository itself:

```shell
$ hrepl tests/binary-with-lib:lib
...
*Lib Lib>
```

Or, within a subdirectory and with [multiple targets](#multiple-build-targets):

```shell
$ cd tests/binary-with-lib
$ hrepl :lib :binary-with-lib
*Lib Lib Main>
```

You may also specify individual source files, which will cause hrepl to load
targets that declare those files in their srcs. (If there is more than one
possibility, it will choose arbitrarily.)

```shell
$ hrepl tests/binary-with-lib/Lib.hs
*Lib Lib>
```

After you modify the interpreted module(s), the `:reload` command will pick up
any changes. This approach is much faster than rebuilding with Bazel each time.

For more information about `hrepl`'s command-line flags, run `hrepl --help`.

## Using Build Targets

### Compiled Dependencies

You may also load modules from the dependencies of your target(s), using
`:module` or `import`. For example:

```shell
$ hrepl //some:library  # depends on the "split" package
Prelude Library> import Data.List.Split
Prelude Library Data.List.Split>
```

`hrepl` compiles those dependencies with `bazel` before starting the
interpreter. It behaves this way because it's not feasible to load every
transitive dependency (including third-party packages) into GHCi at once; and
due to the way Bazel works, there isn't always a clear distinction between
"installed" and "local" packages.

As a consequence, the interpreter is not aware of the source files of
dependencies, and will not pick up changes to them on `:reload`. Instead, you
will need to `:quit` and restart `hrepl`. The same is true for changes to
`BUILD` or `.bzl` files that affect your targets.

Note: `hrepl` will not let you load (compiled) modules from transitive dependencies
automatically. This behavior is similar to the build rules, which only expose
modules from targets listed directly in their `deps`.  To expose a transitive
dependency in the interpreter, pass `--package //label/of:dep`.

### Multiple build targets

You may load zero or more Bazel targets in the interpeter at once. For example,
to load two targets:

```shell
$ hrepl //your:target1 //another:target2
Prelude Target1 Target2>
```

`hrepl` will also interpret (i.e., not compile) any "intermediate" targets. For
example, suppose that `:target1` depends on `:dep` and `:dep` depends on
`:target2`. Then `hrepl` will interpret `:dep` as well, and `:reload` will pick
up any changes to `:dep` as well as to `:target1` and `:target2`. However,
`hrepl` will not expose the definitions in `:dep` by default. If you want to use
them, either specify those targets on the command-line or call `import`. For
example:

```shell
$ hrepl //your:target1 //another:target2
Prelude Target1 Target2> import Dep
Prelude Target1 Target2 Dep>
```

### Additional Compiled Targets

Alternately, you may tell `hrepl` to compile an unrelated target with the
`--package` flag. For example:

```shell
$ hrepl //your/haskell:target --package @stackage//:split
Prelude Target>
```

In that case, `@stackage//:split` will be compiled and available for
`import` in the interpreter:

```shell
Prelude Target> import Data.List.Split
Prelude Target Data.List.Split>
```

Similar to any dependencies of `:target`, it won't be reloaded unless you
manually `:quit` and restart the interpreter.

You may also use this flag to expose a dependency of a target without also compiling it.

## Forwarding Command-line Flags

`hrepl` supports forwarding flags to its subprocesses in several different ways.

### To GHC

You may pass compiler flags directly to `hrepl`. For example:

```
$ hrepl -XPackageImports -freverse-errors //some:target
```

To pass [RTS options] to GHC, use the `--with-rtsopts` flag, which takes a
space-separated list of flags. For example:

```shell
$ hrepl --with-rtsopts='-t -S'  //some:target
```

does the equivalent of `ghc +RTS -t -S -RTS`.

[RTS options]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#setting-rts-options-on-the-command-line

### To Bazel

You can use `--bazel-args=--some-bazel-params` to make `hrepl` pass certain
flags in each call to `bazel`.

`--bazel-args` takes a space-separated list of arguments. If it's specified
multiple times, the values will accumulate. For example, `--bazel-args='-c opt'`
is equivalent to `--bazel-args=-c --bazel-args=-opt`. As a special shortcut,
`hrepl` supports directly passing the Bazel `-c` flag to it.

For example:

```shell
$ hrepl --bazel-args='-c opt' //your/haskell:library
$ hrepl -c opt //your/haskell:library`
```
