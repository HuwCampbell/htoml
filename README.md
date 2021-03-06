htoml
=====

A [TOML](https://github.com/mojombo/toml) parser library in
[Haskell](http://haskell-lang.org).

TOML is the obvious, minimal configuration language by
[Tom Preston-Werner](https://github.com/mojombo).
It is an alternative to the [XML](http://www.w3.org/TR/REC-xml/),
[YAML](http://www.yaml.org/spec/1.2/spec.html) and
[INI](http://en.wikipedia.org/wiki/INI_file) formats for the purpose of
configuration files, as the first two are too heavy for that prupose,
and the latter is underspecified.
Toml is to configuration files, like what Markdown is for rich-text.

This library aims to be compatible with the latest version of the
[TOML spec](https://github.com/mojombo/toml), currently that is
[v0.3.1](https://github.com/toml-lang/toml/releases/tag/v0.3.1).

The documentation for this package may (or may not) be found on
[Hackage](https://hackage.haskell.org/package/htoml).


### Quick start

Installing `htoml` is easy.

    cabal install htoml

In order to make your project depend on it you can add it as a
dependency in your project's cabal file.

To quickly show some features of `htoml` we start `GHCi` from the
root of the repository so it picks up configuration from the
`.ghci` file that lives there.

    git clone https://github.com/cies/htoml.git
    cd htoml
    cabal sandbox init  ;# initialize a cabal sandbox
    cabal install       ;# install the dependencies and build all
    cabal repl          ;# starts a sandbox-aware GHCi

We can immediately start exploring from the `GHCi` prompt.

    > txt <- readFile "benchmarks/example.toml"
    > let r = parseTomlDoc "" txt
    > r
    Right (fromList [("database",NTable (fromList [("enabled",NTValue (VBoolean True) [...]

    > let Right toml = r
    > toJSON toml
    Object (fromList [("database",Object (fromList [("enabled",Bool True) [...]

    > let Left error = parseTomlDoc "" "== invalid toml =="
    > e
    (line 1, column 1):
    unexpected '='
    expecting "#", "[" or end of input

Notice that some outputs are truncated, indicated by `[...]`.


### How to pull data from a TOML file after parsing it

Once you have sucessfully parsed a TOML file you most likely want to pull
some piecces of data out of the resulting data structure.

To do so you have two main options. The first is to use pattern matching.
For example let's consider the following `parseResult`:

```haskell
Right (fromList [("server",NTable (fromList [("enabled",NTValue (VBoolean True))] ) )] )
```

Which could be pattern matched with:

```haskell
case parseResult of
  Left  _ -> "Could not parse file"
  Right m -> case m ! "server" of
    NTable mm -> case mm ! "enabled" of
      VBoolean b -> "Server is " ++ (if b then "enabled" else "disabled")
      _ -> "Could not parse server status (Boolean)"
    _ -> "TOML file does not contain the 'server' key"
```

The second main option is to use the `toJSON` function to transform the data
to an [Aeson](https://hackage.haskell.org/package/aeson) data structure,
after which you can use your Aeson toolbelt to tackle the problem. Since
TOML is intended to be a close cousin of JSON this is a very practical
approach.

Other ways to pull data from a parsed TOML document will most likely
exist; maybe the `lens` library can give great results in some cases.
But I have no experience with them.


### Version contraints of `htoml`'s dependencies

If you encounter any problems because `htoml`'s dependecies are
constrained either too much or too little, please
[file a issue](https://github.com/cies/htoml/issues) for that.

I will try to have `htoml` included in [Stackage](http://stackage.org)
as soon as it is reviewed by the community. Stackage provides a very
attractive solution to most (dependency) version conflicts.


### Tests and benchmarks

The test suite is build by default, `cabal configure --disable-tests` disables them.
The benchmark suite is not run by default, `cabal configure --enable-benchmarks` enables them.

With `cabal build` both of these suites are build as executables and
put somewhere in `dist/`. Passing `--help` to them will reveal their
options.

[BurntSushi's language agnostic test suite](https://github.com/BurntSushi/toml-test)
is embedded in the test suite executable.  Using a shell script (that
lives in `test/BurntSushi`) the latest tests can be fetched from
BurntSushi's repository.


### Contributions

Most welcome! Please raise issues, start discussions, give comments or
submit pull-requests.
This is one of the first Haskell libraries I wrote, any feedback is
much appreciated.


### Features

* Compatibility to the TOML spec is proven by an extensive test suite
* Incorporates [BurntSushi's language agnostic test suite](https://github.com/BurntSushi/toml-test)
* Has an internal representation that easily maps to JSON
* Provides an [Aeson](https://hackage.haskell.org/package/aeson)-style JSON interface (suggested by Greg Weber)
* Useful error messages (thanks to using Parsec over Attoparsec)
* Understands arrays as described in [this issue](https://github.com/toml-lang/toml/issues/254)
* Fails on mix-type arrays (as per spec)
* Comes with a benchmark suite to make performance gains/regressions measurable
* Tries to be well documented (please raise an issue if you find documentation lacking)


### Todo

* Release a stable 1.0 release and submit it to [Stackage](http://stackage.org)
* More documentation
* Make all tests pass (currently some more obscure corner cases don't pass)
* Add property tests with QuickCheck (the internet says it's possible for parsers)
* Extensively test error cases
* Try using Vector instead of List (measure performance increase with the benchmarks)
* See how lenses may (or may not) fit into this package


### Acknoledgements

Originally this project started off by improving the `toml` package by
Spiros Eliopoulos.


### Copyright and licensing

This package includes BurntSushi's language agnostic
[TOML tests](https://github.com/BurntSushi/toml-test), which are WTFPL
licensed.

The TOML examples that are used as part of the benchmarks are copied
from Tom Preston-Werner's TOML spec which is MIT licensed.

For all other files in this project the copyrights are specified in the
`htoml.cabal` file, and are distributed under the BSD3 license as found
in the `LICENSE` file.
