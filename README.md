Aplite
======

An embedded, domain-specific language for high-performance web applications.
[This paper](http://haste-lang.org/pubs/haskell16.pdf) explains the general
idea behind it.

Installation
------------

First, make sure that you have version 0.6 or newer of the Haste compiler
installed.
See the [build instructions](https://github.com/valderman/haste-compiler/blob/master/doc/building.md)
for information about how to actually make this work if you're building from source.

Then, fetch a few hacked-up dependencies as well as Aplite itself:
```
$ git clone https://github.com/valderman/syntactic.git
$ git clone https://github.com/valderman/open-typerep.git
$ git clone https://github.com/valderman/aplite.git
```

Finally, install the dependencies plus Aplite:
```
$ haste-cabal install ./aplite ./syntactic ./open-typerep
```
