# Active Clojure

A library with various basic utilities for programming with Clojure.

## Usage

### Records

The `active.clojure.record` namespace implements a
`define-record-type` form similar to Scheme's [SRFI
9](http://srfi.schemers.org/srfi-9/).

### Conditions

The `active.clojure.condition` namespace implements *conditions*,
specifically crafted exception objects that form a protocol for
communicating the cause of an exception, similar to the condition
system in [R6RS Scheme](http://r6rs.org/).

### Lenses

The `active.clojure.lens` namespace implements *lenses*.  Lenses
provide a subtle way to access and update the elements of a structure
and are well-known in [functional programming
languages](http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html).

### Configuration

The `active.clojure.config` namespace implements application
configuration via a big map.

### Debugging

The `active.clojure.debug` namespace implements some useful debugging
tools such as a macro `pret` that prints and returns its argument.

### Pattern Matching

The `active.clojure.match` namespaces provides some syntactic sugar
for map matching around `core.match`.

## License

Copyright © 2014 Active Group GmbH

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
