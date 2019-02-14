# Active Clojure

A library with various basic utilities for programming with Clojure.

## Usage

### Records

The `active.clojure.record` namespace implements a
`define-record-type` form similar to Scheme's [SRFI
9](http://srfi.schemers.org/srfi-9/).

### Records (Spec)

The `active.clojure.record-spec` namespace implements a
`define-record-type` form similar to Scheme's [SRFI
9](http://srfi.schemers.org/srfi-9/) (similar to `active.clojure.record`).

Additionally, this form creates Clojure Specs according to provided metadata.

Example:

```clojure
(ns your.namespace
  (:require [active.clojure.record-spec :as rs]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::color #{:hearts :diamonds :spades :clover})
(s/def ::number #{:ace :two :three :four :five :six :seven :eight :nine :ten :jack :queen :king})

(rs/define-record-type card
  (make-card number color) card?
  [^{:spec ::number} number card-number
   (^{:doc "Field with spec, lens and doc." :spec ::color}
   color card-color card-color-lens)])
```

This defines the following Specs (aside from what the regular 
`active.clojure.record`s already define):

* `::card`, a Spec which conforms values that are instances of a card (see 
  example below).
* Specs `::card-number` and `::card-color` for accessors. Note that these names
  are **not** based on the given accessor names but rather a concatenation of
  the record type and field names.
* Spec for the constructor function.

```clojure
(s/explain ::card (make-card :four :spades))
;; => Success!
(s/explain ::card {:card-number :four :card-color :spades})
;; => val: {:card-number :four, :card-color :spades} fails spec:
;;    :your.namespace/card predicate: card?
```

If you don't specify a spec, it defaults to `any?`.
Further, this enables generating data based on record definitions:

```clojure
(gen/sample (s/gen ::card) 3)
;; => (#your.namespace.card{:card-number :four, :card-color :hearts}
;;     #your.namespace.card{:card-number :six, :card-color :hearts}
;;     #your.namespace.card{:card-number :queen, :card-color :diamonds}
```

If instrumentation is enabled (via `clojure.spec.test.alpha/instrument`), the
constructor is checked using the specs provided for the selector functions:

```clojure
;; Does not get checked without instrument.
(make-card :ace :heartz)
;; => #your.namespace.card{:card-number :ace :card-color :heartz}

;; Now, with instrumentation.
(stest/instrument)

(make-card :ace :heartz)
;; =>
;; 1. Unhandled clojure.lang.ExceptionInfo
;; Spec assertion failed.
;; ...
;; Problems: 

;; val: :heartz
;; in: [1]
;; failed: #{:spades :diamonds :hearts :clover}
;; spec: :your.namespace/color
;; at: [:args :color]
```

**NOTE**: You must keep track of your namespaced keywords manually (e.g. the
keywords you use for defining specs). We do not check for collisions, so former
definitions with the same name will be overwritten!

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

The `active.clojure.match` namespace provides some syntactic sugar
for map matching around `core.match`.

## License

Copyright © 2014 Active Group GmbH

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
