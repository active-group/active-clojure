# Active Clojure

A library with various basic utilities for programming with Clojure.

[![Clojars Project](https://img.shields.io/clojars/v/active-clojure.svg)](https://clojars.org/active-clojure)

## Usage

## Records

The `active.clojure.record` namespace implements a
`define-record-type` form similar to Scheme's [SRFI
9](http://srfi.schemers.org/srfi-9/).

Example: A card consists of a number and a color

```clojure
(ns namespace
  (:require [active.clojure.clj.record :as r]))

(r/define-record-type Card
  (make-card number color)
  card?
  [number card-number
   color card-color])

;; Creating a record with field values 3 and "hearts"
(def card-1 (make-card 3 "hearts"))
;; Get number of this card via selector
(card-number card-1)
;; => 3
;; Predicate test
(card? card-1)
;; => true
(card? "3 of Hearts")
;; => false
```

### Options

You can provide additional options in an option-map as second argument to `define-record-type`.

#### Specs

By providing a value to the option key `:spec`, a spec for the record type is created.
The fields of records can also be "spec'd" via meta information.

```clojure
(spec/def ::color #{:diamonds :hearts :spades :clubs})

(defn is-valid-card-number?
  [n]
  (and (int? n)
       (> n 0) (< n 14)))

(r/define-record-type Card
  {:spec ::card}
  (make-card number color)
  card?
  [^{:spec is-valid-card-number?} number card-number
   ^{:spec ::color} color card-color])

(spec/valid? ::card (make-card 5 :hearts))
;; => true
(spec/valid? ::card (make-card 5 "hearts"))
;; => false
(spec/explain ::card (make-card 5 "hearts"))
;; => val: #namespace.Card{:number 124, :color "hearts"} fails spec: :namespace/card
;;    predicate: (valid? :namespace/color (card-color %))
```

To use `spec/def`, `spec/valid?`, and `spec/explain` you have to require `clojure.spec.alpha`
in your `ns` form.

You also get a spec for the constructor function. If instrumentation is enabled
(via `clojure.spec.test.alpha/instrument`), the constructor is checked using the specs
provided for the selector functions:

```clojure
;; Does not get checked without instrument.
(make-card 20 :hearts)
;; => #namespace.Card{:number 20 :color :hearts}

;; Now, with instrumentation.
(clojure.spec.test.alpha/instrument)

(make-card 20 :hearts)
;; => Spec assertion failed.
;;
;; Spec: #object[clojure.spec.alpha$regex_spec_impl$reify__2436 0x31346221
;; "clojure.spec.alpha$regex_spec_impl$reify__2436@31346221"]
;; Value: (20 :hearts)
;;
;; Problems:
;;
;; val: 20
;; in: [0]
;; failed: is-valid-card-number?
;; at: [:args :number]
```

#### Non generative option

If you provide a value (uid) to the `nongenerative` option,
the record-creation operation is nongenerative i.e.,
a new record type is created only if no previous call to
`define-record-type ` was made with the uid.
Otherwise, an error is thrown.
If uid is `true`, a uuid is created automatically.
If this option is not given (or value is falsy),
the record-creation operation is generative, i.e.,
a new record type is created even if a previous call
to `define-record-type` was made with the same arguments.

#### Arrow constructor

Default is `true`.

If you provide the key:val pair `:arrow-constructor?`:`false`,
the creation of the arrow-constructor of the `defrecord` call is omitted,
i.e.

```clojure
(define-record-type Test {:no-arrow-constructor? true} (make-test a) ...)
```
won't yield a function `->Test`.

#### Map protocol

Default is `true`.

If you don't want your records to implement the Map-protocols (in *Clojure*
these are `java.util.Map` and `clojure.lang.IPersistentMap`, in *ClojureScript*
`IMap` and `IAssociative`), you can provide the key:val pair
`:map-protocol?`:`false` to the options map.

#### Remove default interfaces/protocols

There are a number of interfaces, that our records defaultly implement (like
e.g. aforementioned `java.util.Map`). Providing key:val pair
`:remove-interfaces`:`[interface1 interface2 ...]` will prevent the
implementations of the given interfaces.

#### Providing own implementations of interfaces and protocols

You can implement protocols and interfaces with the
`define-record-type`-statement:

```clojure
(defprotocol SaySomething
  (say [this]))

(r/define-record-type Card
  (make-card number color)
  card?
  [number card-number
   color card-color]
  SaySomething
  (say [this] (str "The card's color is " (card-color this))))

(say (make-card 3 :hearts))
```

You can also override the defaultly implemented interfaces/protocols by the same
means. You don't have to provide every method of a default interface, those left
out by you will remain the default ones.

#### Java Class

Default is `true`.

If you provide the key:val pair `:java-class?`:`false`, no java class is created
for the given type, and instead a `record-type-descriptor` is created.

#### Meta information

If `java-class?` is `false`, you can provide meta information with keyword `:meta`.
You can use the type's name to retrieve these infos: `(MyRecord :meta)`.

### Lenses

The `active.clojure.lens` namespace implements *lenses*.  Lenses
provide a subtle way to access and update the elements of a structure
and are well-known in [functional programming
languages](http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html).

#### Records example

If you want to update only one field in a record, it is cumbersome to write out
the whole make-constructor expression:

```clojure
(r/define-record-type Person
  make-person
  person?
  [name person-name
   age person-age
   address person-address
   job person-job])

(def mustermann (make-person "Max Mustermann" 35 "Hechinger Straße 12/1, 72072 Tübingen"
                             "Software Architect"))

(make-person "Max Maier"
             (person-age mustermann)
             (person-address mustermann)
             (person-job mustermann))
```

With lenses you can set and update fields easily:

```clojure
(lens/shove mustermann
            person-name
            "Max Maier")

(lens/overhaul mustermann
               person-age
               inc)
```

**Note:** The `lens` functions don't alter the given record but create and return
a new one.

You can even combine lenses to update records inside records:

```clojure
(r/define-record-type Address
  make-address
  adress?
  [street address-street
   number address-number
   city address-city
   postalcode address-postalcode])

(def mustermann (make-person "Max Mustermann" 35
                             (make-address "Hechinger Strasse" "12/1"
                                           "Tübingen" 72072)
                             "Software Architect"))

(lens/shove mustermann
            (lens/>> person-address address-street)
            "Hechinger Straße")
```

### Conditions

The `active.clojure.condition` namespace implements *conditions*,
specifically crafted exception objects that form a protocol for
communicating the cause of an exception, similar to the condition
system in [R6RS Scheme](http://r6rs.org/).

### Configuration

The `active.clojure.config` namespace implements application
configuration via a big map.

### Debugging

The `active.clojure.debug` namespace implements some useful debugging
tools such as a macro `pret` that prints and returns its argument.

### Pattern Matching

The `active.clojure.match` namespace provides some syntactic sugar
for map matching around `core.match`.

### Higher-order Functions

The `active.clojure.functions` namespace provides the same higher order
functions that `clojure.core` does, but implemented via records and
`IFn`, so that the returned "functions" are `=` if created with `=` arguments.

These can be very handy for using React-based libraries like [Reacl](https://github.com/active-group/reacl),
which can optimize work based on the equality of values.

### Monad

An example usage of the `active.clojure.monad` namespace can be found at https://github.com/active-group/active-clojure-monad-example

## License


Copyright © 2014-2019 Active Group GmbH

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
