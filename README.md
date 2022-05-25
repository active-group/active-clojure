# Active Clojure

A library with various basic utilities for programming with Clojure.

[![Clojars Project](https://img.shields.io/clojars/v/de.active-group/active-clojure.svg)](https://clojars.org/de.active-group/active-clojure)
[![Actions Status](https://github.com/active-group/active-clojure/workflows/Tests/badge.svg)](https://github.com/active-group/active-clojure/actions)
[![cljdoc badge](https://cljdoc.org/badge/de.active-group/active-clojure)](https://cljdoc.org/d/de.active-group/active-clojure/CURRENT)

### Breaking changes in version 0.38

- For an RTD record `MyRecord`, `(MyRecord :meta)` will no longer
  return a meta data map. Use `(meta #'MyRecord)` instead.

### Breaking changes since version `0.28.0`
- Clojure version 1.9.0 or higher and Clojurescript version 1.9.542 or higher
  are required.
- The namespace of ClojureScript's `define-record-type` has changed from
  `active.clojure.record` to `active.clojure.cljs.record`.
- To make sure that the right `active-clojure` version gets picked up by
  Leiningen, you should exclude previous `active-clojure` that are included in
  the dependencies transitively by adding `:exclusions [active-clojure]` to
  libraries that come with the dependency.  When in doubt, check `lein deps :why
  active-clojure`.
- Since selectors are now lenses by default, the previously used "lens triples"
  are no longer valid. You need to remove the parens and the third element and
  use the selector instead of the name of the lens everywhere in your code.

## Usage

## Records

The `active.clojure.record` namespace implements a
`define-record-type` form similar to Scheme's [SRFI
9](http://srfi.schemers.org/srfi-9/).

Example: A card consists of a number and a color

```clojure
(ns namespace
  (:require [active.clojure.record :as r]))

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
(define-record-type Test {:arrow-constructor? false} (make-test a) ...)
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

#### Java Classes, RTD records

By default `define-record-type` generates new types in the host
language (Java for Clojure or JavaScript for ClojureScript), just
like `defrecord` does. That can be changed by specifying either
`:java-class? false`, or `rtd-record? true` options like so:

```clojure
(r/define-record-type Foo {:rtd-record? true}
 ...)
```

These records have the advantage, that a hot code reload of the same
definition will not create a new type in the host language. So record
values created before the code reload are still compatible with the
record type, unless its fields have changed of course.

You cannot define protocol implementations for these kinds of record
types, but you can use multi methods. Use the defined type and the
result of `r/record-type` as the dispatch value for that.

Above options may not work with RTD records:

- Arrow: RTD records don't provide an arrow constructor
- Map implementation: RTD records don't implement the map interface
- Interfaces: No interfaces are implemented, you cannot provide your own
  implementations for RTD records

#### Meta data

You can provide meta data via `(define-record-type ^{:foo "bar"}
MyRecord)`. This meta data is then "inherited" to all created symobls
(like `->MyRecord`).

If you use an RTD record (`:java-class?`, `:rtd-record?` options), this data
is also retrievable via `(meta #'MyRecord)`.



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

### Applicative Validation

The `active.clojure.validation` namespace provides utlities for
applicative data validation.  It is useful to create validation
functions that collect all errors that occured (as opposed to finding
only specific or one error) in a purely functional way.

The main building-blocks are the `validate-*` functions and
`validate`.

#### Applicative Validation: Example
An idiomatic example, hiding the actual record constructor and
exposing only a validated record constructor:

```clojure
(ns validation
  (:require [active.clojure.record :as r]
            [active.clojure.validation :as v]))

(r/define-record-type Config
  ^:private make-config config?
  [host        config-host
   port        config-port
   mode        config-mode
   admin-users config-admin-users])
```

Here, we define the record-type `Config`.  We want to have the
following rules:

- The `host` is a non-empty string
- The `port` must be an integer between 0 -- 65536
- The `mode` must be one of `:dev`, `:test`, and `:prod`
- the `admin-users` must be a sequence of non-empty strings.

First, we define a validator for ports which is not already included
in the library:

```clojure
(defn validate-port
  "Given a `candidate` value and an optional `label`, validates that
  `candidate` is between ]0 65536[."
  [candidate & [label]]
  (v/make-validator candidate
                    (fn [candidate]
                      (and (< 0 candidate)
                           (> 65536 candidate)))
                    ::port
                    label))
```

`make-validator` returns a function that checks if the value is valid
and returns either a `ValidationSuccess` or a `ValidationFailure`.
This can then be combined with other validators to create a validated
constructor for `Config`:

```clojure
(defn create-config
  "Creates a validated [[Config]], wrapped in a 'ValidationSuccess'.  If
  any arguments are invalid, returns a 'ValidationFailure' holding all
  'ValidationError's."
  [host port mode admin-users]
  (v/validation make-config
                (v/validate-non-empty-string host :host)
                ;; NOTE: We could also check for pos-int int
                ;; `validate-port`, this is intended to show the
                ;; `validate-all` combinator.
                (v/validate-all [v/validate-pos-int validate-port] port :port)
                (v/validate-one-of #{:dev :test :prod} mode :mode)
                (v/sequence-of v/validate-non-empty-string admin-users :admin-users)))
```

This is in large parts pretty straight forward.  We will go through
the parts of this expression one by one.

- `(v/validation make-config <validations>)` means that, given all
  `<validations>` are `ValidationSuccess`es, call the function
  `make-config` with the validated candidate values.  The
  `make-config` function is curried automatically.
- `(v/validate-non-empty-string host :host)` usese the
  `validate-non-empty-string` form the validation library and checks
  if `host` is a string and not empty.  If it fails, it will keep
  `:host` as a label to refer back to the argument.  All labels are
  optional, but it is a good idea to state a label if you want to map
  back from error to cause.
- `(v/validate-all [v/validate-pos-int validate-port] port :port)`
  uses the `v/validate-all` combinator to say that 'the candidate must
  satisfy all validations, `v/validate-pos-int` and `validate-port`.
  It will use both validators on the candidate and combine both errors
  if there are any into one `ValidationFailure`.
- `(v/validate-one-of #{:dev :test :prod} mode :mode)` validates that
  `mode` is in the specified set of values.
- `(v/sequence-of v/validate-non-empty-string admin-userse
  :admin-users`) also pretty much does what it says on the label: It
  validates that `admin-users` is a sequence of values, each of which
  satisfy the `validate-non-empty-string` validation.

Lets look at some results:

```clojure
;; Valid arguments, returns a ValidationSuccess holding the validated candidate.
(create-config "host" 8888 :dev ["user1" "user2"])
;; => #active.clojure.validation/ValidationSuccess{:candidate
;;      #validation/Config{:host        "host"
;;                         :port        8888
;;                         :dev-mode?   true
;;                         :admin-users ["user1" "user"2]}}
```

Hopefully the most common case: All arguments are valid, therefore the
whole validation succeeds and returns the validated candidate value,
wrapped in a `ValidationSuccess`.

```clojure
;; Every argument is invalid, returns a ValidationFailure with all ValidationErrors.
(create-config "" -1 :staging ["user1" ""])
;; => #active.clojure.validation/ValidationFailure{:errors
;;      [#active.clojure.validation/ValidationError{:candidate ""
;;                                                  :message   :active.clojure.validation/non-empty-string
;;                                                  :label     :host}
;;       #active.clojure.validation/ValidationError{:candidate -1
;;                                                  :message   :active.clojure.validation/pos-int
;;                                                  :label     :port}
;;       #active.clojure.validation/ValidationError{:candidate -1
;;                                                  :message   :validation/port
;;                                                  :label     :port}
;;       #active.clojure.validation/ValidationError{:candidate :staging
;;                                                  :message   [:active.clojure.validation/one-of #{:prod :test :dev}]
;;                                                  :label     :port}
;;       #active.clojure.validation/ValidationError{:candidate ""
;;                                                  :message   :active.clojure.validation/one-of #{:prod :test :dev}
;;                                                  :label     [:admin-users 1]}]}
```

The dire case in which each argument is invalid.  Note that the result
is a `ValidationFailure` that contains a sequence of
`ValidationError`s.  Each error tells us which candidate was causing
the error (`:candidate`), which validation was violated (`:message`)
and gives us a `:label` to refer back to the cause of the error.  Also
note that the `-1` shows up twice.  This is to be expected, because it
violates both validations of our `validate-all` clause.

```clojure
;; Only some arguments are invalid
(create-config "" 65537 :dev ["user1" ""])
;; => #active.clojure.validation/ValidationFailure{:errors
;;      [#active.clojure.validation/ValidationError{:candidate ""
;;                                                  :message   :active.clojure.validation/non-empty-string
;;                                                  :label     :host}
;;       #active.clojure.validation/ValidationError{:candidate 65537
;;                                                  :message   :validation/port
;;                                                  :label     :port}
;;       #active.clojure.validation/ValidationError{:candidate ""
;;                                                  :message   :active.clojure.validation/one-of #{:prod :test :dev}
;;                                                  :label     [:admin-users 1]}]}
```

This case shows the result of only some validations failing.  Take
note of the `:port` validation again.  This time it is only one error.
This is because it satisfies the `validate-pos-int` validation but not
our custom validation specifying the legal port range.

## Development

### Testing

The Clojure tests can be executed via

    lein test

For *auto-testing* the ClojureScript code, we use [figwheel-main](https://github.com/bhauman/figwheel-main). In a terminal, do

    lein fig

which starts a CLJS REPL. Opening

    http://localhost:9500/figwheel-extra-main/auto-testing

in a browser window will then run the tests and display the results. After every code change, it will automatically reload and re-run the tests, notifying you via the browser of the result.

## License

Copyright © 2014-2019 Active Group GmbH

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
