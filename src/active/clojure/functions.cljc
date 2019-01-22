(ns active.clojure.functions
  "Redefines higher order functions and function combinators from
  clojure.core via applicable records (ifn? but not fn?). The
  advantage is, that those objects compare = if they are created from
  equal arguments. Disadvantages are that they are probably a bit
  slower. They also don't implement some additional protocols like
  Runnable yet."
  (:refer-clojure :exclude [partial constantly comp complement juxt fnil every-pred some-fn bound-fn*
                            completing]))

#?(:cljs
   (defrecord ^:no-doc Partial [f_ args]
              IFn
              (-invoke [this] (apply f_ args))
              (-invoke [this a] (apply f_ (concat args (list a))))
              (-invoke [this a b] (apply f_ (concat args (list a b))))
              (-invoke [this a b c] (apply f_ (concat args (list a b c))))
              (-invoke [this a b c d] (apply f_ (concat args (list a b c d))))
              (-invoke [this a b c d e] (apply f_ (concat args (list a b c d e))))
              (-invoke [this a b c d e f] (apply f_ (concat args (list a b c d e f))))
              (-invoke [this a b c d e f g] (apply f_ (concat args (list a b c d e f g))))
              (-invoke [this a b c d e f g h] (apply f_ (concat args (list a b c d e f g h))))
              (-invoke [this a b c d e f g h i] (apply f_ (concat args (list a b c d e f g h i))))
              (-invoke [this a b c d e f g h i j] (apply f_ (concat args (list a b c d e f g h i j))))
              (-invoke [this a b c d e f g h i j k] (apply f_ (concat args (list a b c d e f g h i j k))))
              (-invoke [this a b c d e f g h i j k l] (apply f_ (concat args (list a b c d e f g h i j k l))))
              (-invoke [this a b c d e f g h i j k l m] (apply f_ (concat args (list a b c d e f g h i j k l m))))
              (-invoke [this a b c d e f g h i j k l m n] (apply f_ (concat args (list a b c d e f g h i j k l m n))))
              (-invoke [this a b c d e f g h i j k l m n o] (apply f_ (concat args (list a b c d e f g h i j k l m n o))))
              (-invoke [this a b c d e f g h i j k l m n o p] (apply f_ (concat args (list a b c d e f g h i j k l m n o p))))
              (-invoke [this a b c d e f g h i j k l m n o p q] (apply f_ (concat args (list a b c d e f g h i j k l m n o p q))))
              (-invoke [this a b c d e f g h i j k l m n o p q r] (apply f_ (concat args (list a b c d e f g h i j k l m n o p q r))))
              (-invoke [this a b c d e f g h i j k l m n o p q r s] (apply f_ (concat args (list a b c d e f g h i j k l m n o p q r s))))
              (-invoke [this a b c d e f g h i j k l m n o p q r s t] (apply f_ (concat args (list a b c d e f g h i j k l m n o p q r s t))))
              (-invoke [this a b c d e f g h i j k l m n o p q r s t rest] (apply f_ (concat args (list a b c d e f g h i j k l m n o p q r s t) rest)))))

#?(:clj
   (defrecord ^:no-doc Partial [f_ args]
              clojure.lang.IFn
              (applyTo [this arglist] (apply f_ (concat args arglist)))
              (invoke [this] (apply f_ args))
              (invoke [this a] (apply f_ (concat args (list a))))
              (invoke [this a b] (apply f_ (concat args (list a b))))
              (invoke [this a b c] (apply f_ (concat args (list a b c))))
              (invoke [this a b c d] (apply f_ (concat args (list a b c d))))
              (invoke [this a b c d e] (apply f_ (concat args (list a b c d e))))
              (invoke [this a b c d e f] (apply f_ (concat args (list a b c d e f))))
              (invoke [this a b c d e f g] (apply f_ (concat args (list a b c d e f g))))
              (invoke [this a b c d e f g h] (apply f_ (concat args (list a b c d e f g h))))
              (invoke [this a b c d e f g h i] (apply f_ (concat args (list a b c d e f g h i))))
              (invoke [this a b c d e f g h i j] (apply f_ (concat args (list a b c d e f g h i j))))
              (invoke [this a b c d e f g h i j k] (apply f_ (concat args (list a b c d e f g h i j k))))
              (invoke [this a b c d e f g h i j k l] (apply f_ (concat args (list a b c d e f g h i j k l))))
              (invoke [this a b c d e f g h i j k l m] (apply f_ (concat args (list a b c d e f g h i j k l m))))
              (invoke [this a b c d e f g h i j k l m n] (apply f_ (concat args (list a b c d e f g h i j k l m n))))
              (invoke [this a b c d e f g h i j k l m n o] (apply f_ (concat args (list a b c d e f g h i j k l m n o))))
              (invoke [this a b c d e f g h i j k l m n o p] (apply f_ (concat args (list a b c d e f g h i j k l m n o p))))
              (invoke [this a b c d e f g h i j k l m n o p q] (apply f_ (concat args (list a b c d e f g h i j k l m n o p q))))
              (invoke [this a b c d e f g h i j k l m n o p q r] (apply f_ (concat args (list a b c d e f g h i j k l m n o p q r))))
              (invoke [this a b c d e f g h i j k l m n o p q r s] (apply f_ (concat args (list a b c d e f g h i j k l m n o p q r s))))
              (invoke [this a b c d e f g h i j k l m n o p q r s t] (apply f_ (concat args (list a b c d e f g h i j k l m n o p q r s t))))))


(defn partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."
  [f & args]
  (Partial. f args))

(letfn [(_lift-variadic [f fargs & args]
          (apply (apply f fargs) args))]
  ;; Note: this is most easiest way to lift a higher-order fn f, but often not the most efficient
  (defn lift-variadic [f & fargs]
    (partial _lift-variadic f fargs)))

(letfn [(_constantly [v & args]
          v)]
  (defn constantly
    "Returns a function that takes any number of arguments and returns x."
    [v]
    (partial _constantly v)))

(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  ([] identity)
  ([f] f)
  ([f g & fs]
   (apply lift-variadic clojure.core/comp f g fs)))

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  [f]
  (lift-variadic clojure.core/complement f))

(defn juxt
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]"
  [f & fns]
  (apply lift-variadic clojure.core/juxt f fns))

(defn fnil
  "Takes a function f, and returns a function that calls f, replacing
  a nil first argument to f with the supplied value x. Higher arity
  versions can replace arguments in the second and third
  positions (y, z). Note that the function f can take any number of
  arguments, not just the one(s) being nil-patched."
  ([f x] (lift-variadic clojure.core/fnil f x))
  ([f x y] (lift-variadic clojure.core/fnil f x y))
  ([f x y z] (lift-variadic clojure.core/fnil f x y z)))

(defn every-pred
  "Takes a set of predicates and returns a function f that returns true if all of its
  composing predicates return a logical true value against all of its arguments, else it returns
  false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical false result against the original predicates."
  [p & ps]
  (apply lift-variadic clojure.core/every-pred p ps))


(defn some-fn
  "Takes a set of predicates and returns a function f that returns the first logical true value
  returned by one of its composing predicates against any of its arguments, else it returns
  logical false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical true result against the original predicates."
  [p & ps]
  (apply lift-variadic clojure.core/some-fn p ps))

#?(:clj
   (letfn [(_bound-fn* [bindings f & args]
             (apply with-bindings* bindings f args))]
     (defn bound-fn*
       "Returns a function, which will install the same bindings in effect as in
  the thread at the time bound-fn* was called and then call f with any given
  arguments. This may be used to define a helper function which runs on a
  different thread, but needs the same bindings in place."
       [f]
       ;; Note: this cannot be done with lift-variadic, because
       ;; get-thread-bindings is side-effectful, and has to be called
       ;; now, not later.
       (let [bindings (get-thread-bindings)]
         (partial _bound-fn* bindings f)))))

(defn completing
  "Takes a reducing function f of 2 args and returns a fn suitable for
  transduce by adding an arity-1 signature that calls cf (default -
  identity) on the result argument."
  ([f] (completing f identity))
  ([f cf] (lift-variadic clojure.core/completing f cf)))
