(ns bsless.kanren.micro.impl.protocols)

(defprotocol IMPlus
  "Add two streams of results.
  A fair implementation interleaves them."
  (-mplus [this g]))

(defprotocol IBind
  "Interweave to streams of results.
  If you want to understand this read Kiselyov's paper on LogicM."
  (-bind [this g]))

(defmacro -inc [& rest]
  `(fn* ~'-inc [] ~@rest))

;;; MZero

(extend-type nil
  IBind
  (-bind [_ g] nil)
  IMPlus
  (-mplus [_ f] (f)))

(extend-type clojure.lang.Fn
  IBind
  (-bind [this g]
    (-inc (-bind (this) g)))
  IMPlus
  (-mplus [this f]
    ;; Interleaving is achieved by switching the order of arguments
    ;; The stream is ensured to be "cold" because it's wrapped in [[-inc]]
    (-inc (-mplus (f) this))))

(defn pull
  [f]
  (if (instance? clojure.lang.Fn f)
    (recur (f))
    f))

(defprotocol IUnify
  "Type based dispatch for terms unification.
  Every term specifies how it can unify with other objects in a given
  substitution."
  (-unify-terms [u v s] "Unify `u` with `v` in substitution `s`."))

(definterface ITreeTerm)

(defn tree-term? [x]
  (or
   (instance? clojure.lang.IPersistentCollection x)
   (instance? ITreeTerm x)))

(defprotocol ISubstitution
  "Base protocol for substitution behavior.
  A substitution implementation is defined by:
  [[-walk]]: triangular search semantics
  [[-unify-in]]: How to unify two variables in the substitution.
  [[-ext]]: Extending the substitution.
  [[-find]] and [[-lookup]]: lookup semantics."
  (-walk [s u] "Chase down `u` in `s` to its ground value.")
  (-unify-in [s u v] "Given that `u` and `v` need to be unified, unify
  them in the specific implementation of the substitution `s`.")
  (-ext [s u v] "Extend the substitution `s` to associate `v` to `u`.")
  (-find [s v] "Find `v` in `s`, returns entry.")
  (-lookup [s v] "Get `v` in `s` or nil."))

(defprotocol IBindable
  (-bindable? [this]))

(extend-protocol IBindable
  nil (-bindable? [_] false)
  Object (-bindable? [_] false))
