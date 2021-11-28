(ns io.github.bsless.kanren.micro
  (:require
   [io.github.bsless.kanren.micro.impl.protocols
    :as p
    :refer [-bind -mplus -inc]]
   [io.github.bsless.kanren.micro.impl.choice]
   [io.github.bsless.kanren.micro.impl.lvar :as lvar]
   [io.github.bsless.kanren.micro.impl.lcons :as lcons]))

(defn lvar
  {:inline
   (fn
     ([] `(lvar/lvar))
     ([name] `(lvar/lvar ~name))
     ([name unique] `(lvar/lvar ~name ~unique)))
   :inline-arities #{1 2 3}}
  ([] (lvar/lvar))
  ([name] (lvar/lvar name))
  ([name unique] (lvar/lvar name unique)))

(definline lvar? [x] `(lvar/lvar? ~x))

(definline lcons [a d] `(lcons/lcons ~a ~d))
(defmacro llist [& args] `(lcons/llist ~@args))

(defmacro bind
  ([a g] `(-bind ~a ~g))
  ([a g & g-rest]
   `(bind (-bind ~a ~g) ~@g-rest)))

(defmacro mplus
  ([e] e)
  ([e & e-rest]
   `(-mplus ~e (-inc (mplus ~@e-rest)))))

(defn call-fresh
  [f]
  (fn goal-fresh [s] ((f (lvar)) s)))

(defn -disj
  [g1 g2]
  (fn goal-disjunction [s] (mplus (g1 s) (g2 s))))

(defn -conj
  [g1 g2]
  (fn goal-conjunction [s] (bind (g1 s) g2)))

(definline walk [s u] `(p/-walk ~s ~u))

(definline bindable? [x] `(p/-bindable? ~x))

(defn unify
  [s u v]
  (p/-unify-in s u v))

(defn ===
  "Goal that will attempt to unify `u` and `v`."
  [u v]
  (fn goal-unify [s] (unify s u v)))

(defonce +make-empty-state-fn+ (volatile! nil))

(defn make-empty-state [] (@+make-empty-state-fn+))
(defn set-empty-state-fn! [f] (vreset! +make-empty-state-fn+ f))

(comment
  (def empty-state {})
  (require '[io.github.bsless.kanren.micro.impl.substitution])

  (defn fives [x] (-disj (=== x 5) (fn [s] (fn [] ((fives x) s)))))

  (into [] (take 1) ((call-fresh fives) empty-state))

  (defn sixes [x]
    (-disj (=== x 6) (fn g [s] (fn $ [] ((sixes x) s)))))

  (def fives-and-sixes
    (call-fresh (fn [x] (-disj (fives x) (sixes x)))))

  (into [] (take 3) (fives-and-sixes empty-state)))

