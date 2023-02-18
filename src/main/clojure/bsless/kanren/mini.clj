(ns bsless.kanren.mini
  (:require
   [bsless.kanren.micro.impl.protocols
    :as p
    :refer [-inc pull]]
   [bsless.kanren.mini.impl.reify :as r]
   [bsless.kanren.micro
    :as u
    :refer [=== -conj -disj call-fresh make-empty-state]]))

;;; η - look! Eta!
(defmacro -delay [g] `(fn ~'goal-delay [s#] (-inc (~g s#))))

(defmacro conj*
  ([g] `(-delay ~g))
  ([g & gs] `(-conj (-delay ~g) (conj* ~@gs))))

(defmacro disj*
  ([g] `(-delay ~g))
  ([g & gs] `(-disj (-delay ~g) (disj* ~@gs))))

(defmacro conde
  [gs & gss]
  `(disj*
    ~@(map
       (fn conjoin [gs] `(conj* ~@gs))
       (cons gs gss))))

(defmacro fresh
  [[lvar & lvars] & goals]
  (if lvar
    `(call-fresh
      (fn [~lvar]
        (fresh [~@lvars] ~@goals)))
    `(conj* ~@goals)))

(defonce pull-xf (map pull))

(defn take-all [$]
  (->Eduction pull-xf (pull $)))

(defn take-n [n $]
  (->Eduction (comp pull-xf (take n)) (pull $)))

(defn call-empty-state
  [g]
  (g (make-empty-state)))

(defmacro run
  [n [x & xs :as bindings] & goals]
  (if (seq xs)
    `(run ~n [q#]
       (fresh [~@bindings]
         (=== q# (list ~@bindings))
         ~@goals))
    `(let [n# ~n
           ret# (call-empty-state
                 (fresh [~x]
                   ~@goals
                   (r/reifyg ~x)))]
       (if (nil? n#)
         (take-all ret#)
         (take-n n# ret#)))))

(comment
  (u/set-empty-state-fn! (constantly {}))
  (run 1 [x] (=== x 5))
  (run 2 [x] (conde [(=== x 5)] [(=== x 6)]))
  (run 2 [a b] (=== a 1) (=== b 2)))

