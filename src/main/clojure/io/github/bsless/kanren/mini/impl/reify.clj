(ns io.github.bsless.kanren.mini.impl.reify
  (:require
   [io.github.bsless.kanren.micro.impl.protocols
    :as p]
   [io.github.bsless.kanren.mini.impl.protocols
    :as p*]
   [io.github.bsless.kanren.micro.impl.lcons :refer [lfirst lnext]]
   [io.github.bsless.kanren.micro.impl.util :refer [-str]]
   [io.github.bsless.kanren.micro.impl.choice :refer [choice]]
   [io.github.bsless.kanren.micro :as u])
  (:import
   (io.github.bsless.kanren.micro.impl.lcons LCons)
   (io.github.bsless.kanren.micro.impl.lvar LVar)))

(def ^:const unbound-names-count 128)

(def ^:const unbound-names
  (let [r (range unbound-names-count)]
    (zipmap r (map (comp symbol str) (repeat "_") r))))

(defn reify-lvar-name [s]
  (let [c (count s)]
    (if (< c unbound-names-count)
      (unbound-names c)
      (clojure.lang.Symbol/intern (-str "_" (count s))))))

(defn walk* [s v]
  (let [v (u/walk s v)]
    (p*/-walk-term
     v
     (fn [x]
       (let [x (u/walk s x)]
         (if (p/tree-term? x)
           (walk* s x)
           x))))))

(defn -reify* [s v]
  (let [v (u/walk s v)]
    (p*/-reify-term v s)))

(defn -reify
  ([s v]
   (let [v (walk* s v)]
     (walk* (-reify* (with-meta (u/make-empty-state) (meta s)) v) v)))
  ([s v r]
   (let [v (walk* s v)]
     (walk* (-reify* r v) v))))

(extend-protocol p*/IReifyTerm
  LVar
  (-reify-term [v s]
    (let [rf (-> s clojure.core/meta :reify-vars)]
      (if (fn? rf)
        (rf v s)
        (if rf
          (p/-ext s v (reify-lvar-name s))
          (p/-ext s v (:oname v))))))
  LCons
  (-reify-term [v s]
    (loop [v v s s]
      (if (instance? LCons v)
        (recur (lnext v) (-reify* s (lfirst v)))
        (-reify* s v))))

  nil (-reify-term [v s] s)

  Object (-reify-term [v s] s)

  Long (-reify-term [v s] s)

  clojure.lang.IPersistentCollection
  (-reify-term [v s]
    (loop [v v s s]
      (if (seq v)
        (recur (next v) (-reify* s (first v)))
        s))))

(defonce empty-f (fn []))

(defn reifyg
  "Reification as a goal."
  [x]
  (fn goal-reify [a]
    (let [v (walk* a x)
          r (-reify* (with-meta (u/make-empty-state) (meta a)) v)]
      (if (zero? (count r))
        (choice v empty-f)
        (choice (walk* r v) empty-f)))))
