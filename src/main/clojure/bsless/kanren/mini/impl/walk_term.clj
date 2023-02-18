(ns bsless.kanren.mini.impl.walk-term
  (:require
   [bsless.kanren.micro.impl.lcons :refer [lcons lfirst lnext]]
   [bsless.kanren.mini.impl.protocols
    :as p*])
  (:import
   (bsless.kanren.micro.impl.lcons LCons)))

(extend-protocol p*/IWalkTerm

  nil (-walk-term [v f] (f nil))

  Object (-walk-term [v f] (f v))

  clojure.lang.ISeq
  (-walk-term [v f]
    (with-meta
      (doall (map #(p*/-walk-term (f %) f) v))
      (meta v)))

  clojure.lang.IPersistentVector
  (-walk-term [v f]
    (->
     (fn [r v] (conj! r (p*/-walk-term (f (first v)) f)))
     (reduce (transient []) v)
     persistent!
     (with-meta (meta v))))

  clojure.lang.IPersistentMap
  (-walk-term [v f]
    (->
     (fn [r vfk vfv]
       (assoc!
        r
        (p*/-walk-term (f vfk) f)
        (p*/-walk-term (f vfv) f)))
     (reduce-kv (transient {}) v)
     persistent!
     (with-meta (meta v))))

  LCons
  (-walk-term [v f]
    (lcons (f (lfirst v))
           (f (lnext v))))

  #_#_clojure.lang.IRecord
  (-walk-term [v f]
    (walk-record-term v f)))
