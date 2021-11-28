(ns io.github.bsless.kanren.micro.impl.substitution
  "Use PersistentMap as substitution."
  (:require
   [io.github.bsless.kanren.micro.impl.protocols :as p]
   [io.github.bsless.kanren.micro.impl.lvar :refer [lvar?]])
  (:import
   (clojure.lang IPersistentMap)))

(set! *warn-on-reflection* true)

(let [-walk (fn -walk [^IPersistentMap s v]
              (if (p/-bindable? v)
                (if-let [vp (p/-lookup s v)] (recur s vp) v)
                v))]

  (extend IPersistentMap
    p/ISubstitution
    {:-walk -walk

     :-ext (fn -ext [^IPersistentMap m u v] (.assoc m u v))

     :-lookup (fn -lookup [^IPersistentMap s v]
                (when-let [e (.entryAt s v)]
                  (val e)))

     :-find (fn -find [^IPersistentMap s v] (.entryAt s v))

     :-unify-in
     (fn -unify-in
       [s u v]
       (if (identical? u v)
         s
         (let [u (-walk s u) v (-walk s v)]
           (if (and (lvar? u) (= u v))
             s
             (if (and (not (lvar? u)) (lvar? v))
               (p/-unify-terms v u s)
               (p/-unify-terms u v s))))))}))

(def empty-state {})

(defn make-empty-state [] empty-state)

(extend-protocol p/IBind ;; Use hash map as substitution
  clojure.lang.IPersistentMap
  (-bind [this g] (g this)))
