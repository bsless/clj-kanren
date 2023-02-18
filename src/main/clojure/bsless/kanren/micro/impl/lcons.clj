(ns bsless.kanren.micro.impl.lcons
  (:require
   [bsless.kanren.micro.impl.lvar :as lvar :refer [lvar?]]
   [bsless.kanren.micro.impl.protocols :as p]
   [bsless.kanren.micro.impl.util :refer [-str]])
  (:import
   (bsless.kanren.micro.impl.protocols ITreeTerm)
   (clojure.lang IPersistentCollection)
   (java.io Writer)))

(set! *warn-on-reflection* true)

(defprotocol LConsSeq
  "Clojure doesn't have improper lists but we need them.
  This is a good old pair."
  (lfirst [this])
  (lnext [this]))

(defprotocol LConsPrint
  (toShortString [this]))

(deftype LCons [a d ^{:unsynchronized-mutable true :tag int} cache meta]
  ITreeTerm

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [_ new-meta] (LCons. a d cache new-meta))

  LConsSeq
  (lfirst [_] a)
  (lnext [_] d)

  LConsPrint
  (toShortString [this]
    (if (.. this getClass (isInstance d))
      (-str a " " (toShortString d))
      (-str a " . " d )))

  Object
  (toString [this]
    (if (.. this getClass (isInstance d))
      (-str "(" a " " (toShortString d) ")")
      (-str "(" a " . " d ")")))

  (equals [this o]
    (if (identical? this o)
      true
      (and (.. this getClass (isInstance o))
           (loop [me this
                  you o]
             (cond
               (nil? me) (nil? you)
               (lvar? me) true
               (lvar? you) true
               (and (instance? LCons me) (instance? LCons you))
               (let [mef  (lfirst me)
                     youf (lfirst you)]
                 (when (or (= mef youf)
                           (lvar? mef)
                           (lvar? youf))
                   (recur (lnext me) (lnext you))))
               :else (= me you))))))

  (hashCode [_]
    (if (clojure.core/== cache -1)
      (do
        (set!
         cache
         (-> 31
             unchecked-int
             (unchecked-multiply-int (clojure.lang.Util/hash d))
             (unchecked-add-int
              (clojure.lang.Util/hash a))))
        cache)
      cache))

  p/IUnify
  (-unify-terms [u v s]
    (cond
      (sequential? v)
      (loop [u u
             v (seq v)
             s s]
        (if (nil? v)
          (if (lvar? u)
            (if-let [s (p/-unify-in s u '())]
              s
              (p/-unify-in s u nil))
            nil)
          (if (instance? LCons u)
            (if-let [s (p/-unify-in s (lfirst u) (first v))]
              (recur (lnext u) (next v) s)
              nil)
            (p/-unify-in s u v))))

      (instance? LCons v)
      (loop [u u
             v v
             s s]
        (if (lvar? u)
          (p/-unify-in s u v)
          (cond
            (lvar? v) (p/-unify-in s v u)
            (and (instance? LCons u) (instance? LCons v))
            (if-let [s (p/-unify-in s (lfirst u) (lfirst v))]
              (recur (lnext u) (lnext v) s)
              nil)
            :else (p/-unify-in s u v))))

      :else nil))

  #_#_
  IOccursCheckTerm
  (occurs-check-term [v x s]
    (loop [v v x x s s]
      (if (instance? LCons v)
        (or (occurs-check s x (lfirst v))
            (recur (lnext v) x s))
        (occurs-check s x v))))

  #_#_
  IBuildTerm
  (build-term [u s]
    (loop [u u s s]
      (if (instance? LCons u)
        (recur (lnext u) (build s (lfirst u)))
        (build s u)))))

(defmethod print-method LCons [x ^Writer writer]
  (.write writer (str x)))

(definline lcons
  "Constructs a sequence a with an improper tail d if d is a logic variable."
  [a d]
  (if (number? d)
    `(LCons. ~a ~d (unchecked-int -1) nil)
    `(let [a# ~a d# ~d]
       (if (instance? IPersistentCollection d#)
         (.cons (.seq ^IPersistentCollection d#) a#)
         (if (nil? d#)
           (new clojure.lang.PersistentList a#)
           (LCons. a# d# (unchecked-int -1) nil))))))

(definline lcons? [x] `(instance? LCons ~x))

(defmacro llist
  "Constructs a sequence from 2 or more arguments, with the last argument as the
   tail. The tail is improper if the last argument is a logic variable."
  ([f s] `(lcons ~f ~s))
  ([f s & rest] `(lcons ~f (llist ~s ~@rest))))
