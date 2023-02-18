(ns bsless.kanren.micro.impl.lvar
  (:require
   [bsless.kanren.micro.impl.protocols :as p]
   [bsless.kanren.micro.impl.util :refer [-str]])
  (:import (java.io Writer)))

(definterface IVar)

(deftype LVar [^long id ^boolean unique ^String name ^String oname ^int hash meta]
  IVar
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (case k
      :name name
      :oname oname
      :id id
      not-found))

  clojure.lang.IObj
  (meta [_]
    meta)
  (withMeta [_ new-meta]
    (LVar. id unique name oname hash new-meta))

  Object
  (toString [_] (str "<lvar:" name ">"))

  (equals [_ o]
    (if (instance? LVar o)
      (if unique
        (if (= id (.-id ^LVar o)) true false)
        (if (.equals name (.-name ^LVar o)) true false))
      false))

  (hashCode [_] hash)

  p/IUnify
  (-unify-terms [u v s] (p/-ext s u v))

  p/IBindable
  (-bindable? [_] true)

  #_#_
  IOccursCheckTerm
  (occurs-check-term [v x s] (= (walk s v) x))

  #_#_
  IBuildTerm
  (build-term [u s]
    (let [m (:s s)
          cs (:cs s)
          lv (lvar 'ignore) ]
      (if (contains? m u)
        s
        (make-s (assoc m u lv) cs)))))


(defn lvar
  {:inline
   (fn
     ([] `(let [id# (. clojure.lang.RT (nextID))
                name# (str id#)]
            (LVar. id# true name# nil (.hashCode name#) nil)))
     ([name] `(lvar ~name true))
     ([name unique]
      `(let [name# ~name
             oname# name#
             unique# ~unique
             id#   (if unique#
                    (. clojure.lang.RT (nextID))
                    name#)
             name# (if unique#
                    (-str name# "__" id#)
                    (-str name#))]
         (LVar. id# unique# name# oname# (.hashCode name#) nil))))
   :inline-arities #{1 2 3}}
  ([]
   (let [id (. clojure.lang.RT (nextID))
         name (str id)]
     (LVar. id true name nil (.hashCode name) nil)))
  ([name]
   (lvar name true))
  ([name unique]
   (let [oname name
         id   (if unique
                (. clojure.lang.RT (nextID))
                name)
         name (if unique
                (-str name "__" id)
                (-str name))]
     (LVar. id unique name oname (.hashCode name) nil))))

(defmethod print-method LVar [x ^Writer writer]
  (.write writer (str "<lvar:" (:name x) ">")))

(definline lvar? [x]
  `(instance? LVar ~x))

(defn lvars [n]
  (repeatedly n lvar))
