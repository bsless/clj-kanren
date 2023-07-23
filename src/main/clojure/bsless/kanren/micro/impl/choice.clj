(ns bsless.kanren.micro.impl.choice
  (:require
   [bsless.kanren.micro.impl.protocols
    :as p
    :refer [-bind -mplus -inc pull]]))

(deftype Choice [a f]
  Object
  (toString [_] (str "<choice: " a ", ...>"))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (if (.equals :a k)
      a
      not-found))
  p/IBind
  (-bind [_ g]
    (-mplus (g a) (-inc (-bind f g))))
  p/IMPlus
  (-mplus [_ fp]
    (Choice. a (-inc (-mplus (fp) f))))
  clojure.lang.Seqable
  (seq [_]
    (lazy-seq (cons a (lazy-seq (pull f)))))
  clojure.lang.IReduceInit
  (reduce [_ g init]
    (let [r (g init a)]
      (if (reduced? r)
        @r
        (reduce g r (pull f)))))
  clojure.lang.IReduce
  (reduce [_ g]
    (reduce g a (pull f))))

(definline ^Choice choice [a f]
  `(Choice. ~a ~f))

;;; Unit

(extend-type Object
  p/IMPlus
  (-mplus [this f]
    (Choice. this f)))
