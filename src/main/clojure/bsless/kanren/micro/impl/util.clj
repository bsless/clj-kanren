(ns bsless.kanren.micro.impl.util)

(set! *warn-on-reflection* true)

(defmacro -append [sb x]
  (cond
    (symbol? x)
    `(let [x# ~x] (if (nil? x#) nil (.append ~sb x#)))
    (nil? x) nil
    (seqable? x) `(.append ~sb ~x)
    :else `(.append ~sb ~(.toString ^Object x))
    ))

(defmacro -str
  ([a]
   (cond
     (symbol? a) `(let [a# ~a] (if (nil? a#) "" (.toString ^Object a#)))
     (nil? a) ""
     :else (.toString ^Object a)))
  ([a & args]
   (let [sb (with-meta (gensym "sb__") {:tag 'StringBuilder})]
     `(let [~sb (StringBuilder.)]
        ~@(map (fn [x] `(-append ~sb ~x)) (cons a args))
        (.toString ~sb)))))
