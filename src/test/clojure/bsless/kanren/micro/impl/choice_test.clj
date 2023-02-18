(ns bsless.kanren.micro.impl.choice-test
  (:require
   [bsless.kanren.micro.impl.choice]
   [bsless.kanren.micro.impl.protocols :as p]
   [clojure.test :as t]))

(def one-or-two (p/-mplus 1 (p/-inc [2])))

(t/deftest choice-reduce
  (t/is (= [1 2] (reduce conj [] one-or-two))))

(t/deftest choice-seq
  (t/is (= [1 2] (seq one-or-two))))
