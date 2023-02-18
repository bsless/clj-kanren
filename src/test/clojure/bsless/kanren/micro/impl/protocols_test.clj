(ns bsless.kanren.micro.impl.protocols-test
  (:require
   [bsless.kanren.micro.impl.protocols :as p]
   [clojure.test :as t]))

(t/deftest pull-test
  (t/testing "Unnests all the thunks"
    (t/is (= 2 (-> 2 p/-inc p/-inc p/-inc p/pull)))))
