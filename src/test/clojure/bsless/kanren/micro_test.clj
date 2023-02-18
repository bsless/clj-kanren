(ns bsless.kanren.micro-test
  (:require
   [bsless.kanren.micro.impl.substitution]
   [bsless.kanren.micro :as uk]
   [clojure.test :as t]))

(def empty-state {})

(t/deftest goal-unify
  (t/is (= [5] (->> empty-state
                    ((uk/call-fresh (fn [x] (uk/=== x 5))))
                    vals))))

(defn fives [x] (uk/-disj (uk/=== x 5) (fn [s] (fn [] ((fives x) s)))))

(t/deftest recursive-disjunction
  (t/is (= [5 5 5]
           (->> empty-state
                ((uk/call-fresh fives))
                (into [] (take 3))
                (mapcat vals)))))

(defn sixes [x]
  (uk/-disj (uk/=== x 6) (fn g [s] (fn $ [] ((sixes x) s)))))

(def fives-and-sixes
  (uk/call-fresh (fn [x] (uk/-disj (fives x) (sixes x)))))

(t/deftest disjunction-interleaves
  (t/testing "Disjunction of two infinite streams samples them fairly"
    (t/is (= [5 6 5 6 5]
             (->> empty-state
                  fives-and-sixes
                  (into [] (take 5))
                  (mapcat vals))))))
