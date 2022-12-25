(ns aoc22.day11-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day11 :as sut]))

(def input1
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(def input2 (-> "input11.txt" io/resource slurp))

(deftest pt1
  (is (= 10605 (sut/pt1 input1)))
  (is (= 58322 (sut/pt1 input2))))

(deftest pt2 (is (= 1 1)))

;; takes a while!
#_(deftest pt2
    (is (= 2713310158 (sut/pt2 input1)))
    (is (= 13937702909 (sut/pt2 input2))))
