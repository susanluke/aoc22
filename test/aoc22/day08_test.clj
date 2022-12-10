(ns aoc22.day08-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day08 :as sut]))

(def input
  "30373
25512
65332
33549
35390")

(def f "input08.txt")
(defn read-file [f] (-> f io/resource slurp))
(def input2 (read-file f))

(deftest part1
  (is (= 21 (sut/pt1 input)))
  (is (= 1713 (sut/pt1 input2))))
