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

(def input2 (-> "input08.txt" io/resource slurp))

(deftest part1
  (is (= 21 (sut/pt1 input)))
  (is (= 1713 (sut/pt1 input2))))

(deftest part2
  (is (= 8 (sut/pt2 input))
      (= 268464 (sut/pt2 input2))))
