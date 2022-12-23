(ns aoc22.day09-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day09 :as sut]))

(def input
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def input2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(def input3 (-> "input09.txt" io/resource slurp))

(deftest part1
  (is (= 13 (sut/pt1 input)))
  (is (= 6339 (sut/pt1 input3))))

(deftest pt2
  (is (= 1 (sut/pt2 input)))
  (is (= 36 (sut/pt2 input2)))
  (is (= 2541 (sut/pt2 input3))))
