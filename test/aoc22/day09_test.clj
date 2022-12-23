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

(def input2 (-> "input09.txt" io/resource slurp))

(deftest part1
  (is (= 13 (sut/pt1 input))))
