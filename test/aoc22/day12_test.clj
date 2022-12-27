(ns aoc22.day12-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day12 :as sut]))

(def input1
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(def input2 (-> "input12.txt" io/resource slurp))

(deftest pt1
  (is (= 31 (sut/pt1 input1)))
  (is (= 394 (sut/pt1 input2))))
