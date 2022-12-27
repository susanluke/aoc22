(ns aoc22.day14-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day14 :as sut]))

(def input1
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(def input2 (-> "input14.txt" io/resource slurp))

(deftest pt1
  (is (= 24 (sut/pt1 input1)))
  (is (= 873 (sut/pt1 input2))))
