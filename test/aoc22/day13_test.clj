(ns aoc22.day13-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day13 :as sut]))

(def input1
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(def input2 (-> "input13.txt" io/resource slurp))

(deftest pt1
  (is (= 13 (sut/pt1 input1)))
  (is (= 5198 (sut/pt1 input2)))
  )

(deftest pt2
  (is (= 140 (sut/pt2 input1)))
  (is (= 22344 (sut/pt2 input2))))
