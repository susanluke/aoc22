(ns aoc22.day20-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day20 :as sut])  )

(def encrypted-file-example
  "1
2
-3
3
-2
0
4")

(def encrypted-file (-> "input20.txt" io/resource slurp))

(deftest pt1
  (is (= 3 (sut/pt1 encrypted-file-example)))
  (is (= 13967 (sut/pt1 encrypted-file))))

(deftest pt2
  (is (= 1623178306 (sut/pt2 encrypted-file-example)))
  (is (= 1790365671518 (sut/pt2 encrypted-file))) )
