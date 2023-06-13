(ns aoc22.day22-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day22 :as sut]))

(def example
  "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

(def input (-> "input22.txt" io/resource slurp))

(deftest pt1
  (is (= 6032 (sut/pt1 example))))
