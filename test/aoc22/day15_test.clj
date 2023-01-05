(ns aoc22.day15-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day15 :as sut]))

(def input
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(def input2 (-> "input15.txt" io/resource slurp))

(deftest pt1
  (is (= 26 (sut/pt1 input 10)))
  (is (= 5688618 (sut/pt1 input2 2000000))))

(deftest pt2
  (is (= 56000011 (sut/pt2 input 20)))
  ;; takes ~15 mins
  ;; x,y should be: 3156345,3204261
  ;;(is (= 12625383204261 (sut/pt2 input 4000000)))
  )
