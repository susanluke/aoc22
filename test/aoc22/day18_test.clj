(ns aoc22.day18-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day18 :as sut]))

(def lava-droplets-example
  "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(def lava-droplets
  (-> "input18.txt" io/resource slurp))

(deftest pt1
  (is (= 1 1))
  (is (= 64 (sut/pt1 lava-droplets-example)))
  (is (= 4314 (sut/pt1 lava-droplets))))
