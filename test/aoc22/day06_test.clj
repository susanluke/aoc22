(ns aoc22.day06-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day06 :as sut]))

(def f "input06.txt")
(def input (-> f io/resource slurp))

(deftest first-marker
  (are [s res] (= res (sut/first-marker s 4))
    "bvwbjplbgvbhsrlpgdmjqwftvncz" 5
    "nppdvjthqldpwncqszvftbrmjlhg" 6
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11
    input 1651)
  (are [s res] (= res (sut/first-marker s 14))
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 19
    "bvwbjplbgvbhsrlpgdmjqwftvncz" 23
    "nppdvjthqldpwncqszvftbrmjlhg" 23
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 29
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 26
    input 3837))
