(ns aoc22.day19-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day19 :as sut]))

(def blueprints-example
  "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(def blueprints
  (-> "input19.txt" io/resource slurp))

(deftest pt1-test
  (is (= 33 (sut/pt1 blueprints-example 24)))
  (is (= 851 (sut/pt1 blueprints 24))))

(deftest p2-test
  (is (= 3472 (sut/pt2 blueprints-example 32)))
  (is (= 12160 (sut/pt2 blueprints 32))))

(comment
  "Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.")
