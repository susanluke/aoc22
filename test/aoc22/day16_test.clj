(ns aoc22.day16-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day16 :as sut]))

(def input
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(def input2 (-> "input16.txt" io/resource slurp))

(comment
  ;; input2 takes a few hours
  {:pressure-released 1653,
   :actions
   [[5 [:valve-on :HR]]
    [6 [:travel :HR]]
    [7 [:travel :KO]]
    [8 [:travel :GQ]]
    [9 [:valve-on :YA]]
    [10 [:travel :YA]]
    [11 [:travel :UJ]]
    [12 [:valve-on :NS]]
    [13 [:travel :NS]]
    [14 [:travel :CV]]
    [15 [:valve-on :KV]]
    [16 [:travel :KV]]
    [17 [:travel :TA]]
    [18 [:travel :VH]]
    [19 [:valve-on :DT]]
    [20 [:travel :DT]]
    [21 [:travel :HU]]
    [22 [:travel :TQ]]
    [23 [:valve-on :SU]]
    [24 [:travel :SU]]
    [25 [:travel :OL]]
    [26 [:valve-on :GG]]
    [27 [:travel :GG]]
    [28 [:travel :ML]]
    [29 [:travel :RI]]
    [30 [:travel :RJ]]]})

(deftest pt1
  (is (= 1651 (:pressure-released (sut/pt1 input))))
  ;; Takes a few hours
  ;;(is (= (1653 (sut/pt1 input2))))
  )
