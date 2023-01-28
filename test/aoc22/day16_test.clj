(ns aoc22.day16-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string] ;; TODO - just while messing
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

(deftest pt1
  (is (= 1651 (:pressure-released (sut/pt1 input))))
  ;; takes 3-4 seconds
  (is (= 1653 (:pressure-released (sut/pt1 input2)))))

(deftest pt2
  (is (= 1707 (:pressure-released (sut/pt2 input))))
  ;; takes 5.5h!
  ;;(is (= 2223 (:pressure-released (sut/pt2 input2))))
  )

(comment
  ;; Pt2: input2 takes 5.5hrs
  ;; Need to prune route choices and dismiss routes which obviously don't make sense

  "Elapsed time: 2.0527428064667E7 msecs"
  {:pressure-released 2223,
   :actions
   [[26 [:valve-on :AA :route-onwards (:KV :PE :RI :RJ :AA)]]
    [26 [:valve-on :AA :route-onwards (:RI :RJ :AA)]]
    [24 [:valve-on :RI :route-onwards (:GG :ML :RI)]]
    [22 [:valve-on :KV :route-onwards (:NS :CV :KV)]]
    [21 [:valve-on :GG :route-onwards (:SU :OL :GG)]]
    [19 [:valve-on :NS :route-onwards (:YA :UJ :NS)]]
    [18 [:valve-on :SU :route-onwards (:JQ :YN :SF :SU)]]
    [16 [:valve-on :YA :route-onwards (:HR :KO :GQ :YA)]]
    [14 [:valve-on :JQ :route-onwards (:QF :NJ :JC :JQ)]]
    [12 [:valve-on :HR :route-onwards (:DT :VH :TA :KV :HN :LR :HR)]]
    [10 [:valve-on :QF :route-onwards (:UH :YI :QF)]]
    [7 [:valve-on :UH]]
    [5 [:valve-on :DT]]]}
  )
