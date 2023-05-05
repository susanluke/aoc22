(ns aoc22.day19
  (:require [clojure.string :as string]))

(def ore-idx     0)
(def clay-idx    1)
(def obs-idx     2)
(def geo-idx     3)
(def nothing-idx 4)

(def initial-state [[0 0 0 0 0] [1 0 0 0 0]])

;; Arbitrary max states per simulated minute to explore
(def pt1-max-states 3500)
(def pt2-max-states 25000)

(defn parse-blueprint [s]
  (->> s
       (re-matches #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.")
       (drop 2)
       (map read-string)
       ((fn [[ore-robot-cost-ore
             clay-robot-cost-ore
             obs-robot-cost-ore   obs-robot-cost-clay
             geode-robot-cost-ore geode-robot-cost-obs]]
          ;; return 2D array of cost for each robot based on defined idx's above
          [[ore-robot-cost-ore   0                   0                    0 0] ;; ore robot
           [clay-robot-cost-ore  0                   0                    0 0] ;; clay robot
           [obs-robot-cost-ore   obs-robot-cost-clay 0                    0 0] ;; obsidian robot
           [geode-robot-cost-ore 0                   geode-robot-cost-obs 0 0] ;; geode robot
           [0                    0                   0                    0 0] ;; doing nothing costs nothing
           ]))))

(defn parse-blueprints [s]
  (->> s
       (string/split-lines)
       (map parse-blueprint)))

(defn robots-can-build [blueprint mined]
  (->> (for [robot-type blueprint]
         (->> robot-type
              (map >= mined)
              (every? true?)))
       (map-indexed vector)
       (filter (fn [[i b]] b))
       (map first)))

(defn process-minute-for-state [bp state]
  (let [[mined robots] state
        new-robots     (robots-can-build bp mined)
        new-mined      (mapv + mined robots)]
    (for [robot new-robots]
      [(vec (map - new-mined (get bp robot)))
       (vec (update robots robot inc))])))

;; TODO: tidy this up a bit
;; Rough heuristic - states with more geode & obsidian robots or mined resources
;; are better.
(defn state-comparator [state1 state2]
  (cond
    (not= (get-in state1 [0 geo-idx]) (get-in state2 [0 geo-idx]))
    (> (get-in state1 [0 geo-idx]) (get-in state2 [0 geo-idx]))

    (not= (get-in state1 [1 geo-idx]) (get-in state2 [1 geo-idx]))
    (> (get-in state1 [1 geo-idx]) (get-in state2 [1 geo-idx]))

    (not= (get-in state1 [0 obs-idx]) (get-in state2 [0 obs-idx]))
    (> (get-in state1 [0 obs-idx]) (get-in state2 [0 obs-idx]))

    (not= (get-in state1 [1 obs-idx]) (get-in state2 [1 obs-idx]))
    (> (get-in state1 [1 obs-idx]) (get-in state2 [1 obs-idx]))

    (not= (get-in state1 [0 nothing-idx]) (get-in state2 [0 nothing-idx]))
    (< (get-in state1 [0 nothing-idx]) (get-in state2 [0 nothing-idx]))

    ;; clay robots
    (not= (get-in state1 [1 clay-idx]) (get-in state2 [1 clay-idx]))
    (> (get-in state1 [1 clay-idx]) (get-in state2 [1 clay-idx]))

    ;; ore robots
    (not= (get-in state1 [1 ore-idx]) (get-in state2 [1 ore-idx]))
    (> (get-in state1 [1 ore-idx]) (get-in state2 [1 ore-idx]))

    ;; avoid states where we do a lot of nothing building
    (not= (get-in state1 [1 nothing-idx]) (get-in state2 [1 nothing-idx]))
    (< (get-in state1 [1 nothing-idx]) (get-in state2 [1 nothing-idx]))

    :default
    false))

(defn filter-poor-states [max-states states]
  (->> states
       (sort state-comparator)
       (take max-states)))

(defn process-minute-for-states [bp max-states states]
  (->> (map (partial process-minute-for-state bp) states)
       (apply concat)
       (filter-poor-states max-states)))

(defn find-max-geodes-for-blueprint [bp max-states t]
  (-> (->> (iterate (partial process-minute-for-states bp max-states) [initial-state])
           (drop t)
           ffirst)
      (get-in [0 geo-idx])))

(defn pt1 [s t]
  (->> (map find-max-geodes-for-blueprint (parse-blueprints s) (repeat pt1-max-states) (repeat t))
       (map-indexed (fn [i n] (* (inc i) n)))
       (apply +)))

(defn pt2 [s t]
  (->> (map find-max-geodes-for-blueprint
            (take 3 (parse-blueprints s))
            (repeat pt2-max-states)
            (repeat t))
       (apply *)))
