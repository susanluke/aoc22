(ns aoc22.day15
  (:require [clojure.string :as string]))

(defn parse-line [s]
  (->> s
       (re-matches #"^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$")
       rest
       (map read-string)
       (partition 2)))

(defn parse-string [s]
  (->> (string/split-lines s)
       (map parse-line)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn sensor-cover-for-row [row-y [sen-x sen-y] [bcn-x bcn-y]]
  (let [md           (manhattan-distance [sen-x sen-y] [bcn-x bcn-y])
        sen-row-dist (abs (- sen-y row-y))]
    (when (<= sen-row-dist md)
      [(- sen-x (- md sen-row-dist))
       (+ sen-x (- md sen-row-dist))])))

(defn touch? [[lo1 hi1] [lo2 hi2]]
  (or (<= (dec lo2) lo1 hi2)
      (<= lo2 hi1 (inc hi2))
      (<= lo1 lo2 hi2 hi1)))

(defn consolidate-touching-ranges [rs]
  (let [lows  (map first rs)
        highs (map second rs)]
    [(apply min lows) (apply max highs)]))

(defn consolidate-ranges [rs]
  (loop [res [] rs  rs]
    (if (empty? rs)
      res
      (let [touching (filter #(touch? (first rs) %) (concat res rs))]
        (recur (conj (remove (set touching) res)
                     (consolidate-touching-ranges touching))
               (rest rs))))))

(defn get-ranges [sensors-beacons row-y]
  (->> sensors-beacons
       (map (partial apply sensor-cover-for-row row-y))
       (remove nil?)
       consolidate-ranges))

(defn pt1 [s row-y]
  (->> (get-ranges (parse-string s) row-y)
       (map (fn [[lo hi]] (inc (- hi lo))))
       (apply +)
       ;; remove 1 for beacons (lazy, but there's one in both example and main challenge)
       (dec)))

(defn clip-range [lo hi x]
  (cond (< x lo) 0 (> x hi) hi :else x))

(defn tuning-frequency [[x y]]
  (+ (* 4000000 x) y))

(defn pt2 [s max-dim]
  (let [sensor-beacons (parse-string s)
        y-with-space   (->> (for [row-y (range max-dim)]
                              [row-y (->> sensor-beacons
                                          (map (partial apply sensor-cover-for-row row-y))
                                          (remove nil?)
                                          (map (fn [loc] (map (partial clip-range 0 max-dim) loc)))
                                          consolidate-ranges
                                          (map (fn [[lo hi]] (inc (- hi lo))))
                                          (apply +))])
                            (remove #(= (second %) (inc max-dim)))
                            ffirst)
        x-with-space (-> (get-ranges sensor-beacons y-with-space)
                         sort
                         first
                         second
                         inc)]
    (tuning-frequency [x-with-space y-with-space])))
