(ns aoc22.day10
  (:require [clojure.string :as string]))

(def start-val 1)

(defn parse-line [l]
  (let [[_ op n] (re-matches #"^([a-z]+) ?(-?\d*)$" l)]
    (if (= "noop" op)
      [:noop nil]
      [:addx (read-string n)])))

(defn parse-string [s]
  (->> (string/split-lines s) (map parse-line)))

(defn pt1 [s]
  (let [register-hist (->> (parse-string s)
                           (reduce (fn [output [op n]]
                                     (let [current-val (last output)]
                                       (if (= :noop op)
                                         (conj output current-val)
                                         (-> output
                                             (conj current-val)
                                             (conj (+ current-val n))))))
                                   [start-val]))]
    (->> (map (fn [n]
                (* n (nth register-hist (dec n))))
              [20 60 100 140 180 220])
         (apply +))))
