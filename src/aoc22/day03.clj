(ns aoc22.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(def f "input03.txt")

(defn read-file [f]
  (-> f (io/resource) slurp string/split-lines))

(defn split-contents [s] (split-at (/ (count s) 2) s))

(defn find-chars [s1 s2] (set/intersection (set s1) (set s2)))

(defn char-val [c]
  (let [n (int c)]
    (inc (if (<= 97 n 122)
           (- n (int \a))
           (- n (int \A) -26)))))

(defn part1 []
  (->> f
       read-file
       (map split-contents)
       (map #(apply find-chars %1))
       (map (comp char-val first))
       (apply +)))

(defn part2 []
  (->> f
       read-file
       (partition 3)
       (map #(reduce find-chars %))
       (map (comp char-val first))
       (apply +)))
