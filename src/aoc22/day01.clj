(ns aoc22.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def f "input01.txt")

(defn read-file [f]
  (-> f io/resource slurp string/split-lines))

(defn parse-and-sum [ss]
  (reduce #(if (seq %2)
             (conj (rest %1) (+ (first %1) (read-string %2)))
             (conj %1 0))
          '(0)
          ss))

(defn part1 []
  (->> f
       read-file
       parse-and-sum
       (apply max)))

(defn part2 []
  (->> f
       read-file
       parse-and-sum
       (sort >)
       (take 3)
       (apply +)))
