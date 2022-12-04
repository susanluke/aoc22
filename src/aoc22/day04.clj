(ns aoc22.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def f "input04.txt")

(defn read-file [f] (-> f io/resource slurp))

(defn parse-file [s] (-> s string/split-lines))

(defn process-line [s]
  (let [[l1 h1 l2 h2] (->> (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" s)
                           rest
                           (map read-string))]
    [[l1 h1] [l2 h2]]))

(defn inside? [[[l1 h1] [l2 h2]]]
  (or (and (<= l1 l2)
           (<= h2 h1))
      (and (<= l2 l1)
           (<= h1 h2))))

(defn overlap? [[[l1 h1] [l2 h2]]]
  (or (<= l1 l2 h1)
      (<= l1 h2 h1)
      (inside? [[l1 h1] [l2 h2]])))

(defn partx [filter-fn]
  (->> (read-file f)
       parse-file
       (map process-line)
       (filter filter-fn)
       count))

(defn part1 [] (partx inside?))

(defn part2 [] (partx overlap?))
