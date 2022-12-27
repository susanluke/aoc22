(ns aoc22.day13
  (:require [clojure.string :as string]))

(def divider-packets #{[[2]] [[6]]})

(defn parse-string [s]
  (->> (string/split s #"\n\n")
       (map string/split-lines)
       (mapv #(map read-string %))))

(defn compare-lists [[l r]]
  (cond
    (and (not (coll? l)) (not (coll? r)))
    (cond
      (= l r) := (< l r) :< :else :>)

    (and (coll? l) (coll? r))
    (cond
      (and (empty? l) (empty? r)) :=
      (and (empty? l) (not (empty? r))) :<
      (and (not (empty? l)) (empty? r)) :>

      :else
      (let [head-res (compare-lists [(first l) (first r)])]
        (if (#{:< :>} head-res)
          head-res
          (compare-lists [(rest l) (rest r)]))))

    (and (coll? l) (not (coll? r)))
    (compare-lists [l (vector r)])

    (and (not (coll? l)) (coll? r))
    (compare-lists [(vector l) r])))

(defn pt1 [s]
  (->> (map compare-lists (parse-string s))
       (map-indexed (fn [i v] (if (= :< v) (inc i) 0)))
       (apply +)))

(defn list-comparator [l r]
  (let [res (compare-lists [l r])]
    (boolean (#{:< :=} res))))

(defn pt2 [s]
  (->> (sort list-comparator
             (-> (apply concat (parse-string s))
                 (concat divider-packets)))
       (map-indexed (fn [i v] (if (divider-packets v) (inc i) 1)))
       (apply *)))
