(ns aoc22.day08
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-string [s]
  (->> (string/split-lines s)
       (mapv (fn [l] (mapv (comp read-string str) l)))))

(defn increasing-height-slice? [ns]
  (->> ns
       (reduce (fn [[bs n-max] n]
                 [(conj bs (< n-max n)) (max n n-max)]) [[] -1])
       first))

(defn increasing-height-coords [r ns]
  (->> ns
       increasing-height-slice?
       (map-indexed (fn [i b] (if b [r i] nil)))
       (remove nil?)))

(defn pt1 [s]
  (let [wood (parse-string s)
        rc (count wood)
        nc (count (first wood))]
    (-> (clojure.set/union
         ;; looking west
         (->> (map-indexed increasing-height-coords wood)
              (apply concat)
              set)
         ;; looking east
         (->> (map-indexed increasing-height-coords (map reverse wood))
              (apply concat)
              (map (fn [[r c]] [r (- (dec nc) c)]))
              set)
         ;; looking south
         (->> (map-indexed increasing-height-coords
                           (apply mapv vector wood))
              (apply concat)
              (map (fn [[r c]] [c r]))
              set)
         ;; looking north
         (->> (map-indexed increasing-height-coords
                           (->> (apply mapv vector wood)
                                (map reverse)))
              (apply concat)
              (map (fn [[r c]] [(- (dec nc) c) r]))
              set)
         )
        count)))
