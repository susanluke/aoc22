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

;; Part 2

(defn up [[_ _] [r c]]
  (map #(vector % c) (range (dec r) (dec 0) -1)))

(defn down [[nr __] [r c]]
  (map #(vector % c) (range (inc r) nr)))

(defn left [[_ _] [r c]]
  (map #(vector r %) (range (dec c) (dec 0) -1)))

(defn right [[_ nc] [r c]]
  (map #(vector r %) (range (inc c) nc)))

(defn dims [m]
  ((juxt count (comp count first)) m))

(defn can-see? [d m [r c]]
  (let [coords (d (dims m) [r c])
        h (get-in m [r c])]
    (->> coords
         (map #(get-in m %))
         (take-while #(< % h))
         count
         inc ;; add an extra as take-while doesn't include blocking tree
         (min (count coords)) ;; but maybe there isn't an extra, we're just at edge of wood
         )))

(defn scenic-score [m [r c]]
  (->> (map #(can-see? % m [r c]) [up down left right])
       (apply *)))

(defn pt2 [s]
  (let [wood (parse-string s)
        nr(count wood)
        nc (count (first wood))]
    (->> (for [r (range 0 nr)
               c (range 0 nc)]
           (scenic-score wood [r c]))
         (apply max))))
