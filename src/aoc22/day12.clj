(ns aoc22.day12
  (:require [clojure.string :as string]))

(def start-val 0)
(def end-val 27)
(def moves [[-1  0] ; up
            [ 1  0] ; down
            [ 0 -1] ; left
            [ 0  1] ; right
            ])

(defn queue [coll]
  (reduce conj clojure.lang.PersistentQueue/EMPTY coll))

(defn parse-char [c]
  (case c
    \S start-val
    \E end-val
    (inc (- (int c)
            (int \a)))))

(defn parse-map [s]
  (->> (string/split-lines s)
       (mapv #(mapv parse-char %))))

(defn find-start [m]
  (->> (for [r (range (count m))]
         (for [c (range (count (first m)))]
           (let [v (get-in m [r c])]
             (when (= v start-val) [r c]))))
       (apply concat)
       (remove nil?)
       first))

(defn reachable-squares [m visited [r c]]
  (let [v               (get-in m [r c])
        nr              (count m)
        nc              (count (first m))]
    (->> (map #(mapv + [r c] %) moves)
         (remove (fn [[r c]]
                   (or (neg? r)
                       (neg? c)
                       (>= r nr)
                       (>= c nc))))
         (filter (fn [[r c]]
                   (<= (get-in m [r c]) (inc v))))
         (remove visited))))

(defn explore [m end-val {:keys [routes visited]}]
  (let [r         (peek routes)
        loc       (peek r)
        reachable (reachable-squares m visited loc)]
    (if (= end-val (get-in m loc))
      r
      (explore m end-val {:routes  (apply conj (pop routes)
                                          (map #(conj r %) reachable))
                          :visited (apply conj visited reachable)}))))

(defn pt1 [s]
  (let [m     (parse-map s)
        start (find-start m)]
    (-> (explore m end-val {:routes  (queue [[start]])
                            :visited #{start}})
        count
        dec)))
