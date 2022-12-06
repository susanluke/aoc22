(ns aoc22.day05
  (:require [clojure.string :as string]))

(defn parse-state-line [state l]
  (->> (range 1 (count l) 4)
       (map #(nth l %1))
       (map-indexed list)
       (reduce (fn [acc [i v]]
                 (if (not (= \space v))
                   (update acc i #(conj % v))
                   acc))
               state)))

(defn parse-init-state [ss]
  (let [empty-state (-> (count ss)
                        (repeat '())
                        vec)]
    (->> ss
         (reduce parse-state-line empty-state)
         (mapv reverse))))

(defn parse-ins-line [s]
  (->> (re-find #"^move (\d+) from (\d+) to (\d+)$" s)
       rest
       (map read-string)))

(defn parse-ins [ss]
  (map parse-ins-line ss))

(defn make-move [[f t] state]
  (let [mover (-> state (nth (dec f)) first)]
    (-> state
        (update (dec f) rest)
        (update (dec t) #(conj % mover)))))

(defn make-moves [state [n f t]]
  (-> (iterate (partial make-move [f t]) state)
      (nth n)))

(defn make-moves-9001 [state [n f t]]
  (let [movers (->> (nth state (dec f)) (take n))]
    (-> state
        (update (dec f) (partial drop n))
        (update (dec t) (partial concat movers)))))

(defn part-x [s move-fn]
  (fn [s]
    (let [ss                  (string/split-lines s)
          [state-raw ins-raw] (split-with #(not (re-find #"^ 1" %)) ss)
          init-state          (parse-init-state state-raw)
          ins                 (parse-ins (drop 2 ins-raw))
          final-state         (reduce move-fn init-state ins)]
      (->> final-state
           (map first)
           (apply str)))))


(def part-1 (part-x s make-moves))
(def part-2 (part-x s make-moves-9001))
