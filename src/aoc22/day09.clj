(ns aoc22.day09
  (:require [clojure.string :as string]))

(def start [0 0])
(def move-map
  {[-1  2] [-1  1] [0  2] [0  1] [1  2] [ 1  1]
   [-2  1] [-1  1] [2  1] [1 1]
   [-2  0] [-1  0] [2  0] [1 0]
   [-2 -1] [-1 -1] [2 -1] [1 -1]
   [-1 -2] [-1 -1] [0 -2] [0 -1] [1 -2] [1 -1]
   [-2 -2] [-1 -1]
   [-2  2] [-1  1]
   [ 2 -2] [ 1 -1]
   [ 2  2] [ 1  1]})

(defn parse-line [l]
  (let [[_ d n] (re-matches #"([URLD]) (\d+)" l)]
    [(keyword d) (read-string n)]))

(defn parse-string [s]
  (->> (string/split-lines s)
       (map parse-line)))

(defn make-head-move [[x y] dirn]
  (case dirn
    :U [x (inc y)]
    :D [x (dec y)]
    :L [(dec x) y]
    :R [(inc x) y]))

(defn make-tail-move [[xh yh] [xt yt]]
  (map +
       (get move-map [(- xh xt) (- yh yt)] [0 0])
       [xt yt]))

(defn expand-move [[dirn n]]
  (repeat n dirn))

(defn move-rope [{:keys [knots visited]} dirn]
  (let [new-knots (reduce (fn [t h] (->> h
                                        (make-tail-move (last t))
                                        (conj t)))
                          [(make-head-move (first knots) dirn)]
                          (rest knots))]
    {:knots   new-knots
     :visited (conj visited (last new-knots))}))

(defn ptx [s num-knots]
  (->> (parse-string s)
       (map expand-move)
       flatten
       (reduce move-rope {:knots   (vec (repeat num-knots start))
                          :visited #{start}})
       :visited
       count))

(defn pt1 [s] (ptx s 2))
(defn pt2 [s] (ptx s 10))
