(ns aoc22.day09
  (:require [clojure.string :as string]))

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
(def start [0 0])

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

(defn make-move [{:keys [h t visited]} dirn]
  (let [h2 (make-head-move h dirn)
        t2 (make-tail-move h2 t)]
    {:h h2
     :t t2
     :visited (conj visited t2)}))

(defn pt1 [s]
  (->> (parse-string s)
       (map expand-move)
       flatten
       (reduce make-move {:h [0 0] :t [0 0] :visited #{[0 0]}})
       :visited
       count))

(defn move-rope [{:keys [knots visited]} dirn]
  (let [new-knots (reduce (fn [t h] (->> h
                                        (make-tail-move (last t))
                                        (conj t)))
                          [(make-head-move (first knots) dirn)]
                          (rest knots))]
    {:knots   new-knots
     :visited (conj visited (last new-knots))}))

(defn pt2 [s]
  (->> (parse-string s)
       (map expand-move)
       flatten
       (reduce move-rope {:knots (vec (repeat 10 start)) :visited #{start}})
       :visited
       count))
