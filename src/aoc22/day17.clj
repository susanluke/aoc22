(ns aoc22.day17
  (:require [clojure.string :as string]))

(def chamber-width 7)
(def rock-shapes
  "####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
")

(defn string->keywords [rl]
  (mapv (comp keyword str) rl))

(defn read-rock [r]
  (->> (string/split-lines r) ;; each rock contains lines
       (mapv string->keywords)))

(defn rc->xy [r-max [r c]] [c (- r-max r)])
(defn rock->xy [rock]
  (let [r-max (dec (count rock))]
    (->> (for [r (range (count rock))
               c(range (count (first rock)))]
           (when (= (get-in rock [r c]) :#)
             [r c]))
         (remove nil?)
         (map (partial rc->xy r-max)))))

(defn read-rocks [rock-shapes]
  (->> (string/split rock-shapes #"\n\n") ;; break into individual rocks
       (mapv read-rock)
       (map rock->xy)))

(defn move-rock-x-axis [d rock] (map (fn [[x y]] [(+ x d) y]) rock))
(defn move-rock-y-axis [d rock] (map (fn [[x y]] [x (+ y d)]) rock))
(defn move-rock-down   [rock]   (move-rock-y-axis -1 rock))
(defn move-rock-left   [rock]   (move-rock-x-axis -1 rock))
(defn move-rock-right  [rock]   (move-rock-x-axis +1 rock))

(defn within-bounds [settled-rocks [x y]]
  (and (<= 0 x (dec chamber-width))
       (<= 0 y)
       (not (settled-rocks [x y]))))

(defn move [direction rock]
  ((case direction
     :<    move-rock-left
     :>    move-rock-right
     :down move-rock-down)
   rock))

(defn can-move? [settled-rocks direction rock]
  (->> (move direction rock)
       (every? (partial within-bounds settled-rocks))))

(defn max-rock-height [rock]
  (->> (map second rock)
       (apply max)
       inc));; height is +1 max y

(defn add-rock [{:keys [max-height settled-rocks jets] :as state} rock]
  (let [rock-start (->> rock
                        (move-rock-x-axis 2)
                        (move-rock-y-axis (+ max-height 3)))]
    (loop [rock          rock-start
           jets          jets
           settled-rocks settled-rocks
           max-height    max-height]
      (let [jet-move       (first jets)
            jets-rest      (rest jets)
            rock-after-jet (if (can-move? settled-rocks jet-move rock)
                             (move jet-move rock)
                             rock)]
        (if (can-move? settled-rocks :down rock-after-jet)
          (recur (move :down rock-after-jet) jets-rest settled-rocks max-height)
          {:max-height    (max max-height (max-rock-height rock-after-jet))
           :settled-rocks (apply conj settled-rocks rock-after-jet)
           :jets          jets-rest})))))

(defn pt1 [jet-pattern num-rocks]
  (let [rocks (cycle (read-rocks rock-shapes))
        jets  (cycle (string->keywords jet-pattern))]
    (->> (take num-rocks rocks)
         (reduce add-rock
                 {:max-height    0
                  :settled-rocks #{}
                  :jets          jets})
         :max-height)))

(defn print-rocks [settled-rocks]
  (concat
   (concat [\|] (repeat chamber-width \.) [\|])
   (vec "+-------+")
   ))


(comment
  " Do we care about r,c?  Read as r/c first (easy), convert to x,y and work
with that for remainder of the challenge

- - -> c
|
|
v
r

y
^
|
|
 - - > x

")
