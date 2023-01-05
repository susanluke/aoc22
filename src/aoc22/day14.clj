(ns aoc22.day14
  (:require [clojure.string :as string]))

(def sand-origin [500,0])

(defn process-line [s]
  (map #(->> (string/split % #",")
             (map read-string)) s))

(defn parse-string [s]
  (->> (string/split-lines s)
       (map #(string/split % #" -> "))
       (map process-line)))

(defn grid-dims [cave-scan]
  (let [flat-scan (apply concat cave-scan)
        x-coords (map first flat-scan)
        y-coords (map second flat-scan)]
    [[(dec (apply min x-coords)) ;; one less so can model sand flowing out
      (inc (max (apply max x-coords) ;; one more " "
                (first sand-origin)))]
     [(min (apply min y-coords)
           (second sand-origin))
      (apply max y-coords)]]))

(defn initialise-grid [[[x-min x-max] [y-min y-max]]]
  (let [row (vec (repeat (inc (- x-max x-min)) :.))]
    (vec (repeat (inc (- y-max y-min)) row))))

(defn get-in-grid [x-min grid [x y]]
  (get-in grid [y (- x x-min)]))

(defn assoc-in-grid [x-min v grid [x y]]
  (assoc-in grid [y (- x x-min)] v))

(defn add-rock-path-section [x-min grid [[x1 y1] [x2 y2]]]
  (let [[x-lo x-hi] (sort [x1 x2])
        [y-lo y-hi] (sort [y1 y2])
        coords (for [x (range x-lo (inc x-hi))
                     y (range y-lo (inc y-hi))]
                 [x y])]
    (reduce (partial assoc-in-grid x-min :#) grid coords)))

(defn add-rock-path [x-min grid rock-path]
  (reduce (partial add-rock-path-section x-min)
          grid
          (partition 2 1 rock-path)))

(defn move-sand-grain [x-min grid [x y]]
  (cond
    ;; sand blocked at sand-origin
    (not= :. (get-in-grid x-min grid [x y]))
    :void

    ;; sand falls into endless void
    (= y (dec (count grid)))
    :void

    ;; grain can move down
    (= :. (get-in-grid x-min grid [x (inc y)]))
    (move-sand-grain x-min grid [x (inc y)])

    ;; grain can move left-down
    (= :. (get-in-grid x-min grid [(dec x) (inc y)]))
    (move-sand-grain x-min grid [(dec x) (inc y)])

    ;; grain can move right-down
    (= :. (get-in-grid x-min grid [(inc x) (inc y)]))
    (move-sand-grain x-min grid [(inc x) (inc y)])

    ;; grain can't move
    :else
    [x y]))

(defn add-sand-grain [x-min grid]
  (let [co-ords (move-sand-grain x-min grid sand-origin)]
    (if (= :void co-ords)
      :finished
      (assoc-in-grid x-min :o grid co-ords))))

(defn add-sand [x-min grid]
  (->> (take-while #(not (= :finished %)) (iterate (partial add-sand-grain x-min) grid))
       (map-indexed vector)
       last
       first))

(defn pt1 [s]
  (let [cave-scan                     (parse-string s)
        [[x-min x-max] [y-min y-max]] (grid-dims cave-scan)
        empty-grid                    (initialise-grid [[x-min x-max]
                                                        [y-min y-max]])
        cave-grid                     (reduce (partial add-rock-path x-min) empty-grid cave-scan)]
    (add-sand x-min cave-grid)))


(defn pt2 [s]
  (let [cave-scan                     (parse-string s)
        [[x-min x-max] [y-min y-max]] (grid-dims cave-scan)
        ;; adjust grid size to cater for sand pile
        y-max                         (+ y-max 2) ;; add two for floor
        y-height                      (- y-max y-min)
        x-min                         (- x-min y-height)
        x-max                         (+ x-max y-height)
        empty-grid                    (initialise-grid [[x-min x-max]
                                                        [y-min y-max]])
        empty-grid-with-floor         (assoc empty-grid y-max (vec (repeat (inc (- x-max x-min)) :#)))
        cave-grid                     (reduce (partial add-rock-path x-min) empty-grid-with-floor cave-scan)]
    (add-sand x-min cave-grid)))
