(ns aoc22.day06)

(defn first-marker [s win]
  (->> (partition win 1 s)
       (map-indexed list)
       (filter (fn [[i l]] (= win (count (set l)))))
       ffirst
       (+ win)))
