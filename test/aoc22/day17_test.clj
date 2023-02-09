(ns aoc22.day17-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [aoc22.day17 :as sut]))



(def jet-pattern ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
(def jet-pattern2 (->> (-> "input17.txt" io/resource slurp)
                       (filter #{\< \>})))

(deftest pt1
  (is (= 3068 (sut/pt1 jet-pattern 2022)))
  (is (= 3119 (sut/pt1 jet-pattern2 2022))))

(deftest pt2
  ;; After 1,000,000,000,000 (1 trillion) goes
  #_(is (= 1514285714288 (sut/pt1 jet-pattern 1000000000000)))
  (is (= 1514285714288 (+ 184 (* 3571428571 424))))
  )


(comment
  ;; jet-pattern has a cycle of 40 (divisible by 5, number of rocks) 12.  It takes
  ;; 3 cycles to settle in (maybe less), then every every 7*40=280 rocks height
  ;; grows 424

  ;; So
  ;; first 120 cycles, grows (+ 66 59 59) = 184
  ;; then
  ;; each subsequent cycle, grows (+  64 60 61 62 59 59 59) = 424
  ;; need to work out how much it grows
  (66
   59
   59
   64 60 61 62 59 59 59
   64 60 61 62 59 59 59
   64 60 61 62 59 59 59
   64 60 61 62 59 59 59
   64 60 61 62 59 59 59
   64 60 61 62 59 59 59
   64 60 61 62 59 59 59
   64 60 61 62 59)

  ;; jet pattern2:
  (->> (map (partial sut/pt1 jet-pattern2)
            (range 0 (* 10091 5 20) (* 5 10091)))
       (partition 2 1)
       (map (fn [[lo hi]] (- hi lo))))
  (77560
   77555
   77571
   77560
   77523
   77546
   77542
   77550
   77575
   77562
   77522
   77543
   77544
   77552
   77570
   77562
   77520
   77542
   77550)

  (time (->> (map (partial sut/pt1 jet-pattern2)
                  (range 0 (* 10091 5 26) (* 5 10091)))
             (partition 2 1)
             (map (fn [[lo hi]] (- hi lo)))))
  "Elapsed time: 0.125084 msecs"
  (77560
   77555
   77571
   77560
   77523
   77546
   77542
   77550
   77575
   77562
   77522
   77543
   77544
   77552
   77570
   77562
   77520
   77542
   77550
   77545
   77570
   77558
   77529
   77550
   77540)

  ;; for info, these are the shapes of the rocks
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
  )
