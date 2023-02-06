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
  (is (= 1 1))
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
            (range 0 (* 10091 40) 10091))
       (partition 2 1)
       (map (fn [[lo hi]] (- hi lo)))
       )
  '(x
    15537 15532 15501 15486 15504
    15516
    15521
    15534
    15497
    15487
    15503
    15520
    15514
    15538
    15496
    15487
    15504
    15520
    15516
    15533
    15495
    15492
    15504
    15521
    15511
    15537
    15496
    15486
    15510
    15517
    15512
    15539
    15498
    15483
    15510
    15518
    15512
    15536
    15501)

  )
