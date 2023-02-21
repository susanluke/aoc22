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
  ;; After 1,000,000,000,000 (1 trillion, 1e12) goes..
  ;; ha! this is never going to work (est. ~1yr to run!!)
  #_(is (= 1514285714288 (sut/pt1 jet-pattern 1000000000000)))
  #_(is (= 1536994219669 (sut/pt1 jet-pattern2 (int 1e12))))

  ;; spotted cycles and worked out below.  Now I've got my stars, can't be
  ;; bothered to turn into code.
  )


(comment
  ;; ***************************
  ;; jet pattern:
  ;; ***************************

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


  (->> (map (partial sut/pt1 jet-pattern)
            (range 0 (* 40 5 20) (* 40 5)))
       (partition 2 1)
       (map (fn [[lo hi]] (- hi lo))))

  (
   308 300 306
   303 303 301 306 301 300 306
   303 303 301 306 301 300 306
   303 303)

  ;; ***************************
  ;; jet pattern2:
  ;; ***************************
  ;; first time we see a cycle is at 17.5m rocks:
  ;; here's the start of that cycle

  "At a cycle end:  50455 1 77560 profile: (12 11 1 1 5 8 8)"
  ;; cycle-number: 1
  ;; rocks: 50455
  ;; max-height: 77560

  ;; and the end of that cycle

  "At a cycle end:  17507885 347 26909529 profile: (12 11 1 1 5 8 8)
  cycle seen rock-count: 17507885 max-height 26909529 norm profile: (12 11 1 1 5 8 8)"
  ;; cycle-number: 347
  ;; rocks: 17507885
  ;; max-height: 26909529

  ;; once enter cycle: every
  (- 17507885 50455) ;; 17457430  (17,457,430 rocks)
  ;; grow by
  (- 26909529 77560) ;; 26831969 (26,831,969 height)

  (int (/ (- (long 1e12) 50455)
          17457430))
  ;; 57282  Need this many cycles...

  (mod (- (long 1e12) 50455)
       17457430)
  ;; 3444285 ...and this many additional at the end

  ;; so lets add up start and end:
  (+ 50455 3444285)
  ;; 3494740  (3,494,740 rocks for start and end)

  (sut/pt1 jet-pattern2 3494740)
  ;; 5371411 - height from starting to settle into cycle and final bit


  ;; this much height from 57,282 repeated cycles, growing by 26,831,969 each time
  (* 57282 26831969)
  ;; 1536988848258

  ;; Makes a total height of
  (+ 1536988848258 5371411)
  ;; 1536994219669 (1,536,994,219,669 - 1.53 trillion height)


  ;; for info, these are the shapes of the rocks
  (def rock-shapes
    "
####

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
