(ns aoc22.day22-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day22 :as sut]))

(def example
  "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

(def input (-> "input22.txt" io/resource slurp))

(deftest pt1
  (is (= 6032 (sut/pt1 example)))
  (is (= 30552 (sut/pt1 input))))

(defn new-state-when-off-limits-example [board [dirn [r c :as loc]] move]
  (let [move-remaining (dec move)
        new-posn
        (cond
          (and (= dirn :N) (= r -1)    (<= 8 c 11))  [:S [4        (- 11 c)]] ;; 1
          (and (= dirn :E) (<= 0 r 3)  (= c 12))     [:W [(- 11 r) 15]]       ;; 2 - double check
          (and (= dirn :E) (<= 4 r 7)  (= c 12))     [:S [8        (- 19 r)]] ;; 3
          (and (= dirn :N) (= r 7)     (<= 12 c 15)) [:W [(- 19 c) 11]]       ;; 4 - double check
          (and (= dirn :E) (<= 8 r 11) (= c 16))     [:W [(- 11 r) 11]]       ;; 5
          (and (= dirn :S) (= r 12)    (<= 12 c 15)) [:E [(- 19 c) 0]]        ;; 6
          (and (= dirn :S) (= r 12)    (<= 8 c 11))  [:N [7        (- 11 c)]] ;; 7
          (and (= dirn :W) (<= 8 r 11) (= c 7))      [:N [7        (- 15 r)]] ;; 8
          (and (= dirn :S) (= r 8)     (<= 4 c 7))   [:E [(- 15 c) 8]]        ;; 9
          (and (= dirn :S) (= r 8)     (<= 0 c 3))   [:N [11       (- 11 c)]] ;; 10
          (and (= dirn :W) (<= 4 r 7)  (= c -1))     [:N [11       (- 19 r)]] ;; 11
          (and (= dirn :N) (= r 3)     (<= 0 c 3))   [:S [0        (- 11 c)]] ;; 12
          (and (= dirn :N) (= r 3)     (<= 4 c 7))   [:E [(- c 4)  8]]        ;; 13
          (and (= dirn :W) (<= 0 r 3)  (= c 7))      [:S [4        (+ 4 r)]]  ;; 14
          )]
    (if (= (get-in board (second new-posn)) :#)
      [loc 0]
      [new-posn move-remaining])))

(defn new-state-when-off-limits-input [board [dirn [r c :as loc]] move]
  (let [move-remaining (dec move)
        new-posn
        (cond
          (and (= dirn :N) (= r -1)       (<= 50 c 99))   [:E [(+ 100 c) 0]]         ;; 1
          (and (= dirn :N) (= r -1)       (<= 100 c 149)) [:N [199       (- c 100)]] ;; 2
          (and (= dirn :E) (<= 0 r 49)    (= c 150))      [:W [(- 149 r) 99]]        ;; 3
          (and (= dirn :S) (= r 50)       (<= 100 c 149)) [:W [(- c 50)  99]]        ;; 4
          (and (= dirn :E) (<= 50 r 99)   (= c 100))      [:N [49        (+ r 50)]]  ;; 5
          (and (= dirn :E) (<= 100 r 149) (= c 100))      [:W [(- 149 r) 149]]       ;; 6
          (and (= dirn :S) (= r 150)      (<= 50 c 99))   [:W [(+ 100 c) 49]]        ;; 7
          (and (= dirn :E) (<= 150 r 199) (= c 50))       [:N [149       (- r 100)]] ;; 8
          (and (= dirn :S) (= r 200)      (<= 0 c 49))    [:S [0         (+ c 100)]] ;; 9
          (and (= dirn :W) (<= 150 r 199) (= c -1))       [:S [0         (- r 100)]] ;; 10
          (and (= dirn :W) (<= 100 r 149) (= c -1))       [:E [(- 149 r) 50]]        ;; 11
          (and (= dirn :N) (= r 99)       (<= 0 c 49))    [:E [(+ 50 c)  50]]        ;; 12
          (and (= dirn :W) (<= 50 r 99)   (= c 49))       [:S [100       (- r 50)]]  ;; 13
          (and (= dirn :W) (<= 0 r 49)    (= c 49))       [:E [(- 149 r  0)]]        ;; 14
          )]
    ;; TODO: this logic prob not right - need to go back if we hit a :#
    (if (= (get-in board (second new-posn)) :#)
      [loc 0]
      [new-posn move-remaining])))



(deftest pt2
  ;; TODO - check what expected answer actually is
  (is (= 5031 (sut/pt2 example new-state-when-off-limits-example)));; currently 3048
  (is (= 27567 (sut/pt2 input new-state-when-off-limits-input)))  ) ;; too low
