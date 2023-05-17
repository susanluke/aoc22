(ns aoc22.day21-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day21 :as sut]))
(def riddle-example
  "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(def riddle (-> "input21.txt" io/resource slurp))

(deftest pt1
  (is (= 152 (sut/pt1 riddle-example)))
  (is (= 286698846151845 (sut/pt1 riddle))))

(deftest pt2
  (is (= 301 (sut/pt2 riddle-example)))
  (is (= 3759566892641 (sut/pt2 riddle))))
