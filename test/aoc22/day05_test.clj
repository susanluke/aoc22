(ns aoc22.day05-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day05 :as sut]))

(def input
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def f "input05.txt")
(defn read-file [f] (-> f io/resource slurp))
(def input2 (read-file f))

(deftest init-state
  (is (= '((\N \Z) (\D \C \M) (\P))
         (sut/parse-init-state ["    [D]" "[N] [C]" "[Z] [M] [P]"]))))

(deftest part-1
  (is (= "CMZ"
         (sut/part-1 input)))
  (is (= "TLNGFGMFN"
         (sut/part-1 input2))))

(deftest part-2
  (is (= "MCD"
         (sut/part-2 input)))
  (is (= "FGLQJCMBD"
         (sut/part-2 input2))))
