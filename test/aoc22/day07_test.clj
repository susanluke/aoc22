(ns aoc22.day07-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day07 :as sut]))

(def input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(def f "input07.txt")
(defn read-file [f] (-> f io/resource slurp))
(def input2 (read-file f))

(deftest part1
  (is (= 95437 (sut/part1 input)))
  (is (= 2104783 (sut/part1 input2))))

(deftest part2
  (is (= 24933642 (sut/part2 input)))
  (is (= 5883165 (sut/part2 input2))))
