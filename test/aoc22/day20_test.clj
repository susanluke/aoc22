(ns aoc22.day20-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc22.day20 :as sut])  )

(def encrypted-file-example
  "1
2
-3
3
-2
0
4")

(def encrypted-file (-> "input20.txt" io/resource slurp))
