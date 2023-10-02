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
  (is (= 30552 (sut/pt1 input))) ;; 117028 is too high
  )


(comment
  "Now when we go of the edge, we might need to 'turn' and move to another tile"

  "
          ...#
          .#..
          #...
          ....

...# .... ...#
.... .... #...
..#. ...# ....
.... .... ..#.

          ...# ....
          .... .#..
          .#.. ....
          .... ..#.

0-3  4-7  8-11 12-15

Ideas:

manually create lookup:

of dirn [r c] -> dirn [r c]

eg,

:^ [-1     [8-11]]  -> :v [4        (- 11 c)]  (& converse)
:> [[0-3]   12]     -> :< [(- 11 r) 11]
:> [[4-7]   12]     -> :V [8        (- 19 r)]
:^ [7      [12-15]] -> :< [11 (- 19 c) ]
:> [[8-11]  16]     -> :< [(- 11 r) 11]
:v [12     [12-15]  ->
:v [12     [8-11]]  ->
:< [[8-11]  7]      ->
:v [8      [4-7]]   -> :> [(+ 4 c) 8]
:v [8      [0-3]]
:< [[4-7]  -1]
:^ [3      [0-3]]
:^ [3      [4-7]]
:< [[0-3]  7]]      -> :v []


 XX
 X
XX
X



(19-r)


3 - (c-8)
11 -c






:^ [ 3 *] -> "

  " another way: organise 2D array into 3x 2D strips , one for moving in x, y, &
z direction.  Need to be able to map between strips and back to original
dimensions of the array in order to calculate the end result.

think this is more complicated"

  )
