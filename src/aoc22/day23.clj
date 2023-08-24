(ns aoc22.day23
  (:require [clojure.string :as string]))

(def neighbours (->> (for [x (range -1 2) y (range -1 2)]
                       [x y])
                     (remove #(= [0 0] %))))

(def north (filter #(= -1 (first %))  neighbours))
(def south (filter #(=  1 (first %))  neighbours))
(def west  (filter #(= -1 (second %)) neighbours))
(def east  (filter #(=  1 (second %)) neighbours))

(def initial-move-order [north south west east])

(defn parse-char [r c ch] (when (= \# ch) [r c]))
(defn parse-line [r s] (map-indexed #(parse-char r % %2) s))

(defn parse-input [s]
  (->> (string/split-lines s)
       (map-indexed parse-line)
       (apply concat)
       (remove nil?)
       set))

(defn move [elves elf proposed-moves move-order]
  (map +)
  )


(defn first-half-of-round [])
(defn second-half-of-round [])

(defn pt1 [s]
  (let [elves (parse-input s)]
    ()))


(comment
  "
Thoughts

- should we represent map as a set of locations where we have elves? or a 2D array?

- think latter is best
"

  "
During the first half of each round, each Elf considers the eight positions
adjacent to themself. If no other Elves are in one of those eight positions, the
Elf does not do anything during this round. Otherwise, the Elf looks in each of
four directions in the following order and proposes moving one step in the first
valid direction:

If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.

After each Elf has had a chance to propose a move, the second half of the round
can begin. Simultaneously, each Elf moves to their proposed destination tile if
they were the only Elf to propose moving to that position. If two or more Elves
propose moving to the same position, none of those Elves move.

Finally, at the end of the round, the first direction the Elves considered is
moved to the end of the list of directions. For example, during the second
round, the Elves would try proposing a move to the south first, then west, then
east, then north. On the third round, the Elves would first consider west, then
east, then north, then south.

"  )
