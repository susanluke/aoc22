(ns aoc22.day20
  (:require [clojure.string :as string]))

(def decryption-key 811589153)

(defn parse-file [s]
  (->> s (string/split-lines) (map read-string)))

(defn add-links [i n]
  {:digit n :prev (dec i) :next (inc i)})

(defn index-after-move-fn [l]
  (fn [i] (get-in l [i :next])))

(defn index-after-n-moves [l loop-size i-start num-moves]
  (->> (iterate (index-after-move-fn l) i-start)
       (drop (mod num-moves loop-size))
       first))

(defn splice-out-digit [l i]
  (let [i-prev (get-in l [i :prev])
        i-next (get-in l [i :next])]
    (-> l
        (assoc-in [i-prev :next] i-next)
        (assoc-in [i-next :prev] i-prev))))

(defn splice-in-digit
  "splice i-out so it falls after i-in"
  [l i-ins i-ins-prev]
  (let [i-ins-next (get-in l [i-ins-prev :next])]
    (-> l
        (assoc-in [i-ins :prev] i-ins-prev)
        (assoc-in [i-ins :next] i-ins-next)
        (assoc-in [i-ins-prev :next] i-ins)
        (assoc-in [i-ins-next :prev] i-ins))))

(defn splice-digit [l i i-new-prev]
  (-> l
      (splice-out-digit i)
      (splice-in-digit i i-new-prev)))

(defn mix-digit [l i]
  (let [l-digit-removed (splice-out-digit l i)
        new-prev-i      (index-after-n-moves l-digit-removed
                                             (dec (count l)) ;; -1 as one digit is 'removed'
                                             (get-in l [i :prev])
                                             (get-in l [i :digit]))]
    (splice-in-digit l-digit-removed i new-prev-i)))

(defn find-zero-digit-index [l]
  (->> l
       (map-indexed vector)
       (filter (fn [[i v]] (zero? (:digit v))))
       ffirst))

(defn ptx [s decryption-key]
  (let [enc-file        (->> (parse-file s)
                             (map (partial * decryption-key))
                             (map-indexed add-links)
                             vec)
        last-index      (-> enc-file count dec)
        ;;TODO - data is a bad, generic name.
        data            (-> enc-file
                            (assoc-in [0 :prev] last-index)
                            (assoc-in [last-index :next] 0))
        zero-index      (find-zero-digit-index data)
        loop-size       (count data)
        plain-text-file (reduce mix-digit data (flatten (repeat 10 (range (count data)))))]
    (apply + [(get-in plain-text-file [(index-after-n-moves plain-text-file loop-size zero-index 1000) :digit])
              (get-in plain-text-file [(index-after-n-moves plain-text-file loop-size zero-index 2000) :digit])
              (get-in plain-text-file [(index-after-n-moves plain-text-file loop-size zero-index 3000) :digit])     ])



    )
  )

(defn pt1 [s]
  (let [enc-file   (->> (parse-file s)
                        (map-indexed add-links)
                        vec)
        last-index (-> enc-file count dec)
        ;;TODO - data is a bad, generic name.
        data       (-> enc-file
                       (assoc-in [0 :prev] last-index)
                       (assoc-in [last-index :next] 0))
        zero-index (find-zero-digit-index data)
        loop-size  (count data)
        plain-text (reduce mix-digit data (range loop-size))]
    ( +
     (get-in plain-text [(index-after-n-moves plain-text loop-size zero-index 1000) :digit])
     (get-in plain-text [(index-after-n-moves plain-text loop-size zero-index 2000) :digit])
     (get-in plain-text [(index-after-n-moves plain-text loop-size zero-index 3000) :digit]))))

(defn pt2 [s]
  (let [enc-file        (->> (parse-file s)
                             (map (partial * decryption-key))
                             (map-indexed add-links)
                             vec)
        last-index      (-> enc-file count dec)
        ;;TODO - data is a bad, generic name.
        data            (-> enc-file
                            (assoc-in [0 :prev] last-index)
                            (assoc-in [last-index :next] 0))
        zero-index      (find-zero-digit-index data)
        loop-size       (count data)
        plain-text-file (reduce mix-digit data (flatten (repeat 10 (range (count data)))))]
    (apply + [(get-in plain-text-file [(index-after-n-moves plain-text-file loop-size zero-index 1000) :digit])
              (get-in plain-text-file [(index-after-n-moves plain-text-file loop-size zero-index 2000) :digit])
              (get-in plain-text-file [(index-after-n-moves plain-text-file loop-size zero-index 3000) :digit])     ])



    ))

(comment "
Look at https://groups.google.com/g/greenville-clojure/c/B4RHM-lIC-k?pli=1

'conj' efficiently adds an element to the front of a list or to the back of a
vector.  Chouser mentions that if you need to do insertions into an immutable
vector-like data structure, you probably want finger trees.  He later went on
and wrote a contrib library: https://github.com/clojure/data.finger-tree and
gave a talk about it: https://www.youtube.com/watch?v=UXdr_K0Lwg4

What's probably more efficient (by constant factors, I think) today is RRB trees
and there's a contrib library for that too:
https://github.com/clojure/core.rrb-vector.  We can probably forgive Chouser and
Rich for not mentioning RRB trees since the paper describing them was written
after that thread (3/11/2012) :-)
"

         "
I wonder what the best way to do this is, consider:

- vectors

- lists

- homemade
   - vector of maps - start points to end, and end points to start
   - each digit has a prev and a next link
   - go to each digit in the vec
   - mod vec length, to see how many to go forward (forget about going backward)
       - excise from current loc by changing next of prev and prev of next
       - 'move' forward that lenth, tracing through links.
       - insert at that point by pointing prev's next to it and next's prev to
         it and setting next/prev for digit
   - repeat with next digit in vector.

   ADVANTAGES
   * No need to shuffle data around
   * Need to trace through chain to find new place.

// From webpage

Initial arrangement:
1, 2, -3, 3, -2, 0, 4

1 moves between 2 and -3:
2, 1, -3, 3, -2, 0, 4

2 moves between -3 and 3:
1, -3, 2, 3, -2, 0, 4

-3 moves between -2 and 0: (sl: this is equiv to moving forward 4
1, 2, 3, -2, -3, 0, 4

3 moves between 0 and 4:
1, 2, -2, -3, 0, 3, 4

-2 moves between 4 and 1:
1, 2, -3, 0, 3, 4, -2

0 does not move:
1, 2, -3, 0, 3, 4, -2

4 moves between -3 and 0:
1, 2, -3, 4, 0, 3, -2

Then, the grove coordinates can be found by looking at the 1000th, 2000th, and
3000th numbers after the value 0, wrapping around the list as necessary. In the
above example, the 1000th number after 0 is 4, the 2000th is -3, and the 3000th
is 2; adding these together produces 3.


with 7 numbers in the list,
- the number moving has 6 gaps to go into
 0 -> index stays the same.




"

         )
