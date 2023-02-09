(ns aoc22.day17
  (:require [clojure.string :as string]))

(def chamber-width 7)

(def rock-shapes "####\n\n.#.\n###\n.#.\n\n..#\n..#\n###\n\n#\n#\n#\n#\n\n##\n##\n")
(def rock-shapes
  "####

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

(defn string->keywords [rl]
  (mapv (comp keyword str) rl))

(defn read-rock [r]
  (->> (string/split-lines r) ;; each rock contains lines
       (mapv string->keywords)))

(defn rc->xy [r-max [r c]] [c (- r-max r)])
(defn rock->xy [rock]
  (let [r-max (dec (count rock))]
    (->> (for [r (range (count rock))
               c(range (count (first rock)))]
           (when (= (get-in rock [r c]) :#)
             [r c]))
         (remove nil?)
         (map (partial rc->xy r-max)))))

(defn read-rocks [rock-shapes]
  (->> (string/split rock-shapes #"\n\n") ;; break into individual rocks
       (mapv read-rock)
       (map rock->xy)))

(defn move-rock-x-axis [d rock] (map (fn [[x y]] [(+ x d) y]) rock))
(defn move-rock-y-axis [d rock] (map (fn [[x y]] [x (+ y d)]) rock))
(defn move-rock-down   [rock]   (move-rock-y-axis -1 rock))
(defn move-rock-left   [rock]   (move-rock-x-axis -1 rock))
(defn move-rock-right  [rock]   (move-rock-x-axis +1 rock))

(defn within-bounds [settled-rocks [x y]]
  (and (<= 0 x (dec chamber-width))
       (<= 0 y)
       (not (settled-rocks [x y]))))

(defn move [direction rock]
  ((case direction
     :<    move-rock-left
     :>    move-rock-right
     :down move-rock-down)
   rock))

(defn can-move? [settled-rocks direction rock]
  (->> (move direction rock)
       (every? (partial within-bounds settled-rocks))))

(defn max-rock-height [rock]
  (->> (map second rock)
       (apply max)
       inc));; height is +1 max y

;; TODO - need to record a more detailed topology - trace/recurse over the
;; surface of the rocks to record overhangs.
;; prob track max-y for x=1, so have a starting point
(defn find-y-coord [x rock]
  (->> (conj rock [x -1]);; add a baseline,
       (filter (fn [[x-rock y-rock]] (= x x-rock)))
       (map second)
       (apply max)))

(defn y-coords [rock]
  (map #(find-y-coord % rock) (range chamber-width)))

(defn new-profile [old-profile new-rock]
  (map max old-profile (y-coords new-rock)))

(defn normalize-profile [profile]
  ;;(println "profile:" profile "normalized: " (concat [0] (map #(- % (first profile)) (rest profile))))
  (concat [0] (map #(if (< 800 (abs (- (first profile) %)))
                      800 ;; assume a 'cliff' if too large
                      (abs (- (first profile) %)))
                   (rest profile))))

(defn check-cycle
  "check if we've got through a complete cycle of jets/rocks.  If we have, then "
  [{:keys [max-height rock-count cycle-length profile profiles-seen] :as chamber-state}
   rock-after-jet]
  (let [profile-new (normalize-profile (new-profile profile rock-after-jet))]
    (if (zero? (mod rock-count cycle-length))
      (do (println "At a cycle end: " rock-count (/ rock-count cycle-length) max-height )
          (when (profiles-seen profile-new)
            (println "cycle seen" profile-new "rock-count:" rock-count "max-height" max-height))
          (-> chamber-state
              (assoc :profile profile-new)
              (assoc :profiles-seen (assoc profiles-seen profile-new {:rock-count rock-count
                                                                      :max-height max-height}))))

      ;; if not end of a cycle, just return 0
      (assoc chamber-state :profile profile-new))))

(defn add-rock [{:keys [max-height settled-rocks jets rock-count profile profiles-seen cycle-length] :as chamber-state} rock]
  (let [rock-start (->> rock
                        (move-rock-x-axis 2)
                        (move-rock-y-axis (+ max-height 3)))]
    (loop [rock          rock-start
           jets          jets
           settled-rocks settled-rocks
           max-height    max-height]
      (let [jet-move       (first jets)
            jets-rest      (rest jets)
            rock-after-jet (if (can-move? settled-rocks jet-move rock)
                             (move jet-move rock)
                             rock)]
        ;;(println "rock-after-jet:" rock-after-jet)
        ;;(println "settled-rocks:" settled-rocks)
        (if (can-move? settled-rocks :down rock-after-jet)
          (recur (move :down rock-after-jet) jets-rest settled-rocks max-height)
          (check-cycle (-> chamber-state
                           (assoc :max-height (max max-height (max-rock-height rock-after-jet)))
                           (assoc :settled-rocks (apply conj settled-rocks rock-after-jet))
                           (assoc :jets jets-rest)
                           (update :rock-count inc))
                       rock-after-jet))))))

(defn pt1 [jet-pattern num-rocks]
  (let [rocks      (read-rocks rock-shapes)
        rock-cycle (cycle rocks)
        jets       (cycle (string->keywords jet-pattern))]
    (->> (take num-rocks rock-cycle)
         (reduce add-rock
                 {:max-height    0
                  :settled-rocks #{}
                  :jets          jets
                  :rock-count    0
                  :profile       (repeat chamber-width -1)
                  :profiles-seen {}
                  :cycle-length  (* (count rocks) (count jet-pattern))
                  ;;:cycle-length  40 ;; hard code to 40 for jet-pattern
                  })
         :max-height)))

(defn print-rocks [settled-rocks]
  (concat
   (concat [\|] (repeat chamber-width \.) [\|])
   (vec "+-------+")
   ))


(comment
  " Do we care about r,c?  Read as r/c first (easy), convert to x,y and work
with that for remainder of the challenge

- - -> c
|
|
v
r

y
^
|
|
 - - > x

")
