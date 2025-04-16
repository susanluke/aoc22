(ns aoc22.day22
  (:require [clojure.string :as string]))

(def dirn->move  {:N [-1  0] :S [ 1  0] :W [ 0 -1] :E [ 0  1]})
(def dirn->score {:N 3 :E 0 :W 2 :S 1})
(def turn-right  {:N :E :E :S :S :W :W :N})
(def turn-left   {:N :W :E :N :S :E :W :S})

(defn parse-line [s]
  (->> s (replace {\space \_}) (mapv (comp keyword str))))

(defn read-instructions [instr-acc is-dirn? path]
  (let [re                (if is-dirn? #"[LR]" #"\d")
        [instr path-rest] (split-with #(re-matches re (str %)) path)
        instr-acc-new     (conj instr-acc (if is-dirn?
                                            (keyword (str (first instr)))
                                            (read-string (apply str instr))))]
    (if (seq path-rest)
      (recur instr-acc-new (not is-dirn?) path-rest)
      instr-acc-new)))

(defn parse-input [s]
  (let [[board path] (string/split s #"\n\n")]
    [(->> board (string/split-lines) (mapv parse-line))
     (->> path string/trim-newline (read-instructions [] false))]))

(defn initial-position [board]
  [:E [0 (->> board first (split-with #(not= :. %)) first count)]])


(defn new-state-when-off-limits [board [dirn [r c :as loc]] move]
  ;; returns [dirn loc] move
  (let [loc-val (get-in board loc)]
    ;; NB, we (dec move) when we wrap, assuming we will step onto valid ground
    ;; at some point.  Programatically easier to do it once at the actual wrap,
    ;; rather that trying to detect point we step onto valid ground again.
    (cond
      (and (#{:N :S} dirn)
           (not (<= 0 r (dec (count board)))))
      (let [new-loc [(mod r (count board)) c]]
        [[dirn new-loc] (dec move)])

      (and (#{:E :W} dirn)
           (not (<= 0 c (dec (count (nth board r))))))
      (let [new-loc [r (mod c (count (nth board r)))]]
        [[dirn new-loc] (dec move)])

      :else
      [[dirn [r c]] move])))

;; TODO: overload this, so we dont' ahve to send it duplicate last valid posn
(defn new-posn-walk
  [board oob-fn [dirn loc] last-valid-posn move]
  (if (zero? move)
    last-valid-posn
    (let [new-loc (->> (dirn->move dirn) (mapv + loc))]
      (case (get-in board new-loc )
        ;; blocked, so stick at last valid posn
        :# last-valid-posn

        ;; move to space
        :. (recur board oob-fn [dirn new-loc] [dirn new-loc] (dec move))

        ;; out of bounds - call new-state-when-off-limits, to work out where we
        ;; should go, and recur
        (let [[[new-dirn new-loc2] new-move]
              (oob-fn board [dirn new-loc] move)]
          (recur board oob-fn [new-dirn new-loc2] last-valid-posn new-move))))))

(defn new-posn-turn
  [board [dirn loc] move]
  [(if (= :R move) (turn-right dirn) (turn-left dirn)) loc])

(defn new-posn
  ([board oob-fn posn move]
   (new-posn board oob-fn posn posn move))
  ([board oob-fn [dirn [r c :as loc] :as posn] last-valid-posn move]
   (if (int? move)
     (new-posn-walk board oob-fn posn last-valid-posn move)
     (new-posn-turn board posn move))))

(defn pt1 [input]
  (let [[board moves] (parse-input input)

        [final-dirn [final-r final-c]]
        (reduce (partial new-posn
                         board new-state-when-off-limits)
                (initial-position board)
                moves)]
    (+ (* 1000 (inc final-r))
       (* 4 (inc final-c))
       (dirn->score final-dirn))))

(defn pt2 [input oob-fn]
  (let [[board moves] (parse-input input)

        [final-dirn [final-r final-c]]
        (reduce (partial new-posn
                         board oob-fn)
                (initial-position board)
                moves)]
    (println "final position:" final-r final-c final-dirn)
    (+ (* 1000 (inc final-r))
       (* 4 (inc final-c))
       (dirn->score final-dirn))))
