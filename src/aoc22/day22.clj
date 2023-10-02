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

;; todo: remove this when the new one works
(defn new-posn-walk-old
  [board [dirn loc] last-valid-posn move]
  (println dirn loc last-valid-posn move)
  (if (zero? move)
    last-valid-posn
    (let [board-dims [(count board) (apply max (map count board))]
          new-loc (->> (dirn->move dirn)
                       (map + loc)
                       (#(mapv mod % board-dims)))]
      (case (get-in board new-loc )
        :# last-valid-posn ;; blocked, so stick at last valid posn
        :. (recur board [dirn new-loc] [dirn new-loc] (dec move)) ;; move to space
        ;; 'skip' as it's a blank (:_) or nil
        (recur board [dirn new-loc] last-valid-posn move)))))

(defn new-loc-when-off-limits [board loc]
  (let [board-dims [(count board) (apply max (map count board))]
        new-loc    (mapv mod loc board-dims)]
    new-loc))

(comment "
situations when we're OOB
* outside the 2d array - negative r/c or r/c greater than max [ nil val]
* 'inside the 2d array',but off the end [nil val] <- why can't we wrap early? (going down, but want to keep in col)
  - think we could only wrap on the base of direction
* :_ - not in the space - just keep moving, but don't dec moves
")
(defn new-state-when-off-limits [board [dirn [r c :as loc]] move]
  ;; returns [dirn loc] move
  (let [loc-val (get-in board loc)]
    ;; should we really dec move when 'wrapping' round board?
    ;; we've got to do it when we re-enter valid territory anyway, so maybe
    ;; it's just programmatically easier to do it now?
    (cond
      (and (nil? loc-val)
           (#{:N :S} dirn))
      [[dirn [r (mod c (count board))]] (dec move)]

      (and (nil? loc-val)
           (#{:E :W} dirn))
      [[dirn [(mod r (count (nth board r))) c]] (dec move)]

      (= :_ loc-val)
      [[dirn [r c]] move])))

(defn new-posn-walk
  [board [dirn loc] last-valid-posn move]
  ;;(println dirn loc last-valid-posn move)
  (if (zero? move)
    last-valid-posn
    (let [new-loc (->> (dirn->move dirn) (mapv + loc))]
      (case (get-in board new-loc )
        :# last-valid-posn ;; blocked, so stick at last valid posn
        :. (recur board [dirn new-loc] [dirn new-loc] (dec move)) ;; move to space
        ;; 'skip' as it's a blank (:_) or nil
        (let [new-loc-after-oob (new-loc-when-off-limits board new-loc)]
          (if (and (not= new-loc new-loc-after-oob )
                   ;; dec for :_ as we'll hit valid ground soon
                   (#{:. :_} (get-in board new-loc-after-oob)))
            (recur board
                   [dirn new-loc-after-oob]
                   last-valid-posn
                   (dec move))
            (recur board
                   [dirn new-loc-after-oob]
                   last-valid-posn
                   move)))))))

(defn new-posn-turn
  [board [dirn loc] move]
  [(if (= :R move) (turn-right dirn) (turn-left dirn)) loc])

(defn new-posn
  ([board posn move]
   (new-posn board posn posn move))
  ([board [dirn [r c :as loc] :as posn] last-valid-posn move]
   (if (int? move)
     (new-posn-walk board posn last-valid-posn move)
     (new-posn-turn board posn move))))

(defn pt1 [input]
  (let [[board moves] (parse-input input)
        [final-dirn [final-r final-c]] (reduce (partial new-posn board)
                                               (initial-position board)
                                               moves)]
    (+ (* 1000 (inc final-r))
       (* 4 (inc final-c))
       (dirn->score final-dirn))))
