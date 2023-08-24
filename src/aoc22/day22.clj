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
    (if (not= loc new-loc)
      (do
        ;;        (println "*")
        new-loc ;; we're outside boundary, try the modded location
        )
      loc ;; we're still in boundary, just move along
      )))

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
        (let [new-loc-2 (new-loc-when-off-limits board new-loc)]
          (if (and (not= new-loc new-loc-2 )
                   (#{:. :_} (get-in board new-loc-2)))
            (recur board
                   [dirn new-loc-2]
                   last-valid-posn
                   (dec move))
            (recur board
                   [dirn new-loc-2]
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
