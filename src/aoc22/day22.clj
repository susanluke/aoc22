(ns aoc22.day22
  (:require [clojure.string :as string]))

(def dirn->move  {:N [-1  0] :S [ 1  0] :W [ 0 -1] :E [ 0  1]})
(def dirn->score {:N 3 :E 0 :W 2 :S 1})
(def turn-right  {:N :E :E :S :S :W :W :N})
(def turn-left   {:N :W :E :N :S :E :W :S})

(defn parse-line [s]
  (->> s
       (replace {\space \_})
       (mapv (comp keyword str))))

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

;; TODO - need to handle the situation where we can move over 'non-existant'
;; space, but end up getting blocked before we hit a rock.
(defn move
  ([board posn path-instr]
   (move board posn posn path-instr))
  ([board [dirn [r c :as loc] :as posn] last-valid-posn path-instr]
   (if (int? path-instr)
     ;; it's a number, so we move along
     (if (zero? path-instr)
       last-valid-posn
       (let [new-loc (->> (dirn->move dirn)
                          (map + loc)
                          (#(mapv mod % [(count board) (count (nth board r))])))]
         (case (get-in board new-loc)
           :_ (move board [dirn new-loc] last-valid-posn path-instr) ;; 'skip' as it's a blank
           :. (move board [dirn new-loc] (dec path-instr)) ;; move to space
           :# last-valid-posn ;; blocked, so stick at current loc
           (println "posn:" posn "path-instr:" path-instr (get-in board new-loc) new-loc [(count board) (count (nth board r))]))))
     ;; it's a spin, so we rotate
     [(if (= :R path-instr)
        (turn-right dirn)
        (turn-left dirn))
      loc])))

(defn pt1 [input]
  (let [[board moves] (parse-input input)
        [final-dirn [final-r final-c]] (reduce (partial move board)
                                               (initial-position board)
                                               moves)]
    (+ (* 1000 (inc final-r))
       (* 4 (inc final-c))
       (dirn->score final-dirn))))
