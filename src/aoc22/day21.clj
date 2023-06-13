(ns aoc22.day21
  (:require [clojure.string :as string]))

(def ops {:+ + :- - :* * :/ /})
;; pick a number high enough that includes solution, but not too high to get an
;; overflow error
(def search-start (long 4e12))

(defn parse-line [s]
  (if-let [[_ name v] (re-matches #"([a-z]{4}): (\d+)" s)]
    (vector (keyword name) (read-string v))
    (let [[_ name n1 op n2] (re-matches #"([a-z]{4}): ([a-z]{4}) ([\+\-\*\/]) ([a-z]{4})" s)]
      (vector (keyword name) [(keyword n1) (keyword op) (keyword n2)]))))

(defn parse-string [s]
  (->> s
       (string/split-lines)
       (mapv parse-line)
       (into {})))

(defn get-val [n monkeys]
  (let [v (n monkeys)]
    (if (int? v)
      v
      (let [[n1 op n2] v]
        ((op ops) (get-val n1 monkeys) (get-val n2 monkeys))))))

(defn pt1 [s] (get-val :root (parse-string s)))

(defn bin-search-humn-num [target n monkeys lo hi]
  (let [hi-val (get-val n (assoc monkeys :humn hi))
        lo-val (get-val n (assoc monkeys :humn lo))]
    (cond
      (= hi-val target)
      hi

      (= lo-val target)
      lo

      :else
      ;; try midway
      (let [mid     (long (/ (+ hi lo) 2))
            mid-val (get-val n (assoc monkeys :humn mid)) ]
        (cond
          (or (= lo mid) (= mid hi))
          "solution not found"

          (or (<= lo-val target mid-val)
              (<= mid-val target lo-val))
          (recur target n monkeys lo mid)

          :else
          (recur target n monkeys mid hi))))))


(defn pt2 [s]
  (let [monkeys    (parse-string s)
        [l _ r]    (:root monkeys)
        ;; in both examples, target is on rhs and :humn only changes the lhs
        target     (get-val r monkeys)]
    (bin-search-humn-num target l monkeys (- search-start) search-start)))
