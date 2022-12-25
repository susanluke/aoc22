(ns aoc22.day11
  (:require [clojure.string :as string]))

(defn split-monkeys [s]
  (string/split s #"\n\n"))

(defn parse-items [s]
  (->> (string/split s #", ")
       (map read-string)))

(defn parse-op-element [s]
  (case s
    "*" * "+" + "old" :old
    (read-string s)))

(defn parse-op [s]
  (->> (string/split s #"\s")
       (map parse-op-element)
       (sort (fn [e1 e2] (boolean (#{* +} e1))))))

(defn parse-monkey [s]
  (let [[_ m items op test t f] (re-matches #"Monkey (\d+):
  Starting items: ([0-9, ]+)
  Operation: new = ([old 0-9*+]+)
  Test: divisible by (\d+)
    If true: throw to monkey (\d+)
    If false: throw to monkey (\d+)" (string/trim-newline s))]
    [(read-string m) {:items           (parse-items items)
                      :op              (parse-op op)
                      :test            (read-string test)
                      :if-true         (read-string t)
                      :if-false        (read-string f)
                      :num-inspections 0}]))

(defn monkey-op [extra-op op item]
  (->> (map #(if (= :old %) item %) op)
       eval
       extra-op))

(defn monkey-inspect-item [extra-op {:keys [op test if-true if-false] :as monkey} item]
  (let [new-item (monkey-op extra-op op item)]
    (if (= 0 (mod new-item test))
      [if-true new-item]
      [if-false new-item])))

(defn monkey-turn [extra-op start-turn-state monkey-index]
  (let [{:keys [items] :as monkey} (get start-turn-state monkey-index)]
    (->> items
         (map (partial monkey-inspect-item extra-op monkey))
         (reduce (fn [state [to-monkey item]]
                   (update-in state [to-monkey :items] conj item))
                 (-> start-turn-state
                     (assoc-in [monkey-index :items] '())
                     (update-in [monkey-index :num-inspections] + (count items)))))))

(defn monkey-round [extra-op state round-id]
  (reduce (partial monkey-turn extra-op) state (range (count state))))

(defn ptx [s num-rounds gen-extra-op]
  (let [init-state (->> (split-monkeys s)
                        (map parse-monkey)
                        (into {}))
        extra-op   (gen-extra-op init-state)]
    (->> (reduce (partial monkey-round extra-op) init-state (range num-rounds))
         vals
         (map :num-inspections)
         (sort >)
         (take 2)
         (apply *))))

(defn pt1 [s]
  (ptx s 20 (constantly (fn [n] (int (/ n 3))))))

(defn pt2 [s]
  (ptx s 10000 (fn [init-state]
                 (let [divisor (->> init-state vals (map :test) (apply *))]
                   (fn [n] (mod n divisor))))))
