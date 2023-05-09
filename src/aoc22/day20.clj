(ns aoc22.day20
  (:require [clojure.string :as string]))

(def pt2-decryption-key 811589153)

(defn parse-file [s] (->> s (string/split-lines) (map read-string)))

(defn add-links [i n] {:digit n :prev (dec i) :next (inc i)})

(defn index-after-n-moves [l loop-size i-start num-moves]
  (->> (iterate (fn [i] (get-in l [i :next])) i-start)
       (drop (mod num-moves loop-size))
       first))

(defn splice-out-digit [l i]
  (let [i-prev (get-in l [i :prev])
        i-next (get-in l [i :next])]
    (-> l
        (assoc-in [i-prev :next] i-next)
        (assoc-in [i-next :prev] i-prev))))

(defn splice-in-digit
  [l i-ins i-ins-prev]
  (let [i-ins-next (get-in l [i-ins-prev :next])]
    (-> l
        (assoc-in [i-ins :prev] i-ins-prev)
        (assoc-in [i-ins :next] i-ins-next)
        (assoc-in [i-ins-prev :next] i-ins)
        (assoc-in [i-ins-next :prev] i-ins))))

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

(defn ptx [s num-mixes decryption-key]
  (let [enc-file        (->> (parse-file s)
                             (map (partial * decryption-key))
                             (map-indexed add-links)
                             vec)
        loop-size       (count enc-file)
        last-index      (dec loop-size)
        zero-index      (find-zero-digit-index enc-file)
        l               (-> enc-file
                            (assoc-in [0 :prev] last-index)
                            (assoc-in [last-index :next] 0))
        plain-text-file (reduce mix-digit l (flatten (repeat num-mixes (range loop-size))))]
    (apply + (map #(get-in plain-text-file
                           [(index-after-n-moves plain-text-file loop-size zero-index %) :digit])
                  [1000 2000 3000]))))

(defn pt1 [s] (ptx s 1 1))
(defn pt2 [s] (ptx s 10 pt2-decryption-key))
