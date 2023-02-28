(ns aoc22.day18
  (:require [clojure.string :as string]))

(defn parse-lava-droplet [s]
  (->> (string/split s #",")
       (mapv read-string)))

(defn parse-input [s]
  (->> (string/split-lines s)
       (map parse-lava-droplet)))

(defn neighbours [d]
  (for [f [inc dec]
        i (range (count d))]
    (update d i f)))

(defn neighbours-in-box-not-touching-lava
  "neighbouring spaces inside box and not occupied by lava.  Used to
  model how water fills the space."
  [[[xl xh] [yl yh] [zl zh]] lava d]
  (->> (neighbours d)
       (filter (fn [[x y z]]
                 (and (<= xl x xh)
                      (<= yl y yh)
                      (<= zl z zh))))
       (remove lava)))

(defn count-touching-water-faces [water d]
  (->> (neighbours d)
       (filter water)
       count))

(defn count-exposed-faces [lava d]
  (->> (neighbours d)
       (remove lava)
       count))

(defn pt1 [s]
(let [lava (parse-input s)]
  (->> lava
       (map (partial count-exposed-faces (set lava)))
       (apply +))))

(defn bounding-box [lava]
(for [i (range (count (first lava)))]
  (->> (map #(nth % i) lava)
       ((fn [ns]
          [(-> (apply min ns) dec)
           (-> (apply max ns) inc)])))))

(defn fill-water [box-dims lava visited to-visit w]
  (let [ns           (->> (neighbours-in-box-not-touching-lava box-dims lava w)
                          (remove visited))
        to-visit-new (->> (concat to-visit ns)
                          (remove visited))
        next-w       (first to-visit-new)]
    (if (nil? next-w)
      visited
      (recur box-dims lava (conj visited w) (rest to-visit-new) next-w))))

(defn pt2 [s]
  (let [lava        (parse-input s)
        box-dims    (bounding-box lava)
        water-start (mapv first box-dims)
        water       (fill-water box-dims (set lava) #{} '() water-start)]
    (->> lava
         (map (partial count-touching-water-faces water))
         (apply +))))
