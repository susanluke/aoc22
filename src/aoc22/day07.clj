(ns aoc22.day07
  [:require [clojure.string :as string]])

(def disk-cap 70000000)
(def update-size 30000000)

(def regs
  {:cd #"^\$ cd ([a-z/.]+)$"
   :ls #"^\$ ls$"
   :dir #"^dir ([a-z]+)$"
   :file #"^([0-9]+) ([a-z.]+)$"})

(def init-state
  {:fs {:/ {:files {}}}
   :wd [:fs]})

(defn process-cd [state s]
  (let [[_ dirname] (re-find (:cd regs) s)]
    (if (= dirname "..")
      (update state :wd pop)
      (update state :wd #(conj % (keyword dirname))))))

(defn process-dir [{:keys [wd] :as state} s]
  (let [[_ dirname] (re-find (:dir regs) s)]
    ;; prob should check whether it already exists before refreshing it
    (update-in state wd #(assoc % (keyword dirname) {:files {}}) )))

(defn process-file [{:keys [wd] :as state} s]
  (let [[_ filesize filename] (re-find (:file regs) s)]
    (update-in state (conj wd :files) #(assoc % (keyword filename) (read-string filesize)))))

(defn process-line [state s]
  (cond
    (re-find (:cd regs) s)
    (process-cd state s)

    (re-find (:dir regs) s)
    (process-dir state s)

    (re-find (:file regs) s)
    (process-file state s)

    :else
    state))

(defn process-cmd-lines [ss]
  (reduce process-line init-state (string/split-lines ss)))

(defn dir-sizes [fs]
  (let [dirs    (-> fs (dissoc :files) keys)
        dirsums (->> dirs (map fs) (map dir-sizes))
        dirsum  (apply + (map first dirsums))
        filesum (->> fs :files vals (apply +))]
    [(+ filesum dirsum) dirsums]))

(defn strings->dir-sizes [ss]
  (-> (process-cmd-lines ss)
      (get-in  [:fs :/])
      dir-sizes
      flatten))

(defn part1 [ss]
  (->> (strings->dir-sizes ss)
       (filter #(< % 100000))
       (apply +)))

(defn part2 [ss]
  (let [sizes  (string->dir-sizes ss)
        total-size (first sizes)
        need-to-free (- (+ update-size total-size)
                        disk-cap)]
    (->> sizes
         (filter #(< need-to-free %))
         (apply min))))
