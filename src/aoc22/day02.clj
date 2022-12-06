(ns aoc22.day02
  (:require [clojure.string :as string]))

(def f "input02.txt")

(def moves {"A" :rock "B" :paper "C" :scissors
            "X" :rock "Y" :paper "Z" :scissors})

(def moves2 {"A" :rock "B" :paper "C" :scissors
             "X" :lose "Y" :draw "Z" :win})

(def results {:rock     {:paper :win :rock :draw :scissors :lose
                         :win :paper :lose :scissors :draw :rock}
              :paper    {:scissors :win :paper :draw :rock :lose
                         :win :scissors :lose :rock :draw :paper}
              :scissors {:rock :win :scissors :draw :paper :lose
                         :win :rock :lose :paper :draw :scissors}})

(def scores {:rock 1 :paper 2 :scissors 3
             :win  6 :draw  3 :lose     0})

(defn read-file [f]
  (-> f (io/resource) slurp string/split-lines))

(defn parse [moves s]
  (->> (string/split s #" ")
       (map moves)))

(defn score [[h1 h2]]
  (+ (scores ((h1 results) h2))
     (scores h2)))

(defn part1 []
  (->> (read-file f)
       (map (partial parse moves))
       (map score)
       (apply +)))

(defn part2 []
  (->> (read-file f)
       (map (partial parse moves2))
       (map score)
       (apply +)))
