(ns aoc22.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def f "input02.txt")

(def moves {"A" :rock "B" :paper "C" :scissors
            "X" :rock "Y" :paper "Z" :scissors})

(def moves2 {"A" :rock "B" :paper "C" :scissors
             "X" :lose "Y" :draw "Z" :win})

(def results {:rock     {:paper :win :rock :draw :scissors :lose}
              :paper    {:scissors :win :paper :draw :rock :lose}
              :scissors {:rock :win :scissors :draw :paper :lose}})

(def results2 {:rock     {:win :paper :lose :scissors :draw :rock}
               :paper    {:win :scissors :lose :rock :draw :paper}
               :scissors {:win :rock :lose :paper :draw :scissors}})


(def scores {:rock 1 :paper 2 :scissors 3
             :win  6 :draw  3 :lose     0})

(defn read-file [f]
  (-> f (io/resource) slurp string/split-lines))

(defn parse [moves s]
  (->> (string/split s #" ")
       (map moves)))

(defn score [results [h1 h2]]
  (+ (scores ((h1 results) h2))
     (scores h2)))


(defn part1 []
  (->> (read-file f)
       (map (partial parse moves))
       (map (partial score results))
       (apply +)))

(defn part2 []
  (->> (read-file f)
       (map (partial parse moves2))
       (map (partial score results2))
       (apply +)))
