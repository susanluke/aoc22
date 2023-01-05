(ns aoc22.day16
  (:require [clojure.string :as string]))

(defn parse-dests [s]
  (->> (string/split s #", ")
       (mapv keyword)))

(defn parse-line [s]
  (let [[valve rate dests] (rest (re-matches #"^Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)" s))]
    [(keyword valve) {:rate  (read-string rate)
                      :dests (parse-dests dests)}]))

(defn parse-string [s]
  (->> (string/split-lines s)
       (map parse-line)
       (apply concat)
       (apply hash-map)))

(comment
  " what if we want to go part way, then return (to do something else) and go
  further down the branches to release other far away valves?

It's not just, max released - it's also max & time remaining? -> No because we can explore going back and doing other things with our 30 minutes

REM: need to stop and wait when there are no closed valves
")
(defn max-flow
  "Input:
  * network-info
  * valve-to
  * valve-from
  * time-left
  * valves-on

  Output:
  * total pressure released
  * list of time/action(move or valve-on) pairs"
  [network-info valve valve-from time-left valves-on]
  (when (< 26 time-left)
    (println "t:" time-left "valve:" valve "valves-on:" valves-on))
  (if (or (= 0 time-left)
          (= valves-on (set (keys network-info))))
    {:pressure-released 0
     :actions           []}
    (let [flow-rate (get-in network-info [valve :rate])]
      (->> (conj

            ;; TODO - could avoid backtracking more than 1 node if we haven't turned a valve on
            ;; option 1 - try each route onwards, but unless we've turned this valve on, don't head straight back
            (for [valve-next (->> (get-in network-info [valve :dests])
                                  (remove #(= valve-from %) ))]
              (-> (max-flow network-info valve-next valve (dec time-left) valves-on)
                  (update :actions #(conj % [time-left [:travel valve-next]]))))

            ;; option 2 - turn on valve
            (if (and
                 ;; check valve might be worth turning on
                 (< 0 flow-rate)
                 ;; not already turned on
                 (not (valves-on valve)))
              ;; now we've done something, we can consider going back, so nil out 'valve-from'
              (-> (max-flow network-info valve nil (dec time-left) (conj valves-on valve))
                  (update :pressure-released #(+ % (* flow-rate (dec time-left))))
                  (update :actions #(conj % [time-left [:valve-on valve]]))))

            ;; option 3 - if there are no valid routes out and nothing to do, at least return a blank
            {:pressure-released 0
             :actions           []})
           (remove nil?)
           (sort-by :pressure-released >)
           first))))



(defn pt1 [s]
  (max-flow (parse-string s) :AA nil 30 #{}))

(comment
  "need to think about how to calculate pressure released

* as we go along, each day?
* or by time remaining? <- I think we can do this and accumulate

- what if there are two routes which get to the same state?  Do we need to memoize, or not take same route twice?
")
