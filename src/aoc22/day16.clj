(ns aoc22.day16
  (:require [clojure.string :as string]
            [clojure.set :as set]))

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

(defn max-flow [network-info valve valve-from time-left valves-on]
  (when (< 26 time-left)
    (println "t:" time-left "valve:" valve "valves-on:" valves-on))
  (if (or (= 0 time-left)
          (= valves-on (set (keys network-info))))
    {:pressure-released 0
     :actions           []}
    (let [flow-rate (get-in network-info [valve :rate])]
      (->> (conj

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More efficient attempt

;; prob better to look at nodes with worthwhile valves.  pre-calc shortest path
;; distances between these nodes.  Then work out which order to turn them on
;; which valves are en route to other valves?
;; does turning a valve make sense - if more pressure is released than getting to other valves?

;; TODO - are they valves or nodes - make naming consistent

(defn max-flow2 [network-info shortest-paths destination-nodes valve time-left valves-on]
  (if (or (>= 0 time-left)
          (= valves-on destination-nodes))
    {:pressure-released 0
     :actions           []}
    (->> (conj

          ;; option 1 - turn on valve and then visit a destination node
          (for [valve-next (->> (shortest-paths valve)
                                keys
                                (remove valves-on))]
            (let [ ;; path contains start node, which adds one, but we need to
                  ;; add one anyway to account for turning valve on.
                  time-used (-> (get-in shortest-paths [valve valve-next])
                                count)
                  flow-rate (get-in network-info [valve-next :rate])]
              (-> (max-flow2 network-info shortest-paths destination-nodes
                             valve-next
                             (- time-left time-used)
                             (conj valves-on valve))
                  ;; TODO should really check here that we have enough time before
                  (update :pressure-released #(+ % (* flow-rate (- time-left time-used 1))))
                  (update :actions #(conj % [(- time-left time-used) [:valve-on valve-next]])))))

          ;; option 2 - always return at least this in case there are no valid
          ;; routes out and nothing to do, at least return a blank
          {:pressure-released 0
           :actions           []})
         (sort-by :pressure-released >)
         first)))

;; takes a route - the first item in the route is the one we are visiting.
(defn unvisited-adjoining-nodes [network-info route visited]
  (->> (get-in network-info [(first route) :dests])
       (remove visited)
       (map (fn [next-n] (conj route next-n)))))

;; want to return
;; * list of destination-nodes/routes (distance calc from length of route)
(defn shortest-path [network-info start-n destination-nodes]
  (loop [visited          #{start-n}
         paths-to-explore (unvisited-adjoining-nodes network-info (list start-n) visited)
         results          {}]
    (let [route   (first paths-to-explore)
          n       (first route)]
      (if (or (nil? n)
              (set/subset? destination-nodes visited))
        results
        (if (visited n)
          ;; if already seen, don't add to results, or explore paths, but maybe
          ;; we still need to recur
          (recur visited
                 (rest paths-to-explore)
                 results)
          (recur (conj visited n)
                 (concat (rest paths-to-explore)
                         (unvisited-adjoining-nodes network-info route visited))
                 (if (destination-nodes n)
                   (assoc results n route)
                   results
                   )))))))

(defn my-messing [s]
  (let [network-info      (parse-string s)
        destination-nodes (->> network-info
                               (remove (fn [[k v]] (zero? (:rate v))))
                               (map first))
        shortest-paths (->> (map #(list % (shortest-path network-info % (set destination-nodes))) (conj destination-nodes :AA))
                            (apply concat)
                            (apply hash-map))]
    ;; add extra minute to 'turn on AA'
    ;; TODO - sort the above
    (max-flow2 network-info shortest-paths destination-nodes :AA 31 #{})
    ;;shortest-paths
    ))
