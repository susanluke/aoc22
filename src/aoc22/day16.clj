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

(defn max-flow [network-info shortest-paths destination-nodes valve time-left valves-on res-acc]
  (if (or (>= 0 time-left)
          (= valves-on destination-nodes))
    res-acc
    (->> (conj
          ;; option 1 - turn on valve and then visit a destination node
          (for [valve-next (->> (shortest-paths valve)
                                keys
                                (remove valves-on))]
            (let [ ;; path contains start node, which adds one, but we need to
                  ;; add one anyway to account for turning valve on.
                  time-used-to-next-node (-> (get-in shortest-paths [valve valve-next])
                                             count
                                             dec)
                  time-used-to-turn-on-valve (if (= :AA valve) 0 1)
                  time-used (+ time-used-to-next-node time-used-to-turn-on-valve)
                  flow-rate (get-in network-info [valve :rate])]
              (max-flow network-info shortest-paths destination-nodes
                        valve-next
                        (- time-left time-used)
                        (conj valves-on valve)
                        (-> res-acc
                            (update :pressure-released #(+ % (* flow-rate (dec time-left))))
                            (update :actions #(conj % [time-left [:valve-on valve]]))))))

          ;; option 2 - turn on valve, but don't do anything else
          (-> res-acc
              (update :pressure-released #(+ % (* (get-in network-info [valve :rate]) (dec time-left))))
              (update :actions #(conj % [time-left [:valve-on valve]]))))
         (sort-by :pressure-released >)
         first)))

(defn unvisited-adjoining-nodes [network-info route visited]
  (->> (get-in network-info [(first route) :dests])
       (remove visited)
       (map (fn [next-n] (conj route next-n)))))


(defn shortest-path
  "Takes:
  network-info - map of the form {:NN {:rate 3 :dests [:OO :PP]
                                  :PP {:rate 5 :dests [:NN :JJ]}
  start-n - starting node
  destination-nodes - all the nodes we want to find the shortest route to

  Returns:
  map of the form {:NN (:NN :AA)
                   :PP (:PP :TT :AA)}
  where the values are the routes to take in reverse order (starting
  point is last and finishing is at the start of the list"
  [network-info start-n destination-nodes]
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
                   results)))))))

(defn pt1 [s]
  (let [network-info      (parse-string s)
        ;; destination nodes are nodes worth going to because they have a valve
        ;; which releases pressure
        destination-nodes (->> network-info
                               (remove (fn [[k v]] (zero? (:rate v))))
                               (map first))
        ;; for each 'destination node', pre-calc the shortest path to each of
        ;; the other 'destination nodes'.  Then we only need to try these routes.
        shortest-paths (->> (map #(list % (shortest-path network-info % (set destination-nodes))) (conj destination-nodes :AA))
                            (apply concat)
                            (apply hash-map))]
    (max-flow network-info shortest-paths destination-nodes :AA 30 #{} {:pressure-released 0
                                                                        :actions           []})))

(defn max-flow2 [network-info shortest-paths destination-nodes time-lefts-and-valves valves-visited valves-on res-acc]
  (let [[time-left valve] (first time-lefts-and-valves)
        rest-time-lefts-and-valves (rest time-lefts-and-valves)]
    (if (or (nil? time-left)
            (>= 0 time-left)
            (= valves-on destination-nodes))
      res-acc
      (->> (conj
            ;; option 1 - turn on valve and then visit a destination node
            (for [valve-next (->> (shortest-paths valve)
                                  keys
                                  (remove valves-visited))]
              (let [ ;; path contains start node, which adds one, but we need to
                    ;; add one anyway to account for turning valve on.
                    route (get-in shortest-paths [valve valve-next])
                    time-used-to-next-node (-> route
                                               count
                                               dec)
                    time-used-to-turn-on-valve (if (= :AA valve) 0 1)
                    time-used (+ time-used-to-next-node time-used-to-turn-on-valve)
                    flow-rate (get-in network-info [valve :rate])]
                (max-flow2 network-info shortest-paths destination-nodes
                           (->> (conj rest-time-lefts-and-valves
                                      [(- time-left time-used) valve-next])
                                (sort-by first >))
                           (conj valves-visited valve valve-next) ;; need to check we can get to valve-next
                           (conj valves-on valve) ;; need to check we can get to valve-next
                           (-> res-acc
                               (update :pressure-released #(+ % (* flow-rate (dec time-left))))
                               (update :actions #(conj % [time-left [:valve-on valve :route-onwards route]]))))))

            ;; option 2 - turn on valve, but don't do anything else recurse
            ;; through as partner might be able to do something.  Need to keep
            ;; recursing until we get to the exit conditions.
            (max-flow2 network-info shortest-paths destination-nodes
                       rest-time-lefts-and-valves
                       valves-visited
                       (conj valves-on valve)
                       (-> res-acc
                           (update :pressure-released #(+ % (* (get-in network-info [valve :rate]) (dec time-left))))
                           (update :actions #(conj % [time-left [:valve-on valve]])))))
           (sort-by :pressure-released >)
           first))))

(defn pt2 [s]
  (let [network-info      (parse-string s)
        ;; destination nodes are nodes worth going to because they have a valve
        ;; which releases pressure
        destination-nodes (->> network-info
                               (remove (fn [[k v]] (zero? (:rate v))))
                               (map first))
        ;; for each 'destination node', pre-calc the shortest path to each of
        ;; the other 'destination nodes'.  Then we only need to try these routes.
        shortest-paths (->> (map #(list % (shortest-path network-info % (set destination-nodes))) (conj destination-nodes :AA))
                            (apply concat)
                            (apply hash-map))]
    (max-flow2 network-info shortest-paths destination-nodes [[26 :AA] [26 :AA]] #{} #{} {:pressure-released 0
                                                                                          :actions           []})))

(comment
  "
 2 The elephant opens valve DD.
 3 You open valve           JJ.
 7 You open valve           BB.
 7 The elephant opens valve HH.
 9  You open valve          CC.
11 The elephant opens valve EE.
"

  ;; no diff between valves visited and on
  (->> (sut/pt2 input)
       :actions
       (map (fn [[t acts]] [(- 27 t) acts])))
  ([1 [:valve-on :AA :route (:DD :AA)]]
   [1 [:valve-on :AA :route (:BB :AA)]]
   [2 [:valve-on :BB :route (:JJ :II :AA :BB)]]
   [2 [:valve-on :DD :route (:HH :GG :FF :EE :DD)]]
   [6 [:valve-on :JJ :route (:CC :DD :AA :II :JJ)]]
   [7 [:valve-on :HH :route (:EE :FF :GG :HH)]]
   [11 [:valve-on :EE]])
  )
