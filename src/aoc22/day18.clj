(ns aoc22.day18
  (:require [clojure.string :as string]))

(defn parse-cube [s]
  (->> (string/split s #",")
       (mapv read-string)))

(defn parse-input [s]
  (->> (string/split-lines s)
       (map parse-cube)))

(defn neighbours [d]
  (for [f [inc dec]
        i (range (count d))]
    (update d i f )))

(defn count-exposed-faces [droplets d]
  (->> (neighbours d)
       (remove droplets)
       count))

(defn pt1 [s]
  (let [droplets (parse-input s)]
    (->> droplets
         (map (partial count-exposed-faces (set droplets)))
         (apply +))))

(comment
  " Something seems off about your calculation. The cooling rate depends
on exterior surface area, but your calculation also included the surface area of
air pockets trapped in the lava droplet.

Instead, consider only cube sides that could be reached by the water and steam
as the lava droplet tumbles into the pond. The steam will expand to reach as
much as possible, completely displacing any air on the outside of the lava
droplet but never expanding diagonally.

In the larger example above, exactly one cube of air is trapped within the lava
droplet (at 2,2,5), so the exterior surface area of the lava droplet is 58.

What is the exterior surface area of your scanned lava droplet? "


  "
Thoughts
- Could start at some surface point - eg, lowest x, then lowest y, then lowest z
- for each

- could create a 'water tank' around min/max x/ys and recursively fill water.
- whatever isn't water and isn't rock is an air pocket.
- now count exposed faces of air pocket.


- do we think about faces or cubes?



What if's not a single blob?  There might be multiple blobs?

- check if all the surfaces are accounted for?
--> how would we do this? many cubes would be inside the surface area we are tracking.")
