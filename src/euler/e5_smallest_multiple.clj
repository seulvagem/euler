(ns euler.e5-smallest-multiple
  (:require [euler.e3-prime-factors :as e3]
            [euler.base :as b]))

(def lim 20)

(defn lcm
  [numbers]
  (let [xf-factors-occurence-map (comp (map e3/factors-of)
                                       (map b/coll->occurrences-map))
        factors-map (transduce xf-factors-occurence-map
                               (partial merge-with max)
                               {} numbers)
        xf-occurences-map->items (mapcat #(apply repeat (reverse %)))]
    (println factors-map)
    (transduce xf-occurences-map->items * factors-map)))

(defn -main
  []
  (let[divisors (range 2 (inc lim))]
   (lcm divisors)))