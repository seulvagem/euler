(ns euler.e5-smallest-multiple
  (:require [euler.e3-prime-factors :as e3]
            [euler.base :as b]))

(def lim 20)

(defn -main
  []
  (let [divisors (range 2 (inc lim))
        xf-factors-occurences (comp (map e3/factors-of)
                                    (map b/coll->occurrences-map))
        factors-map (transduce xf-factors-occurences
                               (partial merge-with max)
                               {} divisors)]
    (println factors-map)
    (reduce (fn [acc, [n c]]
              (reduce * acc (repeat c n)))
            1 factors-map)))
