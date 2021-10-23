(ns euler.e5-smallest-multiple
  (:require [euler.e3-prime-factors :as e3]))


(defn count-occurences
  [coll]
  (reduce (fn [acc x]
            (update acc x #(if %
                             (inc %)
                             1)))
          {} coll))

(defn -main
  []
  (let [divisors (range 2 (inc 20))
        xf-factors-occurences (comp (map e3/factors-of)
                                    (map count-occurences))
        factors-map (transduce xf-factors-occurences
                               (partial merge-with max)
                               {} divisors)]
    (reduce (fn [acc, [n c]]
              (apply * acc (repeat c n)))
            1 factors-map)))
