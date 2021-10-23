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
  (let [rang (range 2 (inc 20))
        factors (map e3/factors-of'' rang)
        factors-maps (map count-occurences factors)
        factors-map (reduce (partial merge-with max) factors-maps)]
    (reduce (fn [acc, [n c]]
              (apply * acc (repeat c n)))
            1 factors-map)))
