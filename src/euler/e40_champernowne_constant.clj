(ns euler.e40-champernowne-constant
  (:require [euler.base :as b]))

(defn -main
  []
  (let [nums (range)
        digits (sequence b/x-num->digit nums)
        positions [1e0 1e1 1e2 1e3 1e4 1e5 1e6]
        x-pos->number (comp (map #(nth digits %))
                            (map #(Character/digit % 10)))]
    (transduce x-pos->number * positions)))