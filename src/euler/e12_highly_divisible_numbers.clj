(ns euler.e12-highly-divisible-numbers
  (:require [euler.base :as b]
            [euler.e3-prime-factors :as p]))

(defn all-combinations
  [coll]
  (b/combinations-up-to (dec (count coll)) coll))

(defn divisors-of
  [n]
  (let [factors (p/factors-of n)
        combs (all-combinations factors)
        xf (map #(reduce * %))
        divs (into #{} xf combs)]
    (conj divs 1 n)))

;; (def triangle-numbers)

(defn -main
  []
  )

