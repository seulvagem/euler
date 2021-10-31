(ns euler.e12-highly-divisible-numbers
  (:require [euler.base :as b]))

(defn next-triangle-number
  [[acc n]]
  (let [nn (inc n)]
    [(+ acc nn) nn]))

(defn -main
  []
  (let [indexed-triangulars (iterate next-triangle-number [1 1])
        xf->divisors-count>500 (comp (map #(% 0))
                                    ;;  (map (juxt divisors-of identity))
                                    ;;  (map (b/use-i count))
                                     (map (juxt b/divisors-count identity))
                                     (filter #(> (% 0) 500)))]
    (transduce xf->divisors-count>500
               (completing (comp reduced #(second %&)))
               nil
               indexed-triangulars)))