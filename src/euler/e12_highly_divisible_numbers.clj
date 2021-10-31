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

(defn rd->factors-map
  [acc f]
  (update acc f #(if %
                   (inc %)
                   1)))

(defn divisors-count
  [n]
  (let [factors (p/factors-of n)
        factors-map (reduce rd->factors-map {} factors)
        xf (comp (map #(% 1))
                 (map inc))]
    (transduce xf * (seq factors-map))))


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
                                     (map (juxt divisors-count identity))
                                     (filter #(> (% 0) 500)))]
    (transduce xf->divisors-count>500
               (completing (comp reduced #(second %&)))
               nil
               indexed-triangulars)))