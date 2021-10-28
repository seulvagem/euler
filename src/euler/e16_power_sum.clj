(ns euler.e16-power-sum
  (:require [clojure.math.numeric-tower :as m]
            [euler.base :as b]))

(defn -main
  []
  (let [number (m/expt 2 1000)
        xf (comp b/x-num->digit
                 (map #(Character/digit % 10)))]
    (transduce xf + [number])))