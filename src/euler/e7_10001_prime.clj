(ns euler.e7-100010-prime
  (:require [euler.e3-prime-factors :as e3]))

(defn -main
  []
  (nth e3/primes 10000))