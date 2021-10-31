(ns euler.e21-amicable-numbers
  (:require [euler.base :as b]))

;; Let d (n) be defined as the sum of proper divisors of n 
;;  (numbers less than n which divide evenly into n) .
;; If d (a) = b and d (b) = a, where a ≠ b, then a and b are
;;  an amicable pair and each of a and b are called amicable numbers.

;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11,
;;  20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors
;;  of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

;; Evaluate the sum of all the amicable numbers under 10000.

(def divisors-sum
  (memoize (fn [n]
             (reduce + (b/divisors-of n)))))

(defn get-amicable
  [x]
  (let [y (divisors-sum x)]
    (when (and (not= x y) (= (divisors-sum y) x))
      y)))

(defn -main
  []
  (let [xf (comp (map get-amicable)
                 (filter identity))]
    (transduce xf + (range 1 10000))))

;; (def amicable-cache (atom {}))

;; (defn get-amicable
;;   [x]
;;   (if-let [res (@amicable-cache x)]
;;     res
;;     (let [new-res (get-amicable* x)]
;;       (swap! amicable-cache assoc
;;              x new-res
;;              new-res x)
;;       new-res)))

