(ns euler.prime-sum
  (:require [euler.e3-prime-factors :as e3]
            [clojure.math.numeric-tower :as m]
            [clojure.data.int-map :as i]))


(def limit 200000000)

;; works, but sieve is better in this case (since we know the upper bound), shoud try to implement it next

(defn -main'1
  []
  (transduce (take-while #(< % limit)) + e3/primes))


;; sieve method

(def possible-primes (cons 2 (range 3 limit 2)))

(defn remove-multiples-of
  [limit nums n] 
    ;;start can be the square since lower multiples would've been crosset out by previous primes 
    ;;step can be two because we are skipping evens
  (let [multiples (range (* n n) limit (* n 2))]
    (reduce disj nums multiples))) 

(defn next-number
  [num-set n]
  (let [n+ (inc n)
        num (num-set n+)]
    (if num
      num
      (recur num-set n+))))

(defn primes-up-to'1
  "obsolete"
  [limit]
  (let [lim (m/sqrt limit)]
    (loop [numbers (into (sorted-set) possible-primes)
           i 0]
      (let [prime (nth (seq numbers) i)]
        (if (> prime lim)
          numbers
          (recur (remove-multiples-of limit numbers prime) (inc i)))))))

(defn primes-up-to'2
  "obsolete"
  [limit]
  (let [lim (m/sqrt limit)]
    (loop [numbers (into #{} possible-primes)
           prime 2]
      (if (> prime lim)
        numbers
        (recur (remove-multiples-of limit numbers prime) (next-number numbers prime))))))

(defn primes-up-to
  "generates primes up to given limit, utilizes a sieve in order to do so"
  [limit]
  (let [lim (m/sqrt limit)]
    (loop [numbers (into (i/dense-int-set) possible-primes)
           prime 2]
      (if (> prime lim)
        numbers
        (recur (remove-multiples-of limit numbers prime) (next-number numbers prime))))))

(defn -main
  []
  (let [primes (primes-up-to limit)]
    (reduce + primes)))