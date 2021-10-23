(ns euler.e3-prime-factors
  (:require [clojure.math.numeric-tower :as m]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            ))


;; (defn divides-evenly
;;   [a b]
;;   (zero? (rem a b)))

;; (defn prime?
;;   [n]
;;   (let [half (-> n (/ 2) m/floor inc)
;;         possible-divisors (range 2 half)
;;         divides-n-evenly #(divides-evenly n %)]
;;     (not (some divides-n-evenly possible-divisors))))

(def possible-primes (cons 2 (range 3 ##Inf 2)))

(defn prime?
  [n]
  (loop [lim n
         facs possible-primes]
    (let [d (first facs)]
      (if (>= d lim)
        true
        (if (zero? (rem n d))
          false
          (recur (-> n (/ d) long) (rest facs)))))))

(def primes (filter prime? possible-primes))

(s/def ::prime (s/and pos-int? odd? prime?))

(def s-factors-of
  (s/fspec
   :args (s/cat :input-number (s/and pos-int? #(> % 1)))
   :ret (s/every ::prime :kind vector?)
   :fn (fn [{{input :input-number} :args, ret :ret}]
         (= (apply * ret) input))))

(defmacro sdef-many
  [spec & ops]
  `(do ~@(map #(list 'declare %) ops)
       ~@(map #(list 's/def % spec) ops)))

#_:clj-kondo/ignore
(sdef-many s-factors-of 
           prime-factors-of factors-of factors-of' factors-of'' factors-of'w)

(defn prime-factors-of ;; ~41µs, porém a primeira execução leva ms
  [n]
  (loop [facs []
         n n
         primes primes]
    (if (= n 1)
      facs
      (let [p (first primes)
            r (rem n p)]
        (if (zero? r)
          (recur (conj facs p) (/ n p) primes)
          (recur facs n (rest primes)))))))

(defn factors-of ;; ~117µs
  [n]
  (loop [facs []
         n n
         possible-factors possible-primes]
    (if (= n 1)
      facs
      (let [d (first possible-factors)]
        (if (zero? (rem n d))
          (recur (conj facs d) (/ n d) possible-factors)
          (recur facs n (rest possible-factors)))))))

(defn factors-of' ;; ~35µs
  [n]
  (loop [facs []
         n n
         lim n
         possible-factors possible-primes]
    (let [d (first possible-factors)]
      (if (> d lim)
        (conj facs (long n))
        (let [nd-quo (Math/floor (/ n d))]
          (if (zero? (rem n d))
            (recur (conj facs d) nd-quo nd-quo possible-factors)
            (recur facs n nd-quo (rest possible-factors))))))))

(defn factors-of'' ;; ~24µs
  [n]
  (loop [facs []
         n n
         possible-factors possible-primes]
    (let [d (first possible-factors)
          nd-quo (Math/floor (/ n d))]
      (if (> d nd-quo)
        (conj facs (long n))
        (if (zero? (rem n d))
          (recur (conj facs d) nd-quo possible-factors)
          (recur facs n (rest possible-factors)))))))

(defn factors-of'w ;; ~27µs
  [n]
  (loop [facs []
         n n
         possible-factors possible-primes]
    (let [d (first possible-factors)
          n-poss-facs (rest possible-factors)]
      (if (> (* d d) n)  ;; li na wiki, mas é praticamente a mesma coisa de dividir
        (conj facs n)
        (if (zero? (rem n d))
          (recur (conj facs d) (/ n d) n-poss-facs)
          (recur facs n n-poss-facs))))))

(def input 600851475143)

(defn -main
 []
 (let [factors (factors-of'' input)]
   [(peek factors) factors]))

