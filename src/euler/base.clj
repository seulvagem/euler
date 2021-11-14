(ns euler.base
  (:require [euler.e3-prime-factors :as p]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(defn find-linear
  [pred coll]
  (let [pred-ret (fn [x]
               (when (pred x)
                 x))]
    (some pred-ret coll)))

(defn get-parse-fn
  [length]
  (condp > length 
   10 #(Integer/parseInt %)
   19 #(Long/parseLong %)
   ##Inf bigint))

(defn parse-num
  [num-str]
  (let [parse-fn (get-parse-fn (count num-str))]
    (parse-fn num-str)))

(def x-num->digit
  "number to digit sequence transducer"
  (comp (map str)
        (mapcat seq)))


;; combinations

(defn- combinations-of-1
  "kinda stupid, but it keeps combinate DRY!"
  [coll]
  (map hash-set coll))

(defn- combinate
  [f]
  (fn [coll]
    (loop [acc #{}
           seq1 (seq coll)]
      (let [x (first seq1)
            seq2 (next seq1)]
        (if-not seq2
          acc
          (let [xf (map #(conj % x))
                nacc (into acc xf (f seq2))]
            (recur nacc seq2)))))))

(defn- get-combination-fn
  [n]
  (nth (iterate combinate combinations-of-1) (dec n)))

(defn combinations-of
  [n coll]
  (let [c-fn (get-combination-fn n)]
    (c-fn coll)))

(defn combinations-up-to
  [n coll]
  (let [xf-combs-of (map #(combinations-of % coll))]
    (transduce xf-combs-of set/union (range 1 (inc n)))))

(defn all-combinations
  [coll]
  (combinations-up-to (dec (count coll)) coll))


;; divisors

(defn divisors-of
  [n]
  (let [factors (p/factors-of n)
        combs (all-combinations factors)
        xf (map #(reduce * %))
        divs (into #{} xf combs)]
    (conj divs 1)))

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


;; pass-value-through
(defn use-i
  ([f]
   (use-i f 0))
  ([f n]
   (fn [arg-vec]
     (update arg-vec n f))))