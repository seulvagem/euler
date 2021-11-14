(ns euler.e9-pythagorean-triplet
  (:require [clojure.math.numeric-tower :as m]
            [euler.base :as b]))

(defn ²
  [x]
  (* x x))

(defn pythag-triplet?
  [[a b c]]
  (= (² c) (+ (² a) (² b))))

(defn euclid
  [m n]
  (let [m² (² m)
        n² (² n)
        a (- m² n²)
        b (* 2 m n)
        c (+ m² n²)]
    [a b c]))

(defn -main
  []
  (let [triplets (for [m (range 31 0 -1)
                       n (range (dec m) 0 -1)]
                   (euclid m n))
        sum=1000-triplet (b/find-linear #(= (apply + %) 1000) triplets)]
    (apply * sum=1000-triplet)))