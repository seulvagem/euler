(ns euler.e4-palindrome-numbers
  (:require [clojure.string :as str]))

(def top 999)
(def bottom 100)

(def products (for [a (range top (dec bottom) -1)
                    b (range top (dec a) -1)]
                [(* a b) [a b]]))

(defn palindrome?
  [n]
  (let [str-n (str n)
        rev-n (str/reverse str-n)]
    (= str-n rev-n)))

(defn -main
  []
  (let [xf (filter #(palindrome? (first %)))]
    (transduce xf (partial max-key first) [0] products)))
