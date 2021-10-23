(ns e4-palindrome-numbers
  (:require [clojure.string :as str]))

(defn gen-products
  ([x y]
   (lazy-seq
    (cons (* x y) (gen-products y (dec  x))))))

(defn re-find-groups
  [re s]
  (next (re-find re s)))

(def lim 999)

;; (def products (gen-products lim lim)) doesn't generate all

(defn ->halves
  [s] 
  (let [half (-> s count (/ 2) int)
        re-str (str "^(\\d{" half "})\\d??(\\d{" half "})$")
        re (re-pattern re-str)]
    (re-find-groups re s)))

(def products (for [a (range 99 10 -1)
                     b (range 99 (dec a) -1)]
                 [(* a b) a b]))

(defn reverse-second
  [[_ s]]
  [_ (str/reverse s)])

(def xf-palindrome?
  (comp (map str)
        (map ->halves)
        (map reverse-second)
        (map #(apply = %))))


;; (defn -main
;;   []
  ;; (let [reverse-second ]
;;     ))