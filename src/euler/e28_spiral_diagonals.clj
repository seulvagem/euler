(ns euler.e28-spiral-diagonals)

;; Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
;; (21) 22 23  24 (25)
;; 20  (7)  8  (9) 10
;; 19   6  (1)  2  11
;; 18  (5)  4  (3) 12
;; (17) 16 15  14 (13)
;; It can be verified that the sum of the numbers on the diagonals is 101.
;; What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

;; 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
;; x   x   x   x   x          x           x           x
;; 0 +2 +2 +2 +2 +4 +4 +4 +4 +6 +6 +6 +6 +8 +8 +8 +8
;; 1x1   3x3   5x5   7x7


(defn next-layer
  [step n]
  (let [xf (comp (map #(* % step))
                 (map #(+ % n)))]
    (into '() xf (range 1 5))))

(defn rdcf-layering-sum
  ([]
   [1 1])
  ([[sum n] step]
   (let [layer (next-layer step n)
         n (first layer)
         sum (reduce + sum layer)]
     [sum n]))
  ([[sum]]
   sum))

(defn get-square-diagonals-sum
  [square-size]
  (let [steps (range 2 ##Inf 2)
        max-layers (-> square-size dec (/ 2))
        xf (take max-layers)]
    (transduce xf rdcf-layering-sum steps)))


(def square-size 1001)

(defn -main
  []
  (get-square-diagonals-sum square-size))

;; (defn next-layer'
;;   ([]
;;    ['(1) 2])
;;   ([[last-layer step]]
;;    (let [n (first last-layer)
;;          xf (comp (map #(* % step))
;;                   (map #(+ % n)))
;;          layer (into '() xf (range 1 5))
;;          n-step (+ step 2)]
;;      [layer n-step])))

;; (defn get-square-diagonals-sum'
;;   [square-size]
;;   (let [max-layers (-> square-size (/ 2))
;;         layers (iterate next-layer' (next-layer'))
;;         xf (comp (take max-layers)
;;                  (mapcat first))]
;;     (transduce xf + layers)))