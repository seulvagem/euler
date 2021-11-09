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

(defn next-n
  ([]
   {:step 2 :n 1 :count 3})
  ([{:keys [step count n] :as m}]
     ;; lots of duplication
   (let [n (+ n step)
         step (if (zero? count)
                (+ step 2)
                step)
         count (if (zero? count)
                 3
                 (dec count))]
     {:n n, :step step, :count count})))

;; should probably take this into transducer right?
(def n-seq (iterate next-n (next-n)))

(defn -main
  [square-size] ;; ugly xor caused by execution and checking order
  (let [xf (comp (take-while #(not (and (> (% :step) square-size)
                                        (< (% :count) 3))))
                 (map :n))]
    (transduce xf + n-seq)))