(ns euler.e14-collatz-sequence)

(defn next-collatz
  [n]
  (if (even? n)
    (/ n 2)
    (inc (* n 3))))

(defn collatz
  [start]
  (take-while #(not= % 1) (iterate next-collatz start)))

(defn -main
  []
  (let [starts (range 1 1000000)
        xf (comp (map collatz)
                 (map #(inc (count %)))
                 (map-indexed vector))
        res-vec (transduce xf (partial max-key second) [0 0] starts)]
    (update res-vec 0 inc)))