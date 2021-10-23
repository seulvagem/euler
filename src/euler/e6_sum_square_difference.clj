(ns euler.e6-sum-square-difference)

(defn -main
  []
  (let [nums (range 1 101)
        square #(* % %)
        squared-sum (transduce (map square) + nums)
        sum-squared (square (reduce + nums))]
    (- sum-squared squared-sum)))