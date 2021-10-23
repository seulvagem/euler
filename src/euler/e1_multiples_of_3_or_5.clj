(ns euler.e1-multiples-of-3-or-5)

(defn -main
  []
  (let [multiples-of-3 (range 3 1000 3)
        multiples-of-5 (range 5 1000 5)
        multiples-of-3-or-5 (set (concat multiples-of-3
                                         multiples-of-5))]
    (reduce + multiples-of-3-or-5)))
