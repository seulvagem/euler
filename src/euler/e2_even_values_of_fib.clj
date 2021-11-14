(ns euler.e2-even-values-of-fib)

(defn fib
  ([] (fib 1 1))
  ([a b]
   (lazy-seq
    (cons a (fib b (+ a b))))))

(def limit 4e6)

(defn -main
  []
  (let [fibs (drop 2 (fib))
        xf (comp (take-nth 3)
                 (take-while #(<= % limit)))]
    (transduce xf + fibs)))