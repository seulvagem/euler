(ns e2-even-values-of-fib)

(defn fib
  ([] (fib 1 1))
  ([a b]
   (lazy-seq
    (cons a (fib b (+ a b))))))

(def limit 4000000)

(defn -main
  []
  (let [fibs (drop 2 (fib))
        xf-even-fibs (take-nth 3)
        xf-lim (take-while #(<= % limit))
        xf (comp xf-even-fibs xf-lim)]
    (transduce xf + fibs)))