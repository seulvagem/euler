(ns e768-combinatorial-analysis)

;; (def !
;;   "factorial"
;;   (memoize
;;    (fn ([n]
;;         (! (long 1) (long n)))
;;      ([acc n]
;;       (let [nacc (* acc n)
;;             nn (dec n)]
;;         (if (> nn 1)
;;           (recur nacc nn)
;;           nacc))))))

(defn next-factorial
  [[n acc]]
  (let [in (inc' n)]
    [in (*' acc in)]))

(def factorials (lazy-seq (cons [0 1] (iterate next-factorial [1 1]))))


(defn !
  [n]
  (second (nth factorials n)))


(defn combinations
  [n s]
  (/ (! n)
     (* (! s) (! (- n s)))))

(defn chandelier
  [h c]
  (combinations (/ h 2) (/ c 2)))
