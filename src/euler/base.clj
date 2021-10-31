(ns euler.base)

(defn get-parse-fn
  [length]
  (condp > length 
   10 #(Integer/parseInt %)
   19 #(Long/parseLong %)
   ##Inf bigint))

(defn parse-num
  [num-str]
  (let [parse-fn (get-parse-fn (count num-str))]
    (parse-fn num-str)))

(def x-num->digit
  "number to digit sequence transducer"
  (comp (map str)
        (mapcat seq)))


;; combinations

(defn- combinations-of-1
  "kinda stupid, but it keeps combinate DRY!"
  [coll]
  (map list coll))

(defn- combinate
  [f]
  (fn [coll]
    (loop [acc []
           seq1 (seq coll)]
      (let [x (first seq1)
            seq2 (next seq1)]
        (if-not seq2
          acc
          (let [nacc (into acc (map #(cons x %)) (f seq2))]
            (recur nacc seq2)))))))

(defn- get-combination-fn
  [n]
  (nth (iterate combinate combinations-of-1) (dec n)))

(defn combinations-of
  [n coll]
  (let [c-fn (get-combination-fn n)]
    (c-fn coll)))

(defn combinations-up-to
  [n coll]
  (let [xf-combs-of (map #(combinations-of % coll))]
    (transduce xf-combs-of concat (range 1 (inc n)))))


;; pass-value-through

(defn use-i
  ([f]
   (use-i f 0))
  ([f n]
   (fn [arg-vec]
     (update arg-vec n f))))
