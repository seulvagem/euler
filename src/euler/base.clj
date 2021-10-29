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

(defn combinations-of-2
  [coll]
  (loop [acc '()
         sequ coll]
    (let [x (first sequ)
          seq2 (next sequ)]
      (if-not seq2
        acc
        (let [combs (map #(list x %) seq2)
              nacc (into acc combs)]
          (recur nacc seq2))))))

(defn combinate
  [f coll]
  (loop [acc []
         sequ (seq coll)]
    (let [x (first sequ)
          seq2 (next sequ)]
      (if-not seq2
        acc
        (let [combs (map #(cons x %) (f seq2))
              nacc (into acc combs)]
          (recur nacc seq2))))))
