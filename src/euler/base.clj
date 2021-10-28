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