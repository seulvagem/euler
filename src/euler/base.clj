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



;; QUEUE
(defn queue
  ([]
   (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (into (queue) coll)))

(defn queue?
  [x]
  (= (type x) clojure.lang.PersistentQueue))



;; overwrite default printer for queues
(defmethod print-method clojure.lang.PersistentQueue [q, w] ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))