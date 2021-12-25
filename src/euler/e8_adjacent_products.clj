(ns euler.e8-adjacent-products
  (:require [euler.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clj-queue.core :as q]))

(def input (str/replace "73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"
                        #"\R" ""))

(defn parse-int
  "char -> int"
  [c]
  (Character/digit c 10))

(def section-length 13)

(def numbers (sequence (map #(parse-int %)) input))

;; (defn -main'1
;;   []
;;   (let [starts (range (- (count input) section-length))
;;         xf-start->product (comp (map #(drop % numbers))
;;                                 (map #(take section-length %))
;;                                 (filter #(not (some zero? %)))
;;                                 (map #(reduce * %)))]
;;     (transduce xf-start->product max 0 starts)))

;; queue solution

(defn transduce-numbers
  [reducing-fn]
  (transduce (map #(parse-int %)) reducing-fn input))

;; (defn prod-queue
;;   ([{:keys [max-prod queue prod] :as acc}, num]
;;    (if (zero? num)
;;      (assoc acc :queue (q/ueue), :prod 1)
;;      (let [nq (conj queue num)
;;            nprod (* prod num)
;;            q-size (count nq)]
;;        (if (<= q-size section-length)
;;          (let [acc (assoc acc :queue nq, :prod nprod)]
;;            (if (= q-size section-length)
;;              (assoc acc :max-prod (max max-prod nprod))
;;              acc))
;;          (let [discarded-num (peek nq)
;;                prod (/ nprod discarded-num)
;;                nmax (max max-prod prod)]
;;            (assoc acc :max-prod nmax, :queue (pop nq), :prod prod))))))
;;   ([{max :max-prod}]
;;    max)
;;   ([]
;;    {:max-prod 0
;;     :queue (q/ueue)
;;     :prod 1}))

;; (defn -main'2
;;   []
;;   (transduce-numbers prod-queue))



(s/def ::full? boolean?)
(s/def ::product (s/and integer? #(not (neg? %))))
(s/def ::max-product ::product)
(s/def ::worm (s/keys :req-un [::full? :clj-queue.core/queue ::product ::max-product]))

(defn clear
  "worm ate something bad, time to empty the bowels!"
  [worm]
  (assoc worm
         :full? false
         :queue (q/ueue)
         :product 1))

(defn ingest
  "yummy number"
  [worm n]
  (-> worm
      (update :queue conj n)
      (update :product * n)))

(defn eat
  "ingest and ponder if it's enough already"
  [worm n]
  (let [n-worm (ingest worm n)
        worm-size (count (:queue n-worm))]
    (assoc n-worm
           :full? (>= worm-size section-length))))

(defn dispose
  "gets rid of waste number"
  [{q :queue,, :as worm}]
  (let [discarded (peek q)]
    (-> worm
        (update :queue pop)
        (update :product / discarded))))

(defn walk
  "sometimes you gotta burn those extra calories after eating
   you know, don't want our worm to get fat do we?"
  [worm n]
  (-> worm
      (ingest n)
      (dispose)))

(def worm-actions
  {false eat
   true walk})

(defn act
  "depending on the fullyness of the worm, either walk or eat"
  [worm n]
  (let [action (worm-actions (:full? worm))]
    (action worm n)))

(defn siesta
  "takes a moment after eating to reflect...
   and calculate the max product of course!"
  [{product :product,, :as worm}]
  (update worm :max-product max product))

(defn product-worm
  "does everything a product-worm should, ingest numbers up to it's max size
   and calculate products, while also keeping track of the max product!"
  ([worm n]
   (if (zero? n)
     (clear worm)
     (let [worm (act worm n)]
       (if (:full? worm)
         (siesta worm)
         worm))))
  ([worm]
   (:max-product worm))
  ([]
   {:full? false
    :queue (q/ueue)
    :product 1
    :max-product 0}))

(defn -main
  []
  (transduce-numbers product-worm))
