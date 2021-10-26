(ns euler.e8-adjacent-products
  (:require [euler.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

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

(defn -main
  []
  (let [starts (range (- (count input) section-length))
        zero-c? #(= % \0)
        xf-start->product (comp
                           (map #(subs input % (+ % section-length)))
                           (map seq)
                            ;;   (map #(drop % numbers))
                            ;;   (map #(take section-length %))
                           (filter #(not (some zero-c? %)))
                           (map #(map parse-int %))
                           (map #(reduce * %)))]
    (transduce xf-start->product max 0 starts)))

;; queue solution

(s/def ::state #{:growing :full})

(s/def ::worm-state (s/keys :req-un [::max-product ::state ::queue ::product]))

(defn product-worm
  ([worm-state n])
  ([worm-state])
  ([]))

(transduce (map #(parse-int %)) product-worm input)


(defn -main'
  []
  (loop [max-prod 0
         numbers numbers
         q (b/queue)
         c-prod 1]
    (let [num (first numbers)
          nnumbers (rest numbers)]
      (if-not num
        ;;   (do (println "end q:" q)
        max-prod
        (if (zero? num)
            ;; (do (println "found zero q:" q "num:" num)
          (recur max-prod nnumbers (b/queue) 1)
          (let [nq (conj q num)
                nprod (* c-prod num)]
            (if (<= (count nq) section-length)
                ;; (do (println "growing q:" nq "num: "num)
              (recur max-prod nnumbers nq nprod)
              (let [discarded-num (peek nq)
                    prod (/ nprod discarded-num)
                    nmax (max max-prod prod)]
                ;;   (println "max size q:" nq "num:" num "disc-num:" discarded-num)
                (recur nmax nnumbers (pop nq) prod)))))))))
