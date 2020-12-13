(ns adventofcode.2020.day13
  (:require
   [clojure.string :as string]
   [clojure.math.numeric-tower :as math]))

(def input
  (string/split-lines (slurp "resources/2020/day13.txt")))

(def timestamp
  (Integer. (first input)))

(def ids
  (for [s (string/split (second input) #",")
        :when (not= s "x")]
    (Integer. s)))

(->> (drop timestamp (range))
     (keep
      (fn [ts]
        (first
         (for [id ids
               :when (zero? (mod ts id))]
           [id (- ts timestamp)]))))
     first
     (apply *))
;; => 246

(def timestamps
  (string/split (second input) #","))

(def ids-with-offsets
  (keep-indexed
   (fn [i ts]
     (when (not= ts "x")
       [(Integer. ts) (- i)]))
   timestamps))

;; No common factors in ids so can use:
;; Chinese Remainder Theorem (https://youtu.be/zIFehsBHB8o)
;; SOURCE: https://rosettacode.org/wiki/Chinese_remainder_theorem#Clojure

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs."
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese_remainder
  "Main routine to return the chinese remainder"
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)
                        egcd (extended-gcd p n_i)
                        inv_p (second egcd)]
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))]
    (mod sum-prod prod)))


(chinese_remainder (mapv first ids-with-offsets) (mapv second ids-with-offsets))
;; => 939490236001473
