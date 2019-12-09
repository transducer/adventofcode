(ns adventofcode.2019.day9
  (:require [adventofcode.2019.day4 :refer [digits]]
            [clojure.java.io :as io]))

(def program
  (->> (io/resource "2019/day9.txt")
       slurp
       (format "[%s]")
       read-string
       (map-indexed (fn [i v] [i v]))
       (into {})))

(defn read-param [p mode base value]
  (condp = mode
    1 value
    2 (get p (+ base value) 0)
    (get p value 0)))

(defn write-param [mode base value]
  (if (= mode 2)
    (+ base value)
    value))

(defn run [program & inputs]
  (loop [p program
         out []
         [x & xs :as in] inputs
         base 0
         i 0]
    (let [ins (digits (get p i))
          op (take-last 2 (cons 0 ins))
          [a_mode b_mode c_mode] (reverse (drop-last 2 ins))
          a' (get p (inc i))
          b' (get p (+ i 2))
          c' (get p (+ i 3))
          a (read-param p a_mode base a')
          a_write (write-param a_mode base a')
          b (read-param p b_mode base b')
          c (write-param c_mode base c')]
      (condp = op
        [0 1] (recur (assoc p c (+ a b)) out in base (+ i 4))
        [0 2] (recur (assoc p c (* a b)) out in base (+ i 4))
        [0 3] (recur (assoc p a_write x) out xs base (+ i 2))
        [0 4] (recur p (conj out a) in base (+ i 2))
        [0 5] (recur p out in base (if (zero? a) (+ i 3) b))
        [0 6] (recur p out in base (if (zero? a) b (+ i 3)))
        [0 7] (recur (assoc p c (if (< a b) 1 0)) out in base (+ i 4))
        [0 8] (recur (assoc p c (if (= a b) 1 0)) out in base (+ i 4))
        [0 9] (recur p out in (+ base a) (+ i 2))
        [9 9] (peek out)))))


;; Part 1

(run program 1)

;; => 2436480432


;; Part 2

(run program 2)

;; => 45710
