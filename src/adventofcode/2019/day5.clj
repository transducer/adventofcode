(ns adventofcode.2019.day5
  (:require [adventofcode.2019.day4 :refer [digits]]
            [clojure.java.io :as io]))

(def input
  (->> (io/resource "2019/day5.txt")
       slurp
       (format "[%s]")
       read-string))

(defn run [program in]
  (loop [p program
         out []
         i 0]
    (let [ns (digits (p i))
          op (last ns)
          [a_imm? b_imm?] (->> (drop-last 2 ns)
                               reverse
                               (map (partial = 1)))
          [a' b' c] (subvec p (inc i))
          a (if a_imm? a' (get p a'))
          b (if b_imm? b' (get p b'))]
      (condp = op
        1 (recur (assoc p c (+ a b)) out (+ i 4))
        2 (recur (assoc p c (* a b)) out (+ i 4))
        3 (recur (assoc p a' in) out (+ i 2))
        4 (recur p (conj out a) (+ i 2))
        5 (recur p out (if (zero? a) (+ i 3) b))
        6 (recur p out (if (zero? a) b (+ i 3)))
        7 (recur (assoc p c (if (< a b) 1 0)) out (+ i 4))
        8 (recur (assoc p c (if (= a b) 1 0)) out (+ i 4))
        9 (peek out)))))


;; Part 1

(run input 1)

;; => 16225258


;; Part 2

(run input 5)

;; => 2808771
