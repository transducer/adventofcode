(ns adventofcode.2019.day5
  (:require [adventofcode.2019.day4 :refer [digits]]
            [clojure.java.io :as io]))

(def input
  (->> (io/resource "2019/day5.txt")
       slurp
       (format "[%s]")
       read-string))

(defn run [program in]
  (loop [p [program []]
         i 0]
    (let [[v out :as p] p
          ns (digits (v i))
          op (last ns)
          [a_imm? b_imm?] (->> (drop-last 2 ns)
                               reverse
                               (map (partial = 1)))
          [a' b' c] (subvec v (inc i))
          a (if a_imm? a' (get v a'))
          b (if b_imm? b' (get v b'))]
      (condp = op
        1 (recur [(assoc v c (+ a b)) out] (+ i 4))
        2 (recur [(assoc v c (* a b)) out] (+ i 4))
        3 (recur [(assoc v a' in) out] (+ i 2))
        4 (recur [v (conj out a)] (+ i 2))
        5 (recur p (if (zero? a) (+ i 3) b))
        6 (recur p (if (zero? a) b (+ i 3)))
        7 (recur [(assoc v c (if (< a b) 1 0)) out] (+ i 4))
        8 (recur [(assoc v c (if (= a b) 1 0)) out] (+ i 4))
        9 (peek out)))))


;; Part 1

(run input 1)

;; => 16225258


;; Part 2

(run input 5)

;; => 2808771
