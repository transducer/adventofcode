(ns adventofcode.2019.day7
  (:require [adventofcode.2019.day4 :refer [digits]]
            [adventofcode.2019.day5 :refer [run]]
            [clojure.core.async :as async :refer [chan go-loop <! >! <!! put! close!]]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :refer [permutations]]))

(def program
  (->> (io/resource "2019/day7.txt")
       (slurp)
       (format "[%s]")
       (read-string)))


;; Part 1

(->> (permutations (range 5))
     (map (partial reduce
                   (fn [out i]
                     (run program i out))
                   0))
     (apply max))

;; => 38834


;; Part 2

(defn run-async [program in out]
  (go-loop [p program
            i 0]
    (let [ins (digits (p i))
          op (last ins)
          [a_imm? b_imm?] (->> (drop-last 2 ins)
                               reverse
                               (map (partial = 1)))
          [a' b' c] (subvec p (inc i))
          a (if a_imm? a' (get p a'))
          b (if b_imm? b' (get p b'))]
      (condp = op
        1 (recur (assoc p c (+ a b)) (+ i 4))
        2 (recur (assoc p c (* a b)) (+ i 4))
        3 (recur (assoc p a' (<! in)) (+ i 2))
        4 (do (>! out a) (recur p (+ i 2)))
        5 (recur p (if (zero? a) (+ i 3) b))
        6 (recur p (if (zero? a) b (+ i 3)))
        7 (recur (assoc p c (if (< a b) 1 0)) (+ i 4))
        8 (recur (assoc p c (if (= a b) 1 0)) (+ i 4))
        9 :exit))))

(defn amplifiers [phase_a phase_b phase_c phase_d phase_e]
  (let [[a_out b_out c_out d_out e_out] (repeatedly chan)]
    (put! e_out phase_a)
    (put! e_out 0)
    (put! a_out phase_b)
    (put! b_out phase_c)
    (put! c_out phase_d)
    (put! d_out phase_e)
    (run-async program e_out a_out)
    (run-async program a_out b_out)
    (run-async program b_out c_out)
    (run-async program c_out d_out)
    (run-async program d_out e_out)
    e_out))

(def phase-setting-seqs (permutations (range 5 10)))
(def buf-size (count phase-setting-seqs))

(def outputs
  (async/merge
   (doall
    (for [s phase-setting-seqs]
      (apply amplifiers s)))
   buf-size))

(<!! (async/timeout 100))
(close! outputs)
(apply max (<!! (async/into [] outputs)))

;; => 69113332
