(ns adventofcode.2019.day7
  (:require [adventofcode.2019.intcode :refer [run run-async]]
            [clojure.core.async :as async :refer [chan <!! put! close!]]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :refer [permutations]]))

(def program
  (slurp (io/resource "2019/day7.txt")))


;; Part 1

(->> (permutations (range 5))
     (map (partial reduce
                   (fn [out i]
                     (peek (run program i out)))
                   0))
     (apply max))

;; => 38834


;; Part 2

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
