(ns adventofcode.2019.day09
  (:require [adventofcode.2019.intcode :refer [run]]
            [clojure.java.io :as io]))

(def program
  (slurp (io/resource "2019/day09.txt")))


;; Part 1

(peek (run program 1))

;; => 2436480432


;; Part 2

(peek (run program 2))

;; => 45710
