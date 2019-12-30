(ns adventofcode.2019.day5
  (:require [adventofcode.2019.intcode :refer [run]]
            [clojure.java.io :as io]))

(def program
  (slurp (io/resource "2019/day5.txt")))


;; Part 1

(peek (run program 1))

;; => 16225258


;; Part 2

(peek (run program 5))

;; => 2808771
