(ns adventofcode.2019.day05
  (:require
   [adventofcode.2019.intcode :refer [run]]
   [clojure.java.io :as io]))

(def program
  (slurp (io/resource "2019/day05.txt")))

(peek (run program 1))
;; => 16225258

(peek (run program 5))
;; => 2808771
