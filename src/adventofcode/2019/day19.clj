(ns adventofcode.2019.day19
  (:require [adventofcode.2019.intcode :refer [run]]
            [clojure.java.io :as io]))

(def program
  (slurp (io/resource "2019/day19.txt")))


;; Part 1

(->> (for [x (range 50)
           y (range 50)]
       (peek (run program x y)))
     (reduce +))

;; => 141
