(ns adventofcode.2019.day01
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "2019/day01.txt")
       slurp
       (format "(%s)")
       read-string))

(defn fuel [m]
  (- (int (/ m 3)) 2))


;; Part 1

(transduce (map fuel) + input)

;; => 3349352


;; Part 2

(defn fuel* [m]
  (->> (iterate fuel m)
       (rest)
       (take-while pos?)
       (reduce +)))

(transduce (map fuel*) + input)

;; =>  5021154
