(ns adventofcode.2018.day1
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "2019/day1.txt")
       slurp
       (format "(%s)")
       read-string))

(defn fuel [m]
  (- (int (/ m 3)) 2))


;; Part 1

(transduce (map fuel) + input)


;; Part 2

(defn fuel* [total m]
  (if (pos? m)
    (let [new (max 0 (fuel m))]
      (recur (+ total new) new))
    total))

(transduce (map (partial fuel* 0)) + input)
