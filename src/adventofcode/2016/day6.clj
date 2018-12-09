(ns adventofcode.2016.day6
  (:require [clojure.java.io :as io]))

(def input
  (-> "2016/day6.txt" io/resource io/reader line-seq))

(defn transpose
  [d]
  (apply mapv vector d))


;; Part 1

(->> input
     transpose
     (map frequencies)
     (map #(sort-by second %))
     (map last)
     (map first)
     (apply str))


;; Part 2

(->> input
     transpose
     (map frequencies)
     (map #(sort-by second %))
     (map ffirst)
     (apply str))
