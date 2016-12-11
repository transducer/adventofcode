(ns adventofcode.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "day6.txt"
      io/resource
      io/reader
      line-seq))

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
