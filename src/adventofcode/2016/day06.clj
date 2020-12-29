(ns adventofcode.2016.day06
  (:require
   [clojure.java.io :as io]))

(def input
  (-> "2016/day06.txt" io/resource io/reader line-seq))

(defn transpose
  [d]
  (apply mapv vector d))

(->> input
     transpose
     (map frequencies)
     (map #(sort-by second %))
     (map last)
     (map first)
     (apply str))
;; => "bjosfbce"

(->> input
     transpose
     (map frequencies)
     (map #(sort-by second %))
     (map ffirst)
     (apply str))
;; => "veqfxzfx"
