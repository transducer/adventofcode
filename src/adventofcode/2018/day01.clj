(ns adventofcode.2018.day01
  (:require
   [clojure.java.io :as io]))

(def input
  (->> (io/resource "2018/day01.txt")
       slurp
       (format "(%s)")
       read-string))

(apply + input)
;; => 402

(def freqs-seen
  (reductions
   +
   0
   (cycle input)))

(->> (distinct freqs-seen)
     (map (fn [a b] (when (not= a b) a)) freqs-seen)
     (remove nil?)
     first)
;; => 481
