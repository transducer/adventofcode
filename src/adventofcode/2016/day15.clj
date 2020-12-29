(ns adventofcode.2016.day15
  (:require
   [clojure.java.io :as io]))

(def input (-> "2016/day15.txt" io/resource io/reader line-seq))

(defn parse-ints [c]
  (map #(Integer/parseInt %) c))

(defn parse [d]
  (mapv #(-> (re-matches #".*#\d+ has (\d+).*position (\d+)." %)
             rest
             parse-ints) d))

(defn goes-through? [d t]
  (every? zero? (map-indexed #(mod (+ (inc t) %1 (second %2)) (first %2)) d)))

(def discs (parse input))
(def time-range (range))

(defn true-indices [bools]
  (keep-indexed #(when %2 %1) bools))

(->> time-range
     (map (partial goes-through? discs))
     true-indices
     first)
;; => 376777

(def new-disc
  '(11 0))

(->> time-range
     (map (partial goes-through? (conj discs new-disc)))
     true-indices
     first)
;; => 3903937
