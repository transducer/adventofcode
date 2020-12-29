(ns adventofcode.2015.day05)

(def strings
  (re-seq #".*\n" (slurp "resources/2015/day05.txt")))

(defn nice? [s]
  (and (>= (count (re-seq #"[aeiou]{1}" s)) 3)
       (re-find #"(.)\1" s)
       (not (re-find #"(ab|cd|pq|xy)" s))))

(count (filter nice? strings))
;; => 238

(defn nice2? [s]
  (and (re-find #"(..).*\1" s)
       (re-find #"(.).\1" s)))

(count (filter nice2? strings))
;; => 69
