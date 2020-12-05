(ns adventofcode.2020.day5
  (:require
   [clojure.string :as string]))

(def input
  (string/split-lines (slurp "resources/2020/day5.txt")))

(defn binary-slice [pred high chars]
  (loop [v (vec (range high))
         [c & _ :as cs] chars]
    (if c
      (recur
       (if (pred c)
         (subvec v 0 (Math/ceil (/ (count v) 2)))
         (subvec v (/ (count v) 2) (count v)))
       (next cs))
      (first v))))

(defn row [chars]
  (binary-slice #{\F} 128 chars))

(defn column [chars]
  (binary-slice #{\L} 8 chars))

(defn seat-id [boarding-pass]
  (+ (* 8 (row (take 7 boarding-pass)))
     (column (drop 7 boarding-pass))))

(def seat-ids
  (map seat-id input))


;; Part 1

(apply max seat-ids)

;; => 911


;; Part 2

(->> seat-ids
     sort
     (partition 2 1)
     (filter (fn [[a b]] (not= (inc a) b)))
     ffirst
     inc)

;; => 629
