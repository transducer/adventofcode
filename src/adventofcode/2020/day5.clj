(ns adventofcode.2020.day5
  (:require
   [clojure.string :as string]))

(def input
  (string/split-lines (slurp "resources/2020/day5.txt")))

(defn binary-slice [chars]
  (-> (reduce (fn [acc c] (str acc (if (#{\F \L} c) 0 1))) "" chars)
      (Integer/parseInt 2)))

(defn seat-id [boarding-pass]
  (+ (* 8 (binary-slice (take 7 boarding-pass)))
     (binary-slice (drop 7 boarding-pass))))

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
