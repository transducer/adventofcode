(ns adventofcode.2016.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> "day20.txt" io/resource io/reader line-seq))

(defn parse-ints [d]
  (map #(Long/parseLong %) d))

(defn parse [d]
  (map parse-ints (map #(str/split % #"-") d)))


;; Part 1

(let [d (sort-by first (parse input))]
  (loop [sorted        d
         [start1 end1] (first d)
         high-end      end1
         [start2 end2] (second d)]
    ;; If start1 is at least two bigger than highest, we have found the smallest
    ;; IP, and we can return it.
    (if (> (dec start1) high-end)
      (dec start1)
      ;; Else, find next range to check, by iterating till we find a new start
      ;; that is bigger than end1
      (recur (next sorted)
             (first (next sorted))
             ;; We will check the ends in the ranges, if they are higher than
             ;; end1, we update end1 with the new higher value
             (max end1 high-end)
             (second (next sorted))))))

;; => 4793564


; Part 2

(->> (parse input)
     (sort-by first)
     (reduce (fn [[highest cnt] [start end]]
                 [(max highest end) (if (> start (inc highest))
                                      (+ cnt (dec (- start highest)))
                                      cnt)])
             [0 0])
     second)

;; => 146
