(ns adventofcode.2016.day5
  (:require [digest :refer [md5]]))

(def input
  "reyedfim")


;; Part 1

(->> input
     repeat
     (map-indexed #(md5 (str %2 %1)))
     (filter #(= (subs % 0 5) "00000"))
     (take 8)
     (map #(subs % 5 6))
     (apply str))


;; Part 2

(->> input
     repeat
     (map-indexed #(md5 (str %2 %1)))
     (filter #(= (subs % 0 5) "00000"))
     (reduce
      #(let [idx (try
                   (Integer/parseInt (subs %2 5 6))
                   (catch NumberFormatException _ 8))
             c   (subs %2 6 7)]
         (if (some (fn [val] (= val -1)) %1)
           (if (and (< idx 8)
                    (= (%1 idx) -1))
             (assoc %1 idx c)
             %1)
           (reduced %1)))
      (vec (repeat 8 -1)))
     (apply str))
