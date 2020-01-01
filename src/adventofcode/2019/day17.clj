(ns adventofcode.2019.day17
  (:require [adventofcode.2019.intcode :refer [run]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def program
  (slurp (io/resource "2019/day17.txt")))


;; Part 1

(def grid
  (->> (run program)
       (map char)
       (apply str)
       string/split-lines))

(def width
  (count (first grid)))

(def height
  (count grid))

(def transpose
  (partial apply mapv vector))

(defn scaffold? [i e]
  (let [row (grid (int (/ i width)))
        col ((transpose grid) (mod i width))
        top (get col (dec (int (/ i width))) \X)
        bottom (get col (inc (int (/ i width))) \X)
        left (get row (dec (mod i width)) \X)
        right (get row (inc (mod i width)) \X)]
    (every? #{\#} [e top bottom left right])))

(def scaffold-intersections
  (keep-indexed
   (fn [i e]
     (when (scaffold? i e)
       [(mod i width) (int (/ i width))]))
   (apply str grid)))

(reduce (fn [acc [x y]] (+ acc (* x y))) 0 scaffold-intersections)

;; => 3920
