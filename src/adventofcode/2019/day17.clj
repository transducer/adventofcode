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

(defn intersection? [x y e]
  (let [row (grid y)
        col ((transpose grid) x)
        left (get row (dec x))
        right (get row (inc x))
        top (get col (dec y))
        bottom (get col (inc y))]
    (every? #{\#} [e top bottom left right])))

(def intersections
  (keep-indexed
   (fn [i e]
     (let [x (mod i width)
           y (int (/ i width))]
       (when (intersection? x y e)
         [x y])))
   (apply str grid)))

(reduce (fn [acc [x y]] (+ acc (* x y))) 0 intersections)

;; => 3920


;; Part 2

;; Naive path of going straight always (by hand):

"R8L4R4R10R8R8L4R4R10R8L12L12R8R8R10R4R4L12L12R8R8R10R4R4L12L12R8R8R10R4R4R10R4R4R8L4R4R10R8"
