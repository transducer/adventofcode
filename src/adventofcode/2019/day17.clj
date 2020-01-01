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

(defn scaffold? [[x y] e]
  (let [row (grid y)
        col ((transpose grid) x)
        left (get row (dec x))
        right (get row (inc x))
        top (get col (dec y))
        bottom (get col (inc y))]
    (every? #{\#} [e top bottom left right])))

(def scaffold-intersections
  (keep-indexed
   (fn [i e]
     (let [[x y] [(mod i width) (int (/ i width))]]
       (when (scaffold? [x y] e)
         [x y])))
   (apply str grid)))

(reduce (fn [acc [x y]] (+ acc (* x y))) 0 scaffold-intersections)

;; => 3920
