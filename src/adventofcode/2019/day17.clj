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

;; Naive path of going straight always (by hand)
(def path
  "R8L4R4R10R8R8L4R4R10R8L12L12R8R8R10R4R4L12L12R8R8R10R4R4L12L12R8R8R10R4R4R10R4R4R8L4R4R10R8")

;; Common parts (by hand)
(def replacements
  {"A" "R8L4R4R10R8"
   "B" "L12L12R8R8"
   "C" "R10R4R4"})

(def main-routine
  (->> replacements
       (reduce-kv (fn [s k v] (string/replace s v k)) path)
       (interpose \,)
       (map int)
       vec))

(defn to-int [v]
  (if (= (count v) 2)
    [(int (first v)) (int (second v))]
    [(int (first v))]))

(defn ascii-seq [path]
  (->> path
       (re-seq #"([L|R])(\d+)")
       (mapcat rest)
       (interpose ",")
       (mapcat to-int)))

(def functions
  (mapv ascii-seq (vals replacements)))

(def video-feed-input
  [(int \n) (int \newline)])

(def inputs
  (->> (conj functions video-feed-input)
       (into [main-routine])
       (interpose [(int \newline)])
       (apply concat)))

(peek (apply run (string/replace-first program "1" "2") inputs))

;; => 673996
