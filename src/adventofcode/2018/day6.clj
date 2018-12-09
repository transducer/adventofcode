(ns adventofcode.2018.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import clojure.lang.PersistentQueue))

(def coordinates
  (->> (io/resource "day6.txt")
       slurp
       (format "[%s]")
       read-string
       (partition 2)))

(defn transpose [m]
  (apply mapv vector m))

(def grid-top-left
  (let [[xs ys] (transpose coordinates)]
    [(apply min xs) (apply min ys)]))

(def grid-bottom-right
  (let [[xs ys] (transpose coordinates)]
    [(apply max xs) (apply max ys)]))

(defn neighbours [[x y]]
  [[x (inc y)]
   [x (dec y)]
   [(dec x) y]
   [(inc x) y]])

(defn within-grid? [[x y]]
  (let [[lowest-x lowest-y] grid-top-left
        [highest-x highest-y] grid-bottom-right]
    (and (>= highest-x x lowest-x)
         (>= highest-y y lowest-y))))

(defn neighbours-in-grid [[x y]]
  (filter within-grid? (neighbours [x y])))

(defn breadth-first-search [coord neighbours]
  ((fn bfs [queue]
     (lazy-seq
      (when (seq queue)
        (let [new-coord (peek queue)
              children (neighbours new-coord)]
          (cons [coord new-coord] (bfs (into (pop queue) children)))))))
   (conj PersistentQueue/EMPTY coord)))

(map #(breadth-first-search % neighbours-in-grid) coordinates)

;; TODO:
;; - breadth-first all locations
;; - Mark who has reached the earliest, and make a boundary when two have reached the same time
;; - sort-by starting coordinate
;; - count starting coordinates
;; - remove those breadth-first results that have reached the edge
