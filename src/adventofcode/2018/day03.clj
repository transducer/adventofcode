(ns adventofcode.2018.day03
  (:require [clojure.java.io :as io]))

(def input
  (-> "2018/day03.txt" io/resource io/reader line-seq))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse [input]
  (map #(zipmap [:id :x :y :width :height]
                (map parse-int (rest (re-find #"#(.*) @.(\d+),(\d+): (\d+)x(\d+)" %))))
       input))

(def data
  (parse input))


;; Part 1

(def max-width
  (->> (map (fn [{:keys [x width]}] (+ x width)) data)
       (apply max)))

(def max-height
  (->> (map (fn [{:keys [y height]}] (+ y height)) data)
       (apply max)))

(defn inc-indices [v idxs]
  (reduce #(update %1 %2 inc) v idxs))

(def grid
  (vec (repeat (* max-width max-height) 0)))

(defn square-indices
  "Indices of the square at (`x`, `y`) with width `width` and height `height`."
  [x y width height]
  (apply concat
         (for [n (range y (+ y height))]
           (range (+ x (* n max-width))
                  (+ x (* n max-width) width)))))

(def grid-claim-counts
  "Collection that shows per index the amount of times it is claimed."
  (reduce
   (fn [grid {:keys [x y width height]}]
     (inc-indices grid (square-indices x y width height)))
   grid
   data))

(->> grid-claim-counts
     (filter #(> % 1))
     count)


;; Part 2

(->> data
     (filter (fn [{:keys [x y width height]}]
               (every? #(= 1 %)
                       (mapv grid-claim-counts (square-indices x y width height)))))
     first
     :id)
