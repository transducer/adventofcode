(ns adventofcode.day3
  (:require [clojure.string :as str]))

(def input
  (slurp "resources/day3.txt"))

(defn parse
  [data]
  (->> input
       str/split-lines
       (map #(str/trim %))
       (map #(str/split % #"\s+"))
       (map (fn [t] (map #(Integer/parseInt %) t)))))

(defn valid-triangle
  [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))


;; Part 1

(->> input
     parse
     (filter valid-triangle)
     count)


;; Part 2

(defn triangle-cols
  [rows]
  (->> rows
       (apply mapv vector)
       (mapcat (partial partition 3))))

(->> input
     parse
     triangle-cols
     (filter valid-triangle)
     count)
