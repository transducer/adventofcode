(ns adventofcode.2016.day3
  (:require [clojure.string :as str]))

(def input
  (slurp "resources/2016/day3.txt"))

(defn parse
  [data]
  (->> input
       str/split-lines
       (map #(str/trim %))
       (map #(str/split % #"\s+"))
       (map (fn [t] (map #(Integer/parseInt %) t)))))

(defn valid-triangle?
  [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))


;; Part 1

(->> input
     parse
     (filter valid-triangle?)
     count)


;; Part 2

(defn to-triangle-cols
  [rows]
  (->> rows
       (apply mapv vector)
       (mapcat (partial partition 3))))

(->> input
     parse
     to-triangle-cols
     (filter valid-triangle?)
     count)
