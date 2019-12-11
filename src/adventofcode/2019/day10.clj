(ns adventofcode.2019.day10
  (:require
   [clojure.algo.generic.math-functions :refer [atan2]]
   [clojure.string :as string]))

(def input
  (string/split-lines (slurp "resources/2019/day10.txt")))

(def width (count (first input)))
(def height (count input))

(def asteroids
  (set
   (keep-indexed (fn [i v]
                   (when (= v \#)
                     [(mod i width) (int (/ i width))]))
                 (apply str input))))

(defn visible-count [[x y :as point]]
  (->> (for [[x_a y_a :as asteroid] (disj asteroids point)
             :let [atan (atan2 (- x_a x) (- y_a y))]]
         [atan asteroid])
       (into {})
       count))



;; Part 1

(apply max (map visible-count asteroids))

;; =>


;; Part 2

;; =>
