(ns adventofcode.2020.day3
  (:require
   [clojure.string :as string]))

(def grid
  (string/split-lines (slurp "resources/2020/day3.txt")))

(defn get-point [[x y]]
  (when-let [row (get grid y)]
    (nth (cycle row) x)))

(defn neighbour [[offset-x offset-y :as _slope] [x y :as _point]]
  [(+ x offset-x) (+ y offset-y)])

(def height
  (count grid))

(defn count-trees [slope]
  (->> (iterate (partial neighbour slope) slope)
       (take height)
       (filter (comp #{\#} get-point))
       count))


;; Part 1

(count-trees [3 1])

;; => 262


;; Part 2

(* (count-trees [1 1])
   (count-trees [3 1])
   (count-trees [5 1])
   (count-trees [7 1])
   (count-trees [1 2]))

;; => 2698900776
