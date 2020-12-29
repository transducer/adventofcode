(ns adventofcode.2020.day03
  (:require
   [clojure.string :as string]))

(def grid
  (string/split-lines (slurp "resources/2020/day03.txt")))

(defn count-trees [[dx dy :as _slope]]
  (->> (map (fn [row x] (nth (cycle row) x))
            (take-nth dy grid)
            (iterate (partial + dx) 0))
       (filter #{\#})
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
