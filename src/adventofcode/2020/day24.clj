(ns adventofcode.2020.day24
  (:require
   [clojure.string :as string]))

(def lines
  (string/split-lines (slurp "resources/2020/day24.txt")))

(defn path [line]
  (re-seq #"se|sw|nw|ne|e|w" line))

(defn walk [step [col row]]
  (case step
    "se" [(inc col) (inc row)]
    "sw" [(dec col) (inc row)]
    "nw" [(dec col) (dec row)]
    "ne" [(inc col) (dec row)]
    "e" [(+ col 2) row]
    "w" [(- col 2) row]))

(defn endpoint [line]
  (reduce
   (fn [acc step]
     (walk step acc))
   [5 5]
   (path line)))

(def grid
  (->> lines
       (map endpoint)
       (reduce
        (fn [acc endpoint]
          (if (contains? acc endpoint)
            (disj acc endpoint)
            (conj acc endpoint)))
        #{})))

(count grid)
;; => 438

(defn neighbours [[col row]]
  [[(inc col) (inc row)]
   [(dec col) (inc row)]
   [(dec col) (dec row)]
   [(inc col) (dec row)]
   [(+ col 2) row]
   [(- col 2) row]])

(defn new-grid [grid]
  (set
   (apply concat
          (for [pos (set (mapcat neighbours grid))
                :let [neighbour-count (count (filter (set (neighbours pos)) grid))]]
            (cond (and (contains? grid pos) (or (zero? neighbour-count) (> neighbour-count 2))) []
                  (and (not (contains? grid pos)) (= neighbour-count 2)) [pos]
                  (contains? grid pos) [pos]
                  :else [])))))

(count (nth (iterate new-grid grid) 100))
;; => 4038
