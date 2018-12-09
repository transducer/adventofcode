(ns adventofcode.2016.day17
  (:require [digest :refer [md5]]
            [clojure.set :as set]))

(def passcode "ioramepc")
(def grid-size 4)

(defn allowed-directions [path]
  (->> (md5 (str passcode path))
       (take 4)
       (map-indexed #(hash-map %1 (#{\b \c \d \e \f} %2)))
       (map #(set/rename-keys % {0 \U 1 \D 2 \L 3 \R}))
       (filter #(first (vals %)))
       (mapcat keys)))

#_(assert (= (allowed-directions "DU") '(\R)))
#_(assert (empty? (allowed-directions "DUR")))

(def moves
  {\U (fn [[x y]] [x (dec y)])
   \D (fn [[x y]] [x (inc y)])
   \L (fn [[x y]] [(dec x) y])
   \R (fn [[x y]] [(inc x) y])})

(defn allowed?
  "A position is allowed if it does not go over the edge of the grid."
  [[x y]]
  (and (<= 0 x (dec grid-size))
       (<= 0 y (dec grid-size))))

(defn neighbors
  "Returns map of path and position."
  [path position]
  (into {}
        (for [d     (allowed-directions path)
              :let  [new-pos ((moves d) position)]
              :when (allowed? new-pos)]
          [(str path d) new-pos])))

(def min-length #(min (count %1) (count %2)))

(defn dijkstra
  "Finds shortest path from start to finish. For a node n (neighbors n) should
  return a map of its valid successors with as key path and value position."
  [start neighbors finish]
  (loop [frontier (sorted-map-by min-length "" start)]
    (when-let [[path pos] (last frontier)]
      (if (= pos finish)
        path
        (let [new-frontier (neighbors path pos)]
          (recur (into (sorted-map-by min-length) (merge (butlast frontier) new-frontier))))))))


;; Part 1

(dijkstra [0 0] neighbors [3 3])
;; => "RDDRULDDRR"


;; Part 2

(defn longest-path
  "Breadth-first search to find length of longest path from start to finish."
  [start neighbors finish]
  (loop [frontier {"" start}
         longest  0]
    (if-let [[path pos] (first frontier)]
      (if (= pos finish)
        (recur (into {} (next frontier))
               (max longest (count path)))
        (let [new-frontier (neighbors path pos)]
          (recur (merge (into {} (next frontier)) new-frontier)
                 longest)))
      longest)))

(longest-path [0 0] neighbors [3 3])
;; => 766
