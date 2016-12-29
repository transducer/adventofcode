(ns adventofcode.day13
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def input 1352)

(defn open? [[x y]]
  (and
   (>= x 0)
   (>= y 0)
   (->> (+ input (* x x) (* 3 x) (* 2 x y) y (* y y))
        Integer/toBinaryString
        (filter #(= % \1))
        count
        even?)))


;; Dijkstra adjusted from http://www.ummels.de/2014/06/08/dijkstra-in-clojure/

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [pred m]
  (select-keys m (remove pred (keys m))))

(defn dijkstra
  "Computes single-source shortest path distance in a directed graph.

  Given a node n, (f n) should return a map with the successors of n as keys and
  their (non-negative) distance from n as vals.

  Returns distance to target or nil if no path."
  [start target f]
  (loop [q (priority-map start 0) r {}]
    (when-let [[v d] (peek q)]
      (if (= v target) d
          (let [dists (->> (f v) (remove-keys r) (map-vals (partial + d)))]
            (recur (merge-with min (pop q) dists) (assoc r v d)))))))


(defn add-weights [positions]
  (into {} (for [p positions] [p 1])))

(defn possible-positions [[x y]]
  [[x (inc y)]
   [x (dec y)]
   [(dec x) y]
   [(inc x) y]])

(defn successors [[x y]]
  (->> [x y]
       possible-positions
       (filter open?)
       add-weights))


;; Part 1

(dijkstra [1 1] [31 39] successors)
;; => 90


;; Part 2

(defn neighbors [[x y]]
  (->> [x y]
       possible-positions
       (filter open?)))

(defn inc-distance [d frontier]
  (into {} (for [s frontier] [s (inc d)])))

(defn breadth-first-search
  "Finds all distinct nodes and their distance. depth specifies maximum diameter
  to search."
  [start neighbors depth]
  (loop [frontier [start]
         dist     {start 0}]
    (if-let [s (peek frontier)]
      (if (<= (dist s) depth)
        (let [new-frontier (filterv (complement #(dist %)) (neighbors s))]
          (recur (into [] (concat (pop frontier) new-frontier))
                 (conj dist (inc-distance (dist s) new-frontier))))
        (recur (pop frontier) (dissoc dist s)))
      dist)))

(count (breadth-first-search [1 1] neighbors 50))
;; => 135
