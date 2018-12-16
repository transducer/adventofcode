(ns adventofcode.2018.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import clojure.lang.PersistentQueue))

(def coordinates
  (->> (io/resource "2018/day6.txt")
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

(defn on-edge?
  "If a point is on the edge, the area that contains the pont is infinite."
  [[x y]]
  (let [[lowest-x lowest-y] grid-top-left
        [highest-x highest-y] grid-bottom-right]
    (or (= x highest-x) (= x lowest-x) (= y highest-y) (= y lowest-y))))

(defn inc-distance [d frontier]
  (into {} (for [s frontier] [s (inc d)])))

(defn breadth-first-search
  "Finds (non-lazily for now) all distinct nodes and their distance."
  [start neighbours]
  (loop [queue (conj PersistentQueue/EMPTY start)
         distances {start [0 start]}] ;; coord -> [distance start]
    (if-let [coord (peek queue)]
      (let [frontier (into {}
                           (for [c (remove #(distances %) (neighbours coord))]
                             [c [(inc (first (distances coord))) start]]))]
        (recur (apply conj (pop queue) (keys frontier))
               (merge-with #(min (first %1) (first %2)) distances frontier)))
      distances)))

(->> (reduce
      (fn [acc [coord [dist start]]]
        (if-let [prev-coord (acc coord)]
          (let [[prev-dist prev-start boundary?] prev-coord]

            ;; Like every interesting procedure, it's a case analysis. (https://youtu.be/0m6hoOelZH8?t=331)
            (cond boundary?
                  (if (> prev-dist dist)
                    (assoc acc coord [dist start false])
                    acc)
                  (= prev-dist dist)
                  (assoc acc coord [dist start true])
                  (> prev-dist dist)
                  (assoc acc coord [dist start false])
                  :else acc))
          (assoc acc coord [dist start false])))
      {} ; -> map of coord -> [distance start boundary?]
      (->> (map #(breadth-first-search % neighbours-in-grid) coordinates)
           (apply interleave)))
     (filter (fn [[_coord [_dist _start boundary?]]] (not boundary?)))
     (group-by (fn [[_coord [_dist start _boundary]]] start))
     (map (fn [[start coord->dist-start-boundaries]] [start (keys coord->dist-start-boundaries)]))
     (remove (fn [[_start coords-closest]] (some #(on-edge? %) coords-closest)))
     (map (fn [[_start coords-closest]] (count coords-closest)))
     (apply max))
