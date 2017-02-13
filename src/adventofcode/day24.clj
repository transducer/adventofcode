(ns adventofcode.day24
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :refer [permutations combinations]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input
  (-> "day24.txt" io/resource io/reader line-seq))

(defn mapcat-indexed [f c]
  (apply concat (map-indexed f c)))

(defn parse
  "Creates map from coords [x,y] to type (\\#, \\. and \1, \2, ... \n)."
  [lines]
  (apply hash-map
         (mapcat-indexed
          (fn [y line]
            (mapcat-indexed
             (fn [x point] [[x y], point])
             line))
          lines)))

(def maze (parse input))

;; We calculate every distance between every destination using A*.
;; Then we find every possible order, sort by total distance, and take the
;; first.

(def destinations
  "The position of the destinations, starting with \0"
  (->> maze
       (filterv (fn [[pos point]] (<= 48 (int point) 55)))
       (sort-by second)
       (map first)))

(defn remove-keys [pred m]
  (select-keys m (remove pred (keys m))))

(defn by-weight
  "Weight includes distance."
  [a b]
  (if (< (first a) (first b)) a b))

(defn A*
  "Computes single-source shortest path distance in a directed graph.

  Given a node n, (f n) should return a map of the successors as keys, that have
  weight and distance as value in a vector.

  Returns distance to target or nil if no path."
  [start target f h]
  (loop [q (priority-map start [Long/MAX_VALUE 0]), visited {}]
    (when-let [[v [w d]] (peek q)] ;; vertex [weight, distance]
      (if (= v target) d
          (let [dists (->> (f v)
                           (h target d 1)
                           (remove-keys visited))]
            (recur (merge-with by-weight (pop q) dists) (assoc visited v w)))))))

(defn heuristic
  "Weight is Euclidean distance, distance is 1 per step."
  [[endx endy] distance extra-distance positions] ; hack distance and extra
                                                  ; distance into heuristic (we
                                                  ; are sorting for what node to
                                                  ; pick next only by weight,
                                                  ; and it should include the
                                                  ; distance)
  (let [new-distance (+ distance extra-distance)]
    (into {} (for [[x y :as p] positions]
               [p [(+ new-distance
                      (Math/sqrt (+ (Math/pow (- endx x) 2) (Math/pow (- endy y) 2))))
                   new-distance]]))))

(defn possible-positions [[x y]]
  [[x (inc y)]
   [x (dec y)]
   [(dec x) y]
   [(inc x) y]])

(defn open? [d pos]
  (and (d pos) (not= (d pos) \#)))

(defn successors [pos]
  (->> pos
       possible-positions
       (filter (partial open? (parse input)))))

(defn possible-paths
  "Possible paths are start with all permutations of the next destinations
  added. Results in sequence of [start ends], a to b, b to c, ... n-1 to n."
  [[start & to-visit :as destinations]]
  (->> (permutations to-visit)
       (map #(conj % start))
       (map (partial partition 2 1))))

(def distances
  "Creates hash-map with vector start to target as key, and distance as value."
  (->> (combinations destinations 2)
       (pmap (fn [[start target :as route]]
               [route (A* start target successors heuristic)]))
       (into {})))


;; Part 1

(->> destinations
     possible-paths
     (map #(reduce
            (fn [dist [a b]] (+ dist (or (distances [a b]) (distances [b a]))))
            0
            %))
     sort
     first)

;; => 518
