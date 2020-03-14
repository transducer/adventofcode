(ns adventofcode.2019.day20
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [medley.core :refer [remove-keys map-vals]]))

(def input
  (-> "2019/day20.txt" io/resource slurp))

(def width
  (string/index-of input \newline))

(def maze
  (string/replace input "\n" ""))

(defn neighbour-idxs [idx]
  (let [out-of-bounds -1
        above (- idx width)
        left (if (zero? (mod idx width)) out-of-bounds (dec idx))
        below (+ idx width)
        right (if (zero? (mod idx width)) out-of-bounds (inc idx))]
    [above left below right]))

(defn get-letter [idx]
  (re-find #"[A-Z]" (str (get maze idx))))

(defn path? [idx]
  (= (get maze idx) \.))

(defn portal
  "If `idx` is a portal returns vector of idx to name of portal,
  otherwise nil"
  [idx]
  (when-let [letter (get-letter idx)]
    (let [[above left below right :as idxs] (neighbour-idxs idx)
          [l-above l-left l-below l-right] (map get-letter idxs)]
      (cond (and l-above (path? below))
            [idx (str l-above letter)]
            (and l-left (path? right))
            [idx (str l-left letter)]
            (and l-below (path? above))
            [idx (str letter l-below)]
            (and l-right (path? left))
            [idx (str letter l-right)]
            :else
            nil))))

(def portal-idx-to-names
  (keep portal (range (count maze))))

(defn find-idx [portal-name]
  (->> portal-idx-to-names
       (filter (fn [[_idx pname]] (= pname portal-name)))
       ffirst))

(def start
  (find-idx "AA"))

(def finish
  (- (find-idx "ZZ") width))

(def portals
  "idx -> idx"
  (->> portal-idx-to-names
       (group-by second)
       (map second)
       (filter #(> (count %) 1))
       (mapcat (fn [[[idx-a _portal] [idx-b _portal]]]
                 [[idx-a idx-b] [idx-b idx-a]]))
       (into {})))

(defn adjacent
  "Finds neighbours of point index `point-idx` given available
  `portals`. Returns a map of `idx` to `distance`."
  [idx]
  (let [neighbours (neighbour-idxs idx)
        visitable (filter path? neighbours)]
    (into {} (concat (map (fn [i] [i 1]) visitable)
                     (map (fn [i] [i 0]) (keep portals neighbours))))))

(defn dijkstra
  "Computes single-source path distances in a directed graph.

  Given a node `n`, `(f n)` should return a map with the successors of `n` as keys and
  their (non-negative) distance from `n` as vals.

  Returns map with nodes as keys and their distance to `start` as vals."
  [start f]
  (loop [q (priority-map start -1), dists {}]
    (if-let [[n d] (peek q)]
      (recur (merge-with min (pop q) (map-vals (partial + d) (remove-keys dists (f n))))
             (assoc dists n d))
      dists)))

(get (dijkstra start adjacent) finish)
