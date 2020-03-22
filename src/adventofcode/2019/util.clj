(ns adventofcode.2019.util
  (:require
   [clojure.data.priority-map :refer [priority-map]]
   [medley.core :refer [remove-keys map-vals]]))

(defn dijkstra
  "Computes single-source path distances in a directed graph.

  Given a node `n`, `(f n)` should return a map with the successors of `n` as keys and
  their (non-negative) distance from `n` as vals.

  Returns map with nodes as keys and their distance to `start` as vals.

  Source: https://www.ummels.de/2014/06/08/dijkstra-in-clojure/"
  [start f]
  (loop [q (priority-map start 0), dists {}]
    (if-let [[n d] (peek q)]
      (recur (merge-with min (pop q) (map-vals (partial + d) (remove-keys dists (f n))))
             (assoc dists n d))
      dists)))
