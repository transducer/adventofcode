(ns adventofcode.2019.day20
  (:require [adventofcode.2019.util :refer [dijkstra]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

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
  (+ (find-idx "AA") width))

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


;; Part 1

(defn adjacent
  "Finds neighbours of point index `point-idx` given available
  `portals`. Returns a map of `idx` to `distance`."
  [idx]
  (let [neighbours (neighbour-idxs idx)
        visitable (filter path? neighbours)]
    (into {} (concat (for [i visitable] [i 1])
                     (for [i (keep portals neighbours)] [i 0])))))

(get (dijkstra start adjacent) finish)

;; => 696


