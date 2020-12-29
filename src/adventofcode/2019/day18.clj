(ns adventofcode.2019.day18
  (:require
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :refer [map-vals remove-keys]]))

(def input
  (-> "2019/day18.txt" io/resource slurp))

(def height
  (count (filter #{\newline} input)))

(def width
  (string/index-of input \newline))

(def keys-regex
  #"[a-z]")

(def total-keys-count
  (count (re-seq keys-regex input)))

(def start
  (-> input
      (string/replace "\n" "")
      (string/index-of \@)))

(def maze
  (-> input
      (string/replace "\n" "")
      (string/replace \@ \.)))

(defn can-open?
  "Can `lock` be opened with any `picked-up-key`?
  e.g., key \"a\" can open lock \"A\""
  [picked-up-keys lock]
  (contains? picked-up-keys (first (string/lower-case lock))))

(defn key? [node]
  (re-find keys-regex (str node)))

(defn can-visit? [picked-up-keys idx]
  (when-let [node (get maze idx)]
    (and (not= node \#)
         (or (= node \.)
             (key? node)
             (can-open? picked-up-keys node)))))

(defn adjacent
  "Finds neighbours of point index `point-idx` given available keys
  `picked-up-keys`. Returns a map of `[idx available-keys]` to distance."
  [maze point-idx picked-up-keys]
  (let [out-of-bounds -1
        above (- point-idx width)
        left (if (zero? (mod (dec point-idx) width)) out-of-bounds (dec point-idx))
        below (+ point-idx width)
        right (if (zero? (mod point-idx width)) out-of-bounds (inc point-idx))]
    (->> [above left below right]
         (filter (partial can-visit? picked-up-keys))
         (keep (fn [i]
                 (let [node (get maze i)
                       distance 1]
                   (if (key? node)
                     [[i (conj picked-up-keys node)] distance]
                     [[i picked-up-keys] distance]))))
         (into {}))))

(defn dijkstra
  ([start initial-keys]
   (loop [q (priority-map [start initial-keys] 0), dists {}]
     (when-let [[[idx picked-up-keys] d] (peek q)]
       (if (= (count picked-up-keys) total-keys-count)
         d
         (recur (merge-with min (pop q) (->> (adjacent maze idx picked-up-keys)
                                             (remove-keys dists)
                                             (map-vals (partial + d))))
                (assoc dists [idx picked-up-keys] d)))))))

(dijkstra start #{})
;; => 6162

(def closed-maze
  (->> maze
       (map-indexed
        (fn [i c]
          (condp = i
            start \#
            (+ start width) \#
            (- start width) \#
            (dec start) \#
            (inc start) \#
            c)))
       (apply str)))

(defn quadrant [idx]
  (let [upper? (< (quot idx width) (int (/ height 2)))
        left? (< (mod idx width) (int (/ width 2)))]
    (cond (and upper? left?) 0
          upper? 1
          (and (not upper?) left?) 2
          :else 3)))

(def quadrant-keys
  "Map of quadrant (0, 1, 2, 3) to set of keys in it"
  (reduce-kv
   (fn [acc idx e]
     (if (key? e)
       (update acc (quadrant idx) #((fnil conj #{}) % e))
       acc))
   {}
   (vec closed-maze)))

(def starts
  [(- (dec start) width)
   (- (inc start) width)
   (+ (dec start) width)
   (+ (inc start) width)])

(->> (for [quadrant (range 4)
           :let [initial-keys (apply set/union (vals (dissoc quadrant-keys quadrant)))
                 start (nth starts quadrant)]]
       (dijkstra start initial-keys))
     (reduce +))
;; => 1556
