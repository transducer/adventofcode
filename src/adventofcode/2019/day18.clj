(ns adventofcode.2019.day18
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.java.io :as io]
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
  (string/index-of (string/replace input "\n" "") \@))

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
  `picked-up-keys`. Returns a map of `[idx available keys]` to distance."
  [point-idx picked-up-keys]
  (let [above (- point-idx width)
        left (if (zero? (mod (dec point-idx) width)) -1 (dec point-idx))
        below (+ point-idx width)
        right (if (zero? (mod point-idx width)) -1 (inc point-idx))]
    (->> [above left below right]
         (filter (partial can-visit? picked-up-keys))
         (keep (fn [i]
                 (let [node (get maze i)]
                   (if (key? node)
                     [[i (conj picked-up-keys node)] 1]
                     [[i picked-up-keys] 1]))))
         (into {}))))

(defn dijkstra [start]
  (loop [q (priority-map [start #{}] 0), dists {}]
    (when-let [[[idx picked-up-keys] d] (peek q)]
      (if (= (count picked-up-keys) total-keys-count)
        d
        (recur (merge-with min (pop q) (->> (adjacent idx picked-up-keys)
                                            (remove-keys dists)
                                            (map-vals (partial + d))))
               (assoc dists [idx picked-up-keys] d))))))


;; Part 1

(dijkstra start)
;; => 6162
