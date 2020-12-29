(ns adventofcode.2019.day15
  (:require
   [adventofcode.2019.intcode :refer [run-async]]
   [adventofcode.2019.util :refer [dijkstra]]
   [clojure.core.async :as async :refer [chan >!! timeout alts!!]]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [medley.core :refer [filter-vals remove-vals]]))

(def program
  (slurp (io/resource "2019/day15.txt")))

(def movements-commands
  {[0 1] 1, [0 -1] 2, [-1 0] 3, [1 0] 4})

(def opposite-movement-commands
  {1 2, 2 1, 3 4, 4 3})

(def status-codes
  {0 :wall, 1 :correct-direction, 2 :oxygen-system})

(def timeout-ms 10)

(def in (chan))
(def out (chan))

(run-async program in out)

(defn get-neighbours!
  "Walks the robot to every direction that's not a wall or already visited.
  Returns a hash map of the new nodes to status code type"
  [visited walls node]
  (->> movements-commands
       (keep (fn [[move command]]
               (let [new-node (mapv + node move)]
                 (when-not (contains? (set/union visited walls) new-node)
                   [new-node (let [_ (>!! in command)
                                   [v _] (alts!! [out (timeout timeout-ms)])
                                   status (status-codes v)]
                               (when-not (= status :wall)
                                 (>!! in (opposite-movement-commands command))
                                 (alts!! [out (timeout timeout-ms)]))
                               status)]))))
       (into {})))

(defn walk!
  "Walks robot to `new-node`, backtracking if needed. Returns new path vector."
  [node new-node path]
  (loop [curr-node node
         path path]
    (let [direction (mapv - new-node curr-node)]
      ;; Initial point [0 0]...
      (if (= new-node node)
        (conj path new-node)
        ;; If one step away move there
        (if-let [command (movements-commands direction)]
          (do
            (>!! in command)
            (alts!! [out (timeout timeout-ms)])
            (conj path new-node))
          ;; Otherwise backtrack robot to new node first
          (let [prev-node (peek (pop path))
                direction (mapv - prev-node curr-node)
                command (movements-commands direction)]
            (>!! in command)
            (alts!! [out (timeout timeout-ms)])
            (recur prev-node (pop path))))))))

(defn depth-first-walk
  "Walk the robot through the grid using DFS. Returns distance to oxygen system."
  []
  (loop [node [0 0]
         path []
         q [node]
         visited #{}
         walls #{}
         frontier (get-neighbours! visited walls node)] ; {[2 2] :wall, ...}
    (if (seq (filter-vals #{:oxygen-system} frontier))
      (count path)
      (when-let [new-node (peek q)]
        (let [new-path (walk! node new-node path)
              new-visited (conj visited new-node)
              new-frontier (get-neighbours! walls new-visited new-node)
              new-walls (apply conj walls (keys (filter-vals #{:wall} new-frontier)))
              new-q (apply conj (pop q)
                           (apply disj (set (keys (remove-vals #{:wall} new-frontier)))
                                  (set/union new-visited new-walls)))]
          (recur new-node new-path new-q new-visited new-walls new-frontier))))))

(depth-first-walk)
;; => 380

(defn depth-first-explore
  "Walks the robot through the grid using DFS till every node is visited.

  Returns a map of the grid with nodes as keys and its status code
  type (`:wall`,`:correct-direction`, `:oxygen-system`) as values."
  []
  (loop [node [0 0]
         path []
         q [node]
         m {}
         visited #{}
         walls #{}
         frontier (get-neighbours! visited walls node)]
    (if-let [new-node (peek q)]
      (let [new-path (walk! node new-node path)
            new-visited (conj visited new-node)
            new-frontier (get-neighbours! walls new-visited new-node)
            new-walls (apply conj walls (keys (filter-vals #{:wall} new-frontier)))
            new-map (merge m frontier)
            new-q (apply conj (pop q)
                         (apply disj (set (keys (remove-vals #{:wall} new-frontier)))
                                (set/union new-visited new-walls)))]
        (recur new-node new-path new-q new-map new-visited new-walls new-frontier))
      m)))

(def grid (depth-first-explore))

(def valid-nodes
  (->> grid (filter-vals #{:correct-direction}) keys set))

(defn successors [node]
  (let [adjacent-nodes (set (for [move (keys movements-commands)] (mapv + node move)))
        neighbours (set/intersection adjacent-nodes valid-nodes)
        distance 1]
    (into {} (for [n neighbours] [n distance]))))

(def oxygen-system-node
  (ffirst (filter-vals #{:oxygen-system} grid)))

(def distances vals)

(->> (dijkstra oxygen-system-node successors)
     distances
     (apply max))
;; => 410
