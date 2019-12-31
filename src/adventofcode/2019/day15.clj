(ns adventofcode.2019.day15
  (:require
   [adventofcode.2019.intcode :refer [run-async]]
   [clojure.core.async :as async :refer [chan >!! timeout alts!!]]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [medley.core :refer [filter-vals remove-vals]]))

(def program
  (slurp (io/resource "2019/day15.txt")))


;; Part 1

(def movements-commands
  {[0 1] 1
   [0 -1] 2
   [-1 0] 3
   [1 0] 4})

(def opposite-movement-commands
  {1 2
   2 1
   3 4
   4 3})

(def status-codes
  {0 :wall
   1 :correct-direction
   2 :oxygen-system})

(def timeout-ms 10)

(def in (chan))
(def out (chan))

(run-async program in out)

(defn neighbours [visited walls loc]
  ;; Check every direction and create a hash map of location to type
  (->> movements-commands
       (keep (fn [[move command]]
               (let [new-loc (mapv + loc move)]
                 (when-not (contains? (set/union visited walls) new-loc)
                   [new-loc (let [_ (>!! in command)
                                  [v _] (alts!! [out (timeout timeout-ms)])
                                  status (status-codes v)]
                              (when-not (= status :wall)
                                (>!! in (opposite-movement-commands command))
                                (alts!! [out (timeout timeout-ms)]))
                              status)]))))
       (into {})))

(defn walk
  "Walks robot to `new-loc`, backtracking if needed.
  Returns new path"
  [loc new-loc path]
  (loop [curr-loc loc
         path path]
    (let [direction (mapv - new-loc curr-loc)]
      ;; Initial point [0 0]...
      (if (= new-loc loc)
        (conj path new-loc)
        ;; If one step away move there
        (if-let [command (movements-commands direction)]
          (do
            (>!! in command)
            (alts!! [out (timeout timeout-ms)])
            (conj path new-loc))
          ;; Otherwise backtrack robot to new location first
          (let [prev-loc (peek (pop path))
                direction (mapv - prev-loc curr-loc)
                command (movements-commands direction)]
            (>!! in command)
            (alts!! [out (timeout timeout-ms)])
            (recur prev-loc (pop path))))))))

(defn depth-first-walk
  "Walk the robot through the grid using DFS. Returns the distance to
  the oxygen system."
  []
  (loop [loc [0 0]
         path []
         q [loc]
         visited #{}
         walls #{}
         frontier (neighbours visited walls loc)] ; {[2 2] :wall, ...}
    (if (seq (filter-vals #{:oxygen-system} frontier))
      (count path)
      (when-let [new-loc (peek q)]
        (let [new-path (walk loc new-loc path)
              new-visited (conj visited new-loc)
              new-frontier (neighbours walls new-visited new-loc)
              new-walls (apply conj walls (keys (filter-vals #{:wall} new-frontier)))
              new-q (apply conj (pop q)
                           (apply disj (set (keys (remove-vals #{:wall} new-frontier)))
                                  (set/union new-visited new-walls)))]
          (recur new-loc new-path new-q new-visited new-walls new-frontier))))))

(depth-first-walk)

;; => 380
