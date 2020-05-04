(ns adventofcode.2019.day24)

(def input
  (vec (remove #{\newline} (slurp "resources/2019/day24.txt"))))

(defn index [[x y]]
  (+ x (* y 5)))

(def grid
  (->> (for [x (range 5)
             y (range 5)]
         [[x y] (input (index [x y]))])
       (into {})))

(def empty-tile \.)
(def bug \#)
(def bug? #{bug})

(defn biodiversity-rating [grid]
  (reduce
   (fn [score [pos tile]]
     ((fnil + 0) (when (bug? tile) (reduce * (repeat (index pos) 2))) score))
   0
   grid))

(defn neighbouring-bugs-count [neighbours-fn grid pos]
  (->> (neighbours-fn pos)
       (keep (comp bug? grid))
       count))

(defn dies? [neighbours-fn grid pos]
  (not= 1 (neighbouring-bugs-count neighbours-fn grid pos)))

(defn infested? [neighbours-fn grid pos]
  (contains? #{1 2} (neighbouring-bugs-count neighbours-fn grid pos)))

(defn update-tile [neighbours-fn grid pos]
  (let [tile (grid pos)]
    (if (bug? tile)
      (if (dies? neighbours-fn grid pos) empty-tile bug)
      (if (infested? neighbours-fn grid pos) bug empty-tile))))

(defn update-tiles [neighbours-fn grid]
  (->> (for [[k v] grid] [k (update-tile neighbours-fn grid k)])
       (into {})))


;; Part 1

(defn first-duplicate [coll]
  (last
   (reductions
    (fn [seen item]
      (if (seen item)
        (reduced item)
        (conj seen item)))
    #{}
    coll)))

(defn neighbours [[x y]]
  (for [[i j] [[1 0] [-1 0] [0 1] [0 -1]]]
    [(+ x i) (+ y j)]))

(update-tiles neighbours grid)

(->> (iterate (partial update-tiles neighbours) grid)
     first-duplicate
     biodiversity-rating)

;; => 17863711


;; Part 2

(defn middle? [[x y z]]
  (= [x y] [2 2]))

(defn recursive-neighbours [[x y z]]
  (let [current-level (remove middle? (for [pos (neighbours [x y])] (conj pos z)))
        z- (dec z)
        z+ (inc z)]
    (->> (cond (= [x y] [1 2]) [[0 0 z-] [0 1 z-] [0 2 z-] [0 3 z-] [0 4 z-]]
               (= [x y] [2 1]) [[0 0 z-] [1 0 z-] [2 0 z-] [3 0 z-] [4 0 z-]]
               (= [x y] [2 3]) [[0 4 z-] [1 4 z-] [2 4 z-] [3 4 z-] [4 4 z-]]
               (= [x y] [3 2]) [[4 0 z-] [4 1 z-] [4 2 z-] [4 3 z-] [4 4 z-]]
               (= [x y] [0 0]) [[2 1 z+] [1 2 z+]]
               (= [x y] [0 4]) [[2 3 z+] [1 2 z+]]
               (= [x y] [4 0]) [[2 1 z+] [3 2 z+]]
               (= [x y] [4 4]) [[2 3 z+] [3 2 z+]]
               (= x 0) [[1 2 z+]]
               (= x 4) [[3 2 z+]]
               (= y 0) [[2 1 z+]]
               (= y 4) [[2 3 z+]])
         (concat current-level))))

(defn update-recursive-grid [grid z]
  (->> (for [x (range 5)
             y (range 5)
             z (range (- z) (inc z))]
         [x y z])
       (remove middle?)
       (map (fn [pos] [pos (update-tile recursive-neighbours grid pos)]))
       (into {})))

(def recursive-grid
  (into {} (for [[k v] grid] [(conj k 0) v])))

(defn bug-count [grid]
  (count (filter bug? (vals grid))))

(loop [i 1, g recursive-grid]
  (if (> i 200)
    (bug-count g)
    (recur (inc i) (update-recursive-grid g i))))

;; => 1937
