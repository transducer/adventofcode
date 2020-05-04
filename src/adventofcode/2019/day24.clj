(ns adventofcode.2019.day24)

(def grid
  (vec (remove #{\newline} (slurp "resources/2019/day24.txt"))))

(def empty-tile \.)
(def bug \#)
(def bug? #{bug})

(defn biodiversity-rating [grid]
  (reduce-kv
   (fn [score index tile]
     ((fnil + 0) (when (bug? tile) (reduce * (repeat index 2))) score))
   0
   grid))

(defn neighbours [index]
  (let [side-length 5
        right (when-not (zero? (mod (inc index) side-length)) (inc index))
        left (when-not (zero? (mod index side-length)) (dec index))
        above (- index side-length)
        below (+ index side-length)]
    (filterv (set (range 25)) [right left above below])))

(defn neighbouring-bugs-count [grid index]
  (->> (neighbours index)
       (keep (comp bug? grid))
       count))

(defn dies? [grid index]
  (not= 1 (neighbouring-bugs-count grid index)))

(defn infested? [grid index]
  (< 0 (neighbouring-bugs-count grid index) 3))

(defn update-tile [grid index]
  (let [tile (grid index)]
    (if (bug? tile)
      (if (dies? grid index) empty-tile bug)
      (if (infested? grid index) bug empty-tile))))

(defn update-tiles [grid]
  (mapv (partial update-tile grid) (range 25)))

(defn first-duplicate [coll]
  (last
   (reductions
    (fn [seen item]
      (if (seen item)
        (reduced item)
        (conj seen item)))
    #{}
    coll)))


;; Part 1

(->> (iterate update-tiles grid)
     first-duplicate
     biodiversity-rating)

;; => 17863711
