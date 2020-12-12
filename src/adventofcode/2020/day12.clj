(ns adventofcode.2020.day12
  (:require
   [clojure.string :as string]))

(def instructions
  (string/split-lines (slurp "resources/2020/day12.txt")))

(defn turn-left [direction]
  (case direction
    \N \W
    \S \E
    \E \N
    \W \S))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(->> instructions
     (reduce
      (fn [{:keys [direction] :as acc} [action & values]]
        (let [n (Integer. (apply str values))]
          (condp = action
            \N (update acc :location (fn [[x y]] [(+ x n) y]))
            \S (update acc :location (fn [[x y]] [(- x n) y]))
            \E (update acc :location (fn [[x y]] [x (+ y n)]))
            \W (update acc :location (fn [[x y]] [x (- y n)]))
            \L (condp = n
                 90 (update acc :direction turn-left)
                 180 (update acc :direction (comp turn-left turn-left))
                 270 (update acc :direction (comp turn-left turn-left turn-left)))
            \R (condp = n
                 90 (update acc :direction (comp turn-left turn-left turn-left))
                 180 (update acc :direction (comp turn-left turn-left))
                 270 (update acc :direction turn-left))
            \F (condp = direction
                 \N (update acc :location (fn [[x y]] [(+ x n) y]))
                 \S (update acc :location (fn [[x y]] [(- x n) y]))
                 \E (update acc :location (fn [[x y]] [x (+ y n)]))
                 \W (update acc :location (fn [[x y]] [x (- y n)]))))))
      {:direction \E
       :location [0 0]})
     :location
     manhattan-distance)
;; => 938

(defn rotate-right [[i j]]
  [j (- i)])

(defn move-ship [{[x y] :ship-location [i j] :waypoint-location} n]
  {:ship-location [(+ (* n i) x) (+ (* n j) y)]
   :waypoint-location [i j]})

(->> instructions
     (reduce
      (fn [{[i j] :waypoint-location :as acc} [action & values]]
        (let [n (Integer. (apply str values))]
          (condp = action
            \N (assoc acc :waypoint-location [i (+ j n)])
            \S (assoc acc :waypoint-location [i (- j n)])
            \E (assoc acc :waypoint-location [(+ i n) j])
            \W (assoc acc :waypoint-location [(- i n) j])
            \L (update acc :waypoint-location
                       (condp = n
                         90 (comp rotate-right rotate-right rotate-right)
                         180 (comp rotate-right rotate-right)
                         270 rotate-right))
            \R (update acc :waypoint-location
                       (condp = n
                         270 (comp rotate-right rotate-right rotate-right)
                         180 (comp rotate-right rotate-right)
                         90 rotate-right))
            \F (move-ship acc n))))
      {:ship-location [0 0]
       :waypoint-location [10 1]})
     :ship-location
     manhattan-distance)
;; => 54404
