(ns adventofcode.2019.day10
  (:require
   [clojure.string :as string]))

(def input
  (string/split-lines (slurp "resources/2019/day10.txt")))

(def width (count (first input)))
(def height (count input))

(def asteroids
  (set
   (keep-indexed (fn [i v]
                   (when (= v \#)
                     [(mod i width) (int (/ i width))]))
                 (apply str input))))

(defn visible-count [[x y :as point]]
  (->> (for [[x_a y_a :as asteroid] (disj asteroids point)
             :let [atan (Math/atan2 (- y_a y) (- x_a x))]]
         [atan asteroid])
       (into {})
       count))


(apply max (map visible-count asteroids))
;; => 276

(def atan2->nearest-asteroids
  (let [[x y :as _point] [17 22]]
    (->> asteroids
         (map (fn [[x_a y_a :as asteroid]]
                [(Math/atan2 (- y_a y) (- x_a x)) asteroid]))
         (sort-by (fn [[_ [x_a y_a]]]
                    (+ (Math/abs (- y_a y))
                       (Math/abs (- x_a x)))))
         (into (sorted-map)))))

(def count-after-minus-half-pi
  (->> atan2->nearest-asteroids
       (filter (fn [[atan2 _]]
                 (>= atan2 (- (/ Math/PI 2)))))
       count))

(let [[x y] (-> (vec atan2->nearest-asteroids)
                (nth (- 199 count-after-minus-half-pi))
                val)]
  (+ (* 100 x) y))
;; => 1321
