(ns adventofcode.2019.day12
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [abs lcm]]
            [clojure.string :as string]))

(def moons
  (->> (io/resource "2019/day12.txt")
       slurp
       string/split-lines
       (map (comp (partial zipmap [:x :y :z])
                  (partial map #(Integer/parseInt %))
                  rest
                  (partial re-find #".*x=(-?\d+).*y=(-?\d+).*z=(-?\d+)")))
       (map-indexed (fn [i p] {:name i, :positions p, :velocities {:x 0, :y 0, :z 0}}))))

(defn gravity [moons]
  (->> (for [{{px_a :x, py_a :y, pz_a :z} :positions name :name} moons
             {{px_b :x, py_b :y, pz_b :z} :positions} moons]
         {:name name
          :x (compare px_b px_a)
          :y (compare py_b py_a)
          :z (compare pz_b pz_a)})
       (group-by :name)
       (map (fn [[_ changes]] (apply merge-with + changes)))
       (map (fn [{:keys [velocities] :as moon} velocity-changes]
              (assoc moon :velocities (merge-with + velocities (dissoc velocity-changes :name))))
            moons)
       (map (fn [{:keys [positions velocities] :as moon}]
              (assoc moon :positions (merge-with + positions velocities))))))

(defn energy [moons]
  (->> moons
       (map (fn [{:keys [velocities positions]}]
              (* (apply + (map abs (vals velocities)))
                 (apply + (map abs (vals positions))))))
       (apply +)))


;; Part 1

(energy (nth (iterate gravity moons) 1000))

;; => 6220


;; Part 2

(defn idx-back-to-initial-state [states]
  (first
   (keep-indexed
    (fn [i state] (when (= state (first states)) (inc i)))
    (rest states))))

(defn period [moon-name]
  (->> moons
       (iterate gravity)
       (map (comp first (partial filter (comp #{moon-name} :name))))
       idx-back-to-initial-state))

(reduce lcm (map period (range 4)))
