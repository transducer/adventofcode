(ns adventofcode.2020.day17
  (:require
   [clojure.string :as string]))

(def input
  (string/split-lines (slurp "resources/2020/day17.txt")))

(def initial-state
  (->> input
       (map-indexed (fn [y line] (map-indexed (fn [x c] [[x y 0] (= c \#)]) line)))
       (apply concat)
       (into {})))

(defn neighbours [[x y z]]
  (for [dx (range -1 2)
        dy (range -1 2)
        dz (range -1 2)
        :when (not= dx dy dz 0)]
    [(+ x dx) (+ y dy) (+ z dz)]))

(defn new-active [active? neighbours state]
  (let [active-count (count (filter true? (for [point neighbours] (get state point))))]
    (if active?
      (<= 2 active-count 3)
      (= 3 active-count))))

(defn new-state [state]
  (->> (mapcat
        (fn [[point _]]
          (map
           (fn [p]
             [p (new-active (get state p) (neighbours p) state)])
           (conj (neighbours point) point)))
        state)
       (into {})))

(->> initial-state
     (iterate new-state)
     (drop 6)
     first
     vals
     (filter true?)
     count)
;; => 202

(def initial-state-4d
  (->> input
       (map-indexed (fn [y line] (map-indexed (fn [x c] [[x y 0 0] (= c \#)]) line)))
       (apply concat)
       (into {})))

(defn neighbours-4d [[x y z w]]
  (for [dx (range -1 2)
        dy (range -1 2)
        dz (range -1 2)
        dw (range -1 2)
        :when (not= dx dy dz dw 0)]
    [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))

(defn new-state-4d [state]
  (->> (mapcat
        (fn [[point _]]
          (map
           (fn [p]
             [p (new-active (get state p) (neighbours-4d p) state)])
           (conj (neighbours-4d point) point)))
        state)
       (into {})))

(->> initial-state-4d
     (iterate new-state-4d)
     (drop 6)
     first
     vals
     (filter true?)
     count)
;; => 2028
