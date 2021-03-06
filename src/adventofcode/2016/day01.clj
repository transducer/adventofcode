(ns adventofcode.2016.day01
  (:require
   [clojure.string :as string]))

(def input
  (-> "resources/2016/day01.txt" slurp string/trim-newline (string/split #", ")))

(defn parse
  [data]
  (->> data
       (map (juxt first #(->> % rest (apply str) Integer/parseInt)))))

(def directions
  [:north :west :south :east])

(defn update-direction
  [curr turn]
  (let [idx (.indexOf directions curr)
        new-idx (mod (({\R inc \L dec} turn) idx) 4)
        new-direction (directions new-idx)]
    new-direction))

(defn points-visited
  [pos direction dist]
  (->> pos
       (iterate (fn [[x y]]
                  ({:east [(inc x) y]
                    :west [(dec x) y]
                    :north [x (inc y)]
                    :south [x (dec y)]}
                   direction)))
       (take (inc dist))
       rest))

(defn positions
  [movements]
  (->> movements
       (reductions
        (fn [[positions direction] [turn distance]]
          (let [direction (update-direction direction turn)
                visited   (points-visited (last positions) direction distance)]
            [visited direction]))
        [[[0 0]] :north])
       (mapcat first)
       vec))

(defn distance
  [point]
  (apply + (map #(Math/abs %) point)))

(-> input
    parse
    positions
    last
    distance)
;; => 332

(defn first-visited-twice
  [points]
  (loop [n 0]
    (let [before (take n points)
          curr   (points n)]
      (if (some #(= % curr) before)
        curr
        (recur (inc n))))))

(-> input
    parse
    positions
    first-visited-twice
    distance)
;; => 166
