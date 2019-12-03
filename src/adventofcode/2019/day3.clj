(ns adventofcode.2019.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input
  (->> (-> (io/resource "2019/day3.txt")
           slurp
           (string/split #"\n"))
       (map #(string/split % #","))))

(def directions
  {\R [1 0]
   \L [-1 0]
   \U [0 1]
   \D [0 -1]})

(defn move [dir point]
  (mapv + (directions dir) point))

(defn steps [dir step-cnt point]
  (->> point
       (iterate (partial move dir))
       (rest)
       (take step-cnt)))

(defn points [path]
  (reduce (fn [acc [dir & ns]]
            (let [step-cnt (Integer/parseInt (apply str ns))]
              (apply conj acc
                     (steps dir step-cnt (peek acc)))))
          [[0 0]]
          path))

(defn crosses [path1 path2]
  (->> (concat (distinct path1) (distinct path2))
       frequencies
       (filter (fn [[point cnt]] (> cnt 1)))))


;; Part 1

(->> input
     (map points)
     (apply crosses)
     (map (fn [[point]] (apply + point)))
     (filter pos?)
     (apply min))

;; => 258


(defn update-distance [acc k path i]
  (if (first path)
    (update-in acc [(first path) k]
               (fn [x i]
                 (cond (nil? x) i
                       :else x))
               i)
    acc))

(defn crosses* [path1 path2]
  (loop [acc {}
         path1 path1
         path2 path2
         i 0]
    (if (or path1 path2)
      (recur (-> acc
                 (update-distance :dist1 path1 i)
                 (update-distance :dist2 path2 i))
             (next path1)
             (next path2)
             (inc i))
      acc)))


;; Part 2

(->> input
     (map points)
     (apply crosses*)
     (filter (fn [[point {:keys [dist1 dist2]}]]
               (and dist1 dist2)))
     (map (fn [[point {:keys [dist1 dist2]}]]
            (+ dist1 dist2)))
     (remove zero?)
     (apply min))

;; => 12304
