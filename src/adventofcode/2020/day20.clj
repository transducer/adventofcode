(ns adventofcode.2020.day20
  (:require
   [clojure.set :as set]
   [clojure.string :as string]))

(def input
  (->> (string/split (slurp "resources/2020/day20.txt") #"Tile ")
       rest
       (map
        (fn [part]
          (let [[n img] (string/split part #":\n")]
            [(Integer. n) (string/split-lines img)])))))

(defn flip [img]
  (vec (reverse img)))

(defn rotate [img]
  (map (partial apply str) (apply mapv vector img)))

(defn arrangements [img]
  [(-> img)                              (-> img flip rotate flip)
   (-> img flip rotate flip rotate)      (-> img flip rotate flip rotate flip rotate flip)
   (-> img flip)                         (-> img flip rotate)
   (-> img flip rotate flip rotate flip) (-> img flip rotate flip rotate flip rotate)])

(def images->arrangements
  (map (fn [[n img]] [n (arrangements img)]) input))

(defn possible-sides [imgs]
  (->> imgs
       (mapv (comp #(Long/parseLong % 2) #(string/escape % {\. 0 \# 1}) first))))

(def images->sides
  (map (fn [[n imgs]] [n (possible-sides imgs)]) images->arrangements))

(defn vec-remove [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(def images->count-side-matches
  (map-indexed
   (fn [index [n sides]]
     [n (count (set/intersection
                (apply set/union (map set (vec-remove (mapv second images->sides) index)))
                (set sides)))])
   images->sides))

(->> images->count-side-matches
     (sort-by second)
     (take 4)
     (map first)
     (apply *))
;; => 17250897231301

(defn drop-corners [img]
  (map #(rest (butlast %)) (butlast (rest img))))

(def total-hashes
  (->> images->arrangements
       (mapcat (comp drop-corners first second))
       (apply str)
       (filter #{\#})
       count))

(def char-count-sea-monster
  15)

(def sea-monster-count-guess
  23)

(- total-hashes (* char-count-sea-monster sea-monster-count-guess))
;; => 1576
