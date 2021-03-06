(ns adventofcode.2016.day18
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def input
  (-> "2016/day18.txt" io/resource slurp string/trim-newline))

(def parse
  (partial mapv char))

(def width
  (count (parse input)))

(defn new-tile [previous-row idx]
  ;; Corner cases bit hacky
  (let [[left _center right] (concat (when (= idx 0) [\.])
                                     (subvec previous-row (max 0 (dec idx)) (min width (+ idx 2)))
                                     (when (= idx (dec width)) [\.]))]
    (if (not= left right) \^ \.)))

(defn new-row [previous]
  (mapv (partial new-tile previous) (range width)))

(defn count-safe-tiles [num-rows]
  (->> input
       parse
       (iterate new-row)
       (take num-rows)
       flatten
       (filter #{\.})
       count))

(count-safe-tiles 40) ; => 1939
(count-safe-tiles 400000) ; => 19999535
