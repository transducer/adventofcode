(ns adventofcode.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> "day18.txt" io/resource slurp str/trim-newline))
(def parse (partial mapv char))
(def width (count (parse input)))

(defn new-tile [previous-row idx]
  ;; Corner cases bit hacky
  (let [[left center right] (concat (when (= idx 0) [\.])
                                    (subvec previous-row (max 0 (dec idx)) (min width (+ idx 2)))
                                    (when (= idx (dec width)) [\.]))]
    (if (not= left right) \^ \.)))

(defn new-row [previous]
  (mapv (partial new-tile previous) (range width)))


;; Part 1

(->> input
     parse
     (iterate new-row)
     (take 40)
     flatten
     (filter #{\.})
     count)

;; => 1939


;; Part 2

(->> input
     parse
     (iterate new-row)
     (take 400000)
     flatten
     (filter #{\.})
     count)

;; => 19999535
