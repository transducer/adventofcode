(ns adventofcode.2016.day03
  (:require
   [clojure.string :as string]))

(def input
  (slurp "resources/2016/day03.txt"))

(defn parse
  [data]
  (->> data
       string/split-lines
       (sequence
        (comp
         (map #(string/trim %))
         (map #(string/split % #"\s+"))
         (map (fn [t] (map #(Integer/parseInt %) t)))))))

(defn valid-triangle?
  [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(->> input
     parse
     (filter valid-triangle?)
     count)
;; => 982

(defn to-triangle-cols
  [rows]
  (->> rows
       (apply mapv vector)
       (mapcat (partial partition 3))))

(->> input
     parse
     to-triangle-cols
     (filter valid-triangle?)
     count)
;; => 1826
