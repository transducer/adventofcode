(ns adventofcode.2020.day25
  (:require
   [clojure.string :as string]))

(def public-keys
  (->> (slurp "resources/2020/day25.txt")
       string/split-lines
       (map read-string)))

(def door-public-key
  (first public-keys))

(def card-public-key
  (second public-keys))

(defn step [subject-number n]
  (rem (* subject-number n) 20201227))

(defn loop-size [subject-number public-key]
  (->> (iterate (partial step subject-number) 1)
       (keep-indexed (fn [cnt n] (when (= n public-key) cnt)))
       first))

(-> (iterate (partial step door-public-key) 1)
    (nth (loop-size 7 card-public-key)))
;; => 297257
