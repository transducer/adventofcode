(ns adventofcode.2020.day05
  (:require
   [clojure.string :as string]))

(def input
  (string/split-lines (slurp "resources/2020/day05.txt")))

(defn seat-id [boarding-pass]
  (-> (string/escape boarding-pass {\F 0 \B 1 \L 0 \R 1})
      (Integer/parseInt 2)))

(def seat-ids
  (map seat-id input))


;; Part 1

(apply max seat-ids)

;; => 911


;; Part 2

(->> seat-ids
     sort
     (partition 2 1)
     (filter (fn [[a b]] (not= (inc a) b)))
     ffirst
     inc)

;; => 629
