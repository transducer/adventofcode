(ns adventofcode.2020.day1
  (:require
   [clojure.math.combinatorics :refer [combinations]]))

(def input
  (->> "resources/2020/day1.txt"
       slurp
       (format "[%s]")
       read-string))

(defn product-entries [n]
  (->> (combinations input n)
       (filter (comp #{2020} (partial apply +)))
       first
       (apply *)))


;; Part 1

(product-entries 2)

;; => 1010884


;; Part 2

(product-entries 3)

;; => 253928438
