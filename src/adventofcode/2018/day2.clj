(ns adventofcode.2018.day2
  (:require [clojure.java.io :as io]))

(def input
  (-> "2018/day2.txt" io/resource io/reader line-seq))


;; Part 1

(->> (map frequencies input)
     (reduce (fn [[twos threes] e]
               (let [freqs (vals e)]
                 [((if (some #{2} freqs) inc identity) twos)
                  ((if (some #{3} freqs) inc identity) threes)]))
             [0 0])
     (apply *))


;; Part 2

(defn same-part [word1 word2]
  (->> (map (fn [c1 c2] (when (= c1 c2) c1)) word1 word2)
       (apply str)))

(-> (for [word1 input
          word2 input
          :when (= (count (same-part word1 word2))
                   (dec (count word1)))]
      (same-part word1 word2))
    first)
