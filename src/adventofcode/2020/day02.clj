(ns adventofcode.2020.day02
  (:require
   [clojure.java.io :as io]))

(def input
  (-> "2020/day02.txt" io/resource io/reader line-seq))

(defn parse [s]
  (let [[_ low high [c] pw] (re-find #"(\d+)-(\d+)\s(\D):\s(\w+)" s)]
    {:low (Integer. low) :high (Integer. high) :c c :pw pw}))

(defn valid? [{:keys [low high c pw]}]
  (<= low (get (frequencies pw) c 0) high))


;; Part 1

(count (filter (comp valid? parse) input))

;; => 572


;; Part 2

(defn valid2? [{:keys [low high c pw]}]
  (->> (map (vec pw) [(dec low) (dec high)])
       (filter #{c})
       count
       (= 1)))

(count (filter (comp valid2? parse) input))

;; => 306
