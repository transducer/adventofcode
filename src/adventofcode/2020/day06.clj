(ns adventofcode.2020.day06
  (:require
   [clojure.set :as set]
   [clojure.string :as string]))

(def input
  (string/split (slurp "resources/2020/day06.txt") #"\R\R"))


;; Part 1

(->> input
     (map (fn [s] (count (set (apply str (string/split s #"\r\n"))))))
     (apply +))

;; => 6625


;; Part 2

(->> input
     (map (fn [s] (count (apply set/intersection (map set (string/split s #"\r\n"))))))
     (apply +))

;; => 3360
