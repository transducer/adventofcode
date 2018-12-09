(ns adventofcode.2018.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def step->dependencies
  (->> (io/resource "2018/day7.txt")
       slurp
       string/split-lines
       (map #(re-seq #"[A-Z]" (string/replace-first % #"S" "")))
       (group-by second)
       (map (fn [[k v]] [k (map first v)]))
       (into (sorted-map))))

(defn available? [step step->dependencies]
  )

;; Check available, do step
