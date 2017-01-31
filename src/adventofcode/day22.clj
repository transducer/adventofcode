(ns adventofcode.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> "day22.txt" io/resource io/reader line-seq))

(defn parse-ints [d]
  (map #(Integer/parseInt %) d))

(defn parse [d]
  (->> (drop 2 d)
       (map (fn [l] (let [[x y size used avail use]
                          (parse-ints (drop 1 (re-matches #".*node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%" l)))]
                      {:x x
                       :y y
                       :size size
                       :used used
                       :avail avail
                       :use use})))))

(defn viable-pair? [a b]
  (and (not (zero? (:used a)))
       (not= a b)
       (<= (:used a) (:avail b))))

(def nodes (parse input))

(defn viable-pairs [nodes]
  (for [node1 nodes
        node2 nodes
        :when (viable-pair? node1 node2)]
    [node1 node2]))


;; Part 1

(count (viable-pairs nodes))

;; => 1034


(def goal-node {:x 37, :y 0})
(def target-node {:x 0, :y 0})

;; Part 2
