(ns adventofcode.2020.day11
  (:require
   [clojure.string :as string]))

(def input
  (string/split-lines (slurp "resources/2020/day11.txt")))

(def width
  (count (first input)))

(def height
  (count input))

(def grid
  (into {}
        (for [x (range width)
              y (range height)]
          [[x y] (nth (get input y) x)])))

(defn neighbours [[x y :as _point]]
  (for [[i j] [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]]]
    [(+ x i) (+ y j)]))

(defn new-state [state]
  (into {}
        (for [x (range width)
              y (range height)
              :let [curr (get state [x y])]
              :when curr
              :let [count-occupied (count (filter (fn [p] (= (get state p) \#)) (neighbours [x y])))]]
          [[x y]
           (cond (and (= curr \L) (zero? count-occupied)) \#
                 (and (= curr \#) (>= count-occupied 4)) \L
                 :else curr)])))

(defn first-duplicate [coll]
  (reduce
   (fn [seen item]
     (if (seen item)
       (reduced item)
       (conj seen item)))
   #{}
   coll))

(defn print-state [grid]
  (println)
  (dorun (map println (partition (dec width) (vals (sort-by first grid))))))

(->> (iterate new-state grid)
     first-duplicate
     vals
     (filter #{\#})
     count)
;; => 2448

(defn get-count-occupied [state [x y]]
  (apply +
         (for [[i j] [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]]
               :let [visible (first
                              (keep
                               (fn [scale]
                                 (let [p [(+ x (* scale i)) (+ y (* scale j))]]
                                   (case (get state p)
                                     \. nil
                                     \# 1
                                     \L 0
                                     nil 0)))
                               (range 1 height)))]]
           visible)))

(defn new-state2 [state]
  (into {}
        (for [x (range width)
              y (range height)
              :let [curr (get state [x y])]
              :when curr
              :let [count-occupied (get-count-occupied state [x y])]]
          [[x y]
           (cond (and (= curr \L) (zero? count-occupied)) \#
                 (and (= curr \#) (>= count-occupied 5)) \L
                 :else curr)])))

(->> (iterate new-state2 grid)
     first-duplicate
     vals
     (filter #{\#})
     count)
;; => 2234
