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


;;; Visualization

(require '[quil.core :as q] '[quil.middleware :as m])

(def C 5)

(defn draw [state]
  (doseq [x (range width)
          y (range height)
          :let [value (get state [x y])]]
    (condp = value
      \. (do (q/stroke 255)
             (q/fill 255)
             (q/rect (* C x) (+ (* C y)) C C))
      \# (do (q/fill 128)
             (q/stroke 128)
             (q/rect (* C x) (+ (* C y)) C C))
      \L (do (q/fill 0)
             (q/stroke 0)
             (q/rect (* C x) (- (* C y) (/ C 2)) C (/ C 2)))
      :do-nothing)))

(defn setup []
  (q/background 0)
  (q/frame-rate 240)
  (q/stroke 255)
  (q/color-mode :hsb)
  grid)

(q/defsketch seats
  :title "Seats"
  :size [(* C width) (* C height)]
  :setup setup
  :draw draw
  :update new-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
