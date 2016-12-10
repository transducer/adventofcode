(ns adventofcode.day2
  (:require [clojure.string :as str]))

(def input
  (slurp "resources/day2.txt"))

(defn parse
  [data]
  (str/split-lines data))

(defn rows
  [keypad]
  keypad)

(defn cols
  [keypad]
  (apply mapv vector (rows keypad)))

(defn up
  [col pos]
  (as-> (.indexOf col pos) idx
    (max (dec idx) 0)
    (or (col idx) pos)))

(defn down
  [col pos]
  (as-> (.indexOf col pos) idx
    (min (inc idx) (dec (count col)))
    (or (col idx) pos)))

(defn right
  [row pos]
  (as-> (.indexOf row pos) idx
    (min (inc idx) (dec (count row)))
    (or (row idx) pos)))

(defn left
  [row pos]
  (as-> (.indexOf row pos) idx
    (max (dec idx) 0)
    (or (row idx) pos)))

(defn get-segment-with-pos
  [segments pos]
  (->> segments
       (filter (fn [s] (some #(= % pos) s)))
       first))

(defn change-pos
  [keypad move pos]
  (let [segment (get-segment-with-pos ({\U (cols keypad)
                                        \D (cols keypad)
                                        \R (rows keypad)
                                        \L (rows keypad)} move) pos)]
    (({\U up \L left \R right \D down} move) segment pos)))

(defn new-pos
  [keypad instruction pos]
  (reduce #(change-pos keypad %2 %1) pos instruction))

(defn get-code
  [keypad instructions]
  (->> instructions
       (reduce (fn [[pos code] instruction]
                 (let [pos (new-pos keypad instruction pos)]
                   [pos (str code pos)]))
               [5 ""])
       second))


;; Part 1

(def keypad
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(->> input
     parse
     (get-code keypad))


;; Part 2

(def keypad
  [[nil nil 1  nil nil]
   [nil 2   3  4   nil]
   [5   6   7  8   9  ]
   [nil 'A  'B 'C   nil]
   [nil nil 'D nil nil]])

(->> input
     parse
     (get-code keypad))
