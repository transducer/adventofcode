(ns adventofcode.2016.day02
  (:require
   [clojure.string :as string]))

(def input
  (slurp "resources/2016/day02.txt"))

(defn parse
  [data]
  (string/split-lines data))

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

(defn segment-with-pos
  [segments pos]
  (->> segments
       (filter (fn [s] (some #(= % pos) s)))
       first))

(defn change-pos
  [keypad move pos]
  (let [segment (segment-with-pos ({\U (cols keypad)
                                    \D (cols keypad)
                                    \R (rows keypad)
                                    \L (rows keypad)} move) pos)]
    (({\U up \L left \R right \D down} move) segment pos)))

(defn new-pos
  [keypad instruction pos]
  (reduce #(change-pos keypad %2 %1) pos instruction))

(defn code
  [keypad instructions]
  (->> instructions
       (reduce (fn [[pos code] instruction]
                 (let [pos (new-pos keypad instruction pos)]
                   [pos (str code pos)]))
               [5 ""])
       second))

(def keypad
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(->> input
     parse
     (code keypad))
;; => "78293"

(def keypad2
  [[nil nil 1  nil nil]
   [nil 2   3  4   nil]
   [5   6   7  8   9  ]
   [nil 'A  'B 'C   nil]
   [nil nil 'D nil nil]])

(->> input
     parse
     (code keypad2))
;; => "AC8C8"
