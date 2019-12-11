(ns adventofcode.2019.day11
  (:require [adventofcode.2019.day4 :refer [digits]]
            [clojure.core.async :as async :refer [chan go-loop <! >!]]
            [clojure.java.io :as io]))

(def program
  (->> (io/resource "2019/day11.txt")
       (slurp)
       (format "[%s]")
       (read-string)
       (map-indexed vector)
       (into {})))

(defn read-param [p mode base value]
  (condp = mode
    1 value
    2 (get p (+ base value) 0)
    (get p value 0)))

(defn write-param [mode base value]
  (if (= mode 2)
    (+ base value)
    value))

(defn run-async [program in out]
  (go-loop [p program
            base 0
            i 0]
    (let [ins (digits (get p i))
          op (take-last 2 (cons 0 ins))
          [a_mode b_mode c_mode] (reverse (drop-last 2 ins))
          a' (get p (inc i))
          b' (get p (+ i 2))
          c' (get p (+ i 3))
          a (read-param p a_mode base a')
          a_write (write-param a_mode base a')
          b (read-param p b_mode base b')
          c (write-param c_mode base c')]
      (condp = op
        [0 1] (recur (assoc p c (+ a b)) base (+ i 4))
        [0 2] (recur (assoc p c (* a b)) base (+ i 4))
        [0 3] (recur (assoc p a_write (<! in)) base (+ i 2))
        [0 4] (do (>! out a) (recur p base (+ i 2)))
        [0 5] (recur p base (if (zero? a) (+ i 3) b))
        [0 6] (recur p base (if (zero? a) b (+ i 3)))
        [0 7] (recur (assoc p c (if (< a b) 1 0)) base (+ i 4))
        [0 8] (recur (assoc p c (if (= a b) 1 0)) base (+ i 4))
        [0 9] (recur p (+ base a) (+ i 2))
        [9 9] out))))

(def directions
  {:up [0 1]
   :right [1 0]
   :down [0 -1]
   :left [-1 0]})

(defn left [point dir]
  (let [dirs (keys directions)
        idx (dec (.indexOf dirs dir))
        new-dir (nth dirs (if (neg? idx) (dec (count dirs)) idx))
        new-point (mapv + point (get directions new-dir))]
    [new-point new-dir]))

(defn right [point dir]
  (let [dirs (keys directions)
        idx (inc (.indexOf dirs dir))
        new-dir (nth dirs (if (= idx (count dirs)) 0 idx))
        new-point (mapv + point (get directions new-dir))]
    [new-point new-dir]))


;; Part 1

(def grid (atom nil))

(defn start-robot [initial-grid]
  (let [in (chan)
        out (chan)]
    (run-async program in out)
    (go-loop [visited initial-grid, point [0 0], dir :up]
      (let [color (get visited point :black)
            _ (>! in (if (= color :black) 0 1))
            new-color (if (zero? (<! out)) :black :white)
            [new-point new-dir] (if (zero? (<! out)) (left point dir) (right point dir))]
        (reset! grid visited)
        (recur (assoc visited point new-color) new-point new-dir)))))

(start-robot {})
(count @grid)

;; => 2054


;; Part 2

(start-robot {[0 0] :white})

(defn print-grid [grid]
  (let [width 100]
    (dorun
     (map println
          (reverse
           (partition
            width
            (for [i (range (* width width))
                  :let [x (- (mod i width) (int (/ width 2)))
                        y (- (int (/ i width)) (int (/ width 2)))]]
              (if (= (get grid [x y]) :white) \# \.))))))))

(print-grid @grid)

;; # . . # . # # # . . # # # # . # # # # . . # # . . . . # # . # . . # . # # # .
;; # . # . . # . . # . . . . # . # . . . . # . . # . . . . # . # . . # . # . . #
;; # # . . . # . . # . . . # . . # # # . . # . . # . . . . # . # # # # . # # # .
;; # . # . . # # # . . . # . . . # . . . . # # # # . . . . # . # . . # . # . . #
;; # . # . . # . # . . # . . . . # . . . . # . . # . # . . # . # . . # . # . . #
;; # . . # . # . . # . # # # # . # # # # . # . . # . . # # . . # . . # . # # # .

;; => KRZEAJHB
