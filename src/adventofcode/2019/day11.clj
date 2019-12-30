(ns adventofcode.2019.day11
  (:require [adventofcode.2019.intcode :refer [run-async]]
            [clojure.core.async :as async :refer [chan go-loop <! >!]]
            [clojure.java.io :as io]))

(def program
  (slurp (io/resource "2019/day11.txt")))

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
