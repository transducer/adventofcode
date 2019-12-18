(ns adventofcode.2019.day13
  (:require [adventofcode.2019.day11 :refer [run-async]]
            [clojure.core.async :as async :refer [chan <!! close!]]
            [clojure.java.io :as io]
            [quil.core :as q]
            [quil.middleware :as m]))

(def program
  (->> (io/resource "2019/day13.txt")
       (slurp)
       (format "[%s]")
       (read-string)
       (map-indexed vector)
       (into {})))

(def out (chan 10000))
(def in (chan))

(run-async (assoc program 0 2) in out)
(close! out)

(def outputs (<!! (async/into [] out)))

(spit (io/resource "2019/day13_out.edn") outputs)

(count outputs)

(defn setup []
  (q/background 0)
  (q/frame-rate 6000)
  (q/stroke 255)
  (q/color-mode :hsb)
  outputs)

(defn update-state [outputs]
  ;; if outputs is empty
  ;; http://quil.info/api/input/keyboard
  ;; Then take a key press. Either left (-1), right (+1) or neutral (0)
  ;; And get enough outputs to fill the screen
  ;; (count squares) = 3510... Do not count score printing outputs x = -1
  ;; Otherwise if there are outputs
  (drop 3 outputs))

(defn score? [x y]
  (and (= x -1) (= y 0)))

(defn draw-state [outputs]
  (let [C 10]
    (if-let [[x y tile-id] outputs]
      (if (score? x y)
        (prn "Score:" tile-id)
        (condp = tile-id
          1 (q/rect (* C x) (+ (* C y)) C C)
          2 (do (q/stroke 128) (q/rect (* C x) (+ (* C y)) C C) (q/stroke 255))
          3 (q/rect (* C x) (+ (* C y) (/ C 2)) C (/ C 2))
          4 (q/rect (* C x) (+ (* C y)) C C (/ C 2))
          :do-nothing))
      (q/no-loop))))

(q/defsketch arcade
  :title "Intcode Arcade"
  :size [450 300]
  :setup setup
  :draw draw-state
  :update update-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
