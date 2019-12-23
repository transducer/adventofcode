(ns adventofcode.2019.day13
  (:require [adventofcode.2019.day11 :refer [run-async]]
            [clojure.core.async :as async :refer [chan <!! >!! close! timeout alts!!]]
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


;; Part 1

(def out (chan 10000))
(def in (chan))

(run-async program in out)
(close! out)

(def outputs (<!! (async/into [] out)))

(->> (partition 3 outputs)
     (filter (fn [[_ _ type-id]] (= type-id 2)))
     count)

;; => 306


;; Part 2

(def out (chan 10000))
(def in (chan))

(run-async (assoc program 0 2) in out)

(defn setup []
  (q/background 0)
  (q/frame-rate 60)
  (q/stroke 255)
  (q/color-mode :hsb))

(def prev-ball (atom nil))
(def x-paddle (atom nil))

(defn update-state [_]
  (>!! in (compare (first @prev-ball) @x-paddle))
  (loop [outputs []]
    (let [[v _] (alts!! [out (timeout 10)])]
      (if v
        (recur (conj outputs v))
        outputs))))

(defn score? [x y]
  (and (= x -1) (= y 0)))

(defn draw-state [outputs]
  (let [C 10]
    (doseq [[x y tile-id] (partition 3 outputs)]
      (if (score? x y)
        (prn "Score:" tile-id)
        (condp = tile-id
          1 (q/rect (* C x) (+ (* C y)) C C)
          2 (do (q/stroke 128)
                (q/rect (* C x) (+ (* C y)) C C)
                (q/stroke 255))
          3 (do (reset! x-paddle x)
                (q/fill 0)
                (q/stroke 0)
                (q/rect 10 230 430 10)
                (q/stroke 255)
                (q/fill 255)
                (q/rect (* C x) (- (* C y) (/ C 2)) C (/ C 2)))
          4 (do
              (when-let [[x y] @prev-ball]
                (q/fill 0)
                (q/stroke 0)
                (q/rect (* C x) (+ (* C y)) (inc C) (inc C) (/ C 2))
                (q/stroke 255)
                (q/fill 255))
              (reset! prev-ball [x y])
              (q/rect (* C x) (+ (* C y)) C C (/ C 2)))
          :do-nothing)))))

(q/defsketch arcade
  :title "Intcode Arcade"
  :size [450 300]
  :setup setup
  :draw draw-state
  :update update-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])

;; => 15328
