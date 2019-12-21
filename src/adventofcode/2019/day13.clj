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

(run-async (assoc program 0 2) in out)
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
  (q/color-mode :hsb)
  [])

(defn update-state [_]
  (loop [outputs []]
    (let [[v _] (alts!! [out (timeout 100)])]
      (if v
        (recur (conj outputs v))
        outputs))))

(defn score? [x y]
  (and (= x -1) (= y 0)))

(def prev-ball (atom nil))

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
          3 (do (q/fill 0)
                (q/rect 0 230 450 10)
                (q/fill 255)
                (q/rect (* C x) (- (* C y) (/ C 2)) C (/ C 2)))
          4 (do
              (when-let [[x y] @prev-ball]
                (q/fill 0)
                (q/rect (* C x) (+ (* C y)) C C (/ C 2))
                (q/fill 255))
              (reset! prev-ball [x y])
              (q/rect (* C x) (+ (* C y)) C C (/ C 2)))
          :do-nothing)))))

(defn key-pressed [_ {:keys [key]}]
  (>!! in (case key :left -1 :right 1 0)))

(q/defsketch arcade
  :title "Intcode Arcade"
  :size [450 300]
  :key-pressed key-pressed
  :setup setup
  :draw draw-state
  :update update-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
