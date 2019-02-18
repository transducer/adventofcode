(ns adventofcode.2019.day9)

(def players 459)
(def last-marble 71320)

(def players 10)
(def last-marble 1618)
;; 8317


;; Part 1

(loop [marble-count 0
       current-marble 0
       marbles (make-array Integer/TYPE last-marble)
       scores (make-array Integer/TYPE players)]
  (if (= marble last-marble)
    (->> scores
         (map-indexed vector)
         (apply max-key second)
         first)
    (recur (inc marble-count)
           (mod (inc current-marble) marble-count)
           (aset scores 458 0)
           (aget scores 458))
    ))

;; =>
