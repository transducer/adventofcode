(ns adventofcode.2019.day25
  (:require [adventofcode.2019.intcode :refer [run-async]]
            [clojure.core.async :refer [chan alts!! timeout onto-chan]]))

(def program
  (slurp "resources/2019/day25.txt"))

(def in (chan))
(def out (chan))

(run-async program in out)

(loop [received []]
  (let [[output _] (alts!! [(timeout 20) out])]
    (if output
      (recur (conj received output))
      (do
        (printf "%s>" (apply str (map char received)))
        (onto-chan in (map int (str (read-line) \newline)) false)
        (recur [])))))

;; => 134807554
