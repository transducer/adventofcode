(ns adventofcode.2019.day21
  (:require [adventofcode.2019.intcode :refer [run]]
            [clojure.string :as string]))

(def program
  (slurp "resources/2019/day21.txt"))


;; Part 1

(defn parse [script]
  (->> (str (string/join "\n" script) "\n")
       (map int)))

;; (!A | !B | !C) & D
(def script
  ["NOT C J"
   "AND D J"
   "NOT A T"
   "OR T J"
   "WALK"])

(peek (apply run program (parse script)))

;; => 19357390


;; Part 2

;; (!A | !B | !C) & D & (H | E)
(def script
  ["NOT A J"
   "NOT B T"
   "OR T J"
   "NOT C T"
   "OR T J"
   "AND D J"
   "NOT H T"
   "NOT T T"
   "OR E T"
   "AND T J"
   "RUN"])

(peek (apply run program (parse script)))

;; => 1142844041
