(ns adventofcode.day12
  (:require [clojure.java.io :as io]))

(def input
  (-> "day12.txt" io/resource io/reader line-seq))

(defn parse-ints [s]
  (map #(try (Integer/parseInt %) (catch NumberFormatException e %)) s))

(defn parse [d]
  (mapv #(parse-ints (re-seq #"-*\w+" %)) d))

(def instructions (parse input))
(def length (count instructions))

(defn execute [registers pos]
  (if (>= pos length) registers
      (let [[cmd n1 n2] (instructions pos)]
        (case cmd
          "cpy" (recur (assoc registers n2 (or (registers n1) n1)) (inc pos))
          "jnz" (let [val (or (registers n1) n1)]
                  (recur registers ((cond (> val 0) +
                                          (< val 0) -
                                          :else     inc) pos)))
          "inc" (recur (update registers n1 inc) (inc pos))
          "dec" (recur (update registers n1 dec) (inc pos))))))


;; Part 1

((execute {"a" 0 "b" 0 "c" 0 "d" 0} 0) "a")
;; => 318009


;; Part 2

((execute {"a" 0 "b" 0 "c" 1 "d" 0} 0) "a")
;; => 9227663
