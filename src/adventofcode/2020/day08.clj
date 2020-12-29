(ns adventofcode.2020.day08
  (:require
   [clojure.string :as string]))

(defn parse [line]
  (let [[op arg] (rest (re-find #"(\w+)\s([+-]\d+)" line))]
    [op (read-string arg)]))

(def instructions
  (->> (slurp "resources/2020/day08.txt")
       string/split-lines
       (mapv parse)))


;; Part 1

(defn evaluate [instructions]
  (loop [seen #{} i 0 acc 0]
    (cond (seen i) {:acc acc :final-value nil}
          (= i (dec (count instructions))) {:final-value acc}
          :else (let [[op arg] (nth instructions i)]
                  (recur (conj seen i)
                         (if (= op "jmp") (+ i arg) (inc i))
                         (if (= op "acc") (+ acc arg) acc))))))

(:acc (evaluate instructions))

;; => 1528


;; Part 2

(first
 (keep-indexed
  (fn [i [op arg]]
    (case op
      "nop" (-> instructions (assoc i ["jmp" arg]) evaluate :final-value)
      "jmp" (-> instructions (assoc i ["nop" arg]) evaluate :final-value)
      nil))
  instructions))

;; => 640
