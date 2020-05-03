(ns adventofcode.2019.day22
  (:require [clojure.java.io :as io]))


;; Part 1

(def factory-order-deck
  (vec (range 10007)))

(defn deal-into-new-stack [deck]
  (vec (rseq deck)))

(defn cut [deck n]
  (if (nat-int? n)
    (into (subvec deck n) (subvec deck 0 n))
    (into (subvec deck (+ (count deck) n)) (subvec deck 0 (+ (count deck) n)))))

(defn deal [deck increment]
  (loop [space (vec (repeat (count deck) 0))
         deck-pos 0
         space-pos 0
         searching-open-pos? false]
    (cond (every? (complement zero?) (subvec space 1))
          space
          (zero? (space space-pos))
          (recur (assoc space space-pos (deck deck-pos))
                 (if searching-open-pos?
                   deck-pos
                   (mod (inc deck-pos) (count deck)))
                 (mod (+ space-pos increment) (count deck))
                 false)
          :else
          (recur space
                 deck-pos
                 (mod (inc space-pos) (count space))
                 true))))

(.indexOf
 (reduce
  (fn [deck line]
    (let [op (re-find #"cut|new|increment" line)
          n (re-find #"-*\d+" line)]
      (case op
        "cut" (cut deck (Integer/parseInt n))
        "new" (deal-into-new-stack deck)
        "increment" (deal deck (Integer/parseInt n)))))
  factory-order-deck
  (line-seq (io/reader "resources/2019/day22.txt")))
 2019)

;; => 4096


;; Part 2
