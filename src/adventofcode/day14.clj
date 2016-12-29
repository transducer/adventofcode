(ns adventofcode.day14
  (:require [digest :refer [md5]]))

(def input "yjdafjpo")

(defn hashes
  "Lazy seq of MD5 applied n times on combination of salt and index.
  Default n is 1."
  ([salt]
   (hashes 1 salt))
  ([n salt]
   (m-hashes n salt)))

(defn first-three-in-a-row
  "Returns [match letter]."
  [s]
  (first (re-seq #"(.)\1\1" s)))

(defn five-in-a-row?
  "Does coll contain a string with the letter five times in a row?"
  [letter coll]
  (boolean
   (some #(re-seq (re-pattern (apply str (repeat 5 letter))) %)
         coll)))

(defn index-and-pad-key?
  "Returns lazy seq of [index pad-key?]"
  [hashes]
  (map-indexed #(if-let [[match letter] (first-three-in-a-row %2)]
                  (let [next-1000 (->> hashes
                                       (drop (inc %1))
                                       (take 1000))]
                    [%1 (five-in-a-row? letter next-1000)])
                  [%1 false])
               hashes))


;; Part 1

(->> input
     hashes
     pad-keys
     (filter #(second %))
     (take 64)
     last
     first)

;; => 25427


;; Part 2

(->> input
     (hashes 2017)
     pad-keys
     (filter #(second %))
     (take 64)
     last
     first)

;; => 22045
