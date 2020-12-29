(ns adventofcode.2016.day21
  (:refer-clojure :exclude [reverse])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.combinatorics :refer [permutations]]))

(def password "abcdefgh")
(def input (-> "2016/day21.txt" io/resource io/reader line-seq))

(defn parse [d]
  (map #(str/split % #"\s") d))

(def instructions
  (parse input))

(defn rotate
  "Rotate string `s` `n` times in direction `dir` (\"left\" or \"right\")."
  [s n dir]
  (->> s
       cycle
       (drop (mod (if (= dir "left") n (- n)) (count s)))
       (take (count s))
       (apply str)))

(defn reverse
  "Reverse s from a to b"
  [s a b]
  (apply str
         (apply str (take a s))
         (str/reverse (subs s a (inc b)))
         (apply str (drop (inc b) s))))

(defn swap-pos
  "Swaps pos `a` with pos `b` of string `s`."
  [s a b]
  (apply str
         (map-indexed #(cond (= %1 a) (nth s b)
                             (= %1 b) (nth s a)
                             :else %2)
                      s)))

(defn swap-letter
  "Swap letter `a` with letter `b`."
  [s a b]
  (swap-pos s (.indexOf s a) (.indexOf s b)))

(defn rotate-on-pos
  "Rotate string `s` to the right based on position of letter `x` to the right
  once, plus the position of `x`. If `x`'s index is at least 4 rotate one more
  time."
  [s x]
  (as-> (.indexOf s x) n
    (if (>= n 4) (inc n) n)
    (inc n)
    (rotate s n "right")))

(defn move
  "Remove letter at index `a` in string `s` and insert it at index `b`."
  [s a b]
  (let [letter (.charAt s a)
        s (str (subs s 0 a) (subs s (inc a) (count s)))]
    (str (subs s 0 b) letter (subs s b))))

(defn execute [pw [cmd & args]]
  (case cmd
    "reverse" (reverse pw (Integer/parseInt (nth args 1)) (Integer/parseInt (nth args 3)))
    "rotate" (if (= (nth args 0) "based")
               (rotate-on-pos pw (nth args 5))
               (rotate pw (Integer/parseInt (nth args 1)) (nth args 0)))
    "swap" (if (= (nth args 0) "letter")
             (swap-letter pw (nth args 1) (nth args 4))
             (swap-pos pw (Integer/parseInt (nth args 1)) (Integer/parseInt (nth args 4))))
    "move" (move pw (Integer/parseInt (nth args 1)) (Integer/parseInt (nth args 4)))))

(reduce execute password instructions)
;; => "gfdhebac"

(->> (permutations password)
     (map (fn [chs] (apply str chs)))
     (map #(vector % (reduce execute % instructions)))
     (filter (fn [[_input pw]] (= pw "fbgdceah")))
     ffirst)
;; => "dhaegfbc"
