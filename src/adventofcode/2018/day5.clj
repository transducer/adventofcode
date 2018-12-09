(ns adventofcode.2018.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input
  (-> "day5.txt" io/resource slurp string/trim-newline))


;; Part 1

(defn index-of-match
  "Returns index of first match, or -1 if not found."
  [re s]
  (let [matcher (.matcher re s)]
    (if (.find matcher)
      (.start matcher)
      -1)))

(def pattern
  (->> (range 65 91)
       (mapcat (fn [c] [(str (char (+ c 32)) (char c))
                        (str (char c) (char (+ c 32)))]))
       (interpose "|")
       (apply str)
       re-pattern))

(defn remove-polarity-at-index [s index]
  (str (subs s 0 index) (subs s (+ index 2) (count s))))

(defn length-after-reaction [polymer]
  (let [i (index-of-match pattern polymer)]
    (if (= i -1)
      (count polymer)
      (recur (remove-polarity-at-index polymer i)))))

(length-after-reaction input)


;; Part 2

(def patterns
  (->> (range 65 91)
       (map #(re-pattern (str "(?i)" (char %))))))

(->> (map #(string/replace input % "") patterns)
     (map length-after-reaction)
     (apply min))
