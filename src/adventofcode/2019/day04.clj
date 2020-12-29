(ns adventofcode.2019.day04
  (:require
   [clojure.string :as string]))

(def input
  "109165-576723")

(defn parse-int [s]
  (Integer/parseInt s))

(def numbers
  (->> (string/split input #"-")
       (map parse-int)
       (apply range)))

(defn digits [num]
  (loop [n num
         ns ()]
    (if (zero? n)
      ns
      (recur (quot n 10)
             (conj ns (mod n 10))))))

(defn password? [n]
  (let [ns (digits n)
        parts (partition 2 1 ns)]
    (and (apply <= ns)
         (some (fn [[a b]] (= a b)) parts))))

(count (filter password? numbers))
;; => 2814

(defn password?* [n]
  (let [ns (digits n)]
    (and (apply <= ns)
         (->> ns
              (partition-by identity)
              (map count)
              (some (partial = 2))))))

(count (filter password?* numbers))
;; => 1991
