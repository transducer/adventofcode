(ns adventofcode.2020.day16
  (:require
   [clojure.set :as set]
   [clojure.string :as string]))

(def input
  (slurp "resources/2020/day16.txt"))

(def your-ticket
  (read-string (format "[%s]" (second (re-find #"your ticket:\n(.*)" input)))))

(def nearby-ticket-values
  (->> (string/replace input "\n" ",")
       (re-find #"nearby tickets:,(.*)")
       second
       (format "[%s]")
       read-string))

(def ranges
  (re-seq #"(\d+)-(\d+)" input))

(def in-ranges
  (reduce
   (fn [acc [_ n1 n2]]
     (set/union acc (set (range (Integer. n1) (inc (Integer. n2))))))
   #{}
   ranges))

(defn valid? [n]
  (contains? in-ranges n))

(apply + (remove valid? nearby-ticket-values))
;; => 23044

(def fields
  (map
   (fn [line]
     (->> (re-seq #"(\d+)-(\d+)" line)
          (reduce
           (fn [acc [_ n1 n2]]
             (set/union acc (set (range (Integer. n1) (inc (Integer. n2))))))
           #{})))
   (take 20 (string/split-lines input))))

(def transpose
  (partial apply map vector))

(def nearby-ticket-cols
  (->> (string/split input #"nearby tickets:\n")
       second
       string/split-lines
       (keep
        (fn [line]
          (let [ns (filter valid? (read-string (format "[%s]" line)))]
            (when (= (count ns) 20)
              ns))))
       transpose))

(defn possible-indexes [col]
  (keep-indexed
   (fn [index field]
     (when (set/subset? (set col) field)
       index))
   fields))

(def indexes
  (loop [possible (map (comp set possible-indexes) nearby-ticket-cols)
         acc #{}]
    (if (= (count acc) 6)
      acc
      (let [[i item] (first (keep-indexed #(when (= (count %2) 1) [%1 (first %2)]) possible))]
        (recur (map #(disj % item) possible)
               (if (contains? #{0 1 2 3 4 5} item)
                 (conj acc i)
                 acc))))))

(apply * (map your-ticket indexes))
;; => 3765150732757
