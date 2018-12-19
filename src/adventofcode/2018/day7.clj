(ns adventofcode.2018.day7
  (:require [clojure.java.io :as io]
            [clojure.set :refer [union]]
            [clojure.string :as string]))

(def step->dependencies
  (->> (io/resource "2018/day7.txt")
       slurp
       string/split-lines
       (map #(re-seq #"[A-Z]" (string/replace-first % #"S" "")))
       (group-by second)
       (map (fn [[k v]] [k (into #{} (map first v))]))
       (into {})))

(defn available? [step->dependencies step]
  (empty? (step->dependencies step)))

(defn available-steps-in-deps [step->dependencies]
  (->> (keys step->dependencies)
       (apply disj (apply union (vals step->dependencies)))))

(defn execute [step->deps step]
  (->> step->deps
       (map (fn [[st deps]] [st (disj deps step)]))
       (into {})
       (#(dissoc % step))))


;; Part 1

(defn find-and-do-steps [step->deps acc]
  (if (empty? step->deps)
    acc
    (when-let [steps (->> (keys step->deps)
                          (filter #(available? step->deps %))
                          (union (available-steps-in-deps step->deps))
                          (into (sorted-set)))]
      (for [step steps]
        (find-and-do-steps (execute step->deps step)
                           (str acc step))))))

(first (flatten (find-and-do-steps step->dependencies "")))

;; => CGKMUWXFAIHSYDNLJQTREOPZBV
