(ns adventofcode.2018.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def step->dependencies
  (->> (io/resource "2018/day7.txt")
       slurp
       string/split-lines
       (map #(re-seq #"[A-Z]" (string/replace-first % #"S" "")))
       (group-by second)
       (map (fn [[k v]] [k (into (sorted-set) (map first v))]))
       (into (sorted-map))))

(defn available? [step->dependencies step]
  (empty? (step->dependencies step)))

(defn first-step [step->dependencies]
  (->> (keys step->dependencies)
       (apply disj (apply clojure.set/union (vals step->dependencies)))
       first))

(defn execute [step->deps step]
  (dissoc
   (->> step->deps
        (map (fn [[s deps]] [s (disj deps step)]))
        (into (sorted-map)))
   step))


;; Part 1

(def start (first-step step->dependencies))

(loop [step->deps (execute step->dependencies start)
       acc start]
  (if (empty? step->deps)
    acc
    (if-let [step (->> step->deps
                    keys
                    (filter #(available? step->deps %))
                    first)]
      (recur (execute step->deps step)
             (str acc step))
      acc)))
