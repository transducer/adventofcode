(ns adventofcode.2018.day7
  (:require [clojure.java.io :as io]
            [clojure.set :refer [difference union]]
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


;; Part 2

(defn steps-in-progress [s]
  (->> (vals s)
       (map :step)
       (remove nil?)
       set))

(defn to-seconds [step]
  (- (int (first step)) 4))

(defn assign-worker [workers step]
  (let [free-worker (->> workers
                         (filter (fn [[w {:keys [seconds step]}]]
                                   (zero? seconds)))
                         ffirst)]
    (assoc workers free-worker {:seconds (dec (to-seconds step)) :step step})))

(defn dec-worker-time [workers]
  (->> workers
       (map (fn [[w {:keys [seconds step]}]]
              (if (zero? seconds)
                [w {:seconds 0 :step step}]
                [w {:seconds (dec seconds) :step step}])))
       (into {})))

(defn free-worker-count [workers]
  (->> workers
       (filter (fn [[w {:keys [seconds]}]] (zero? seconds)))
       count))

(def counter (atom 0))

(defn execute-finished-steps [step->deps workers]
  (let [finished-workers (->> workers
                              (filter (fn [[w {:keys [seconds step]}]]
                                        (and (zero? seconds) step))))
        finished-steps (->> finished-workers
                            (map (fn [[k {:keys [step]}]] step)))]
    (reduce
     (fn [s->d step]
       (execute s->d step))
     step->deps
     finished-steps)))

(defn find-and-do-steps-with-workers [step->deps steps-stack seconds workers]
  (let [step->deps (execute-finished-steps step->deps workers)]
    (if (empty? step->deps)
      seconds
      (if (seq steps-stack)
        (recur step->deps (pop steps-stack) seconds (assign-worker workers (peek steps-stack)))
        (let [steps (->> (keys step->deps)
                         (filter #(available? step->deps %))
                         (set)
                         (union (available-steps-in-deps step->deps))
                         (#(difference % (steps-in-progress workers)))
                         (into (sorted-set)))
              steps-now (vec (take (free-worker-count workers) steps))]
          (recur step->deps steps-now (inc seconds) (dec-worker-time workers)))))))

(def workers
  {:w1 {:seconds 0 :step nil}
   :w2 {:seconds 0 :step nil}
   :w3 {:seconds 0 :step nil}
   :w4 {:seconds 0 :step nil}
   :w5 {:seconds 0 :step nil}})

(find-and-do-steps-with-workers step->dependencies [] 0 workers)

;; => 1046
