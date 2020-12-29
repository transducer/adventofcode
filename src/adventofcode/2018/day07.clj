(ns adventofcode.2018.day07
  (:require
   [clojure.java.io :as io]
   [clojure.set :refer [difference union]]
   [clojure.string :as string]))

(def step->dependencies
  (->> (io/resource "2018/day07.txt")
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

(defn steps-in-progress [s]
  (->> (vals s)
       (keep :step)
       set))

(defn to-seconds [step]
  (- (int (first step)) 4))

(defn assign-worker [workers step]
  (let [free-worker (->> workers
                         (filter (fn [[_w {:keys [seconds _step]}]]
                                   (zero? seconds)))
                         ffirst)]
    (assoc workers free-worker {:seconds (dec (to-seconds step)) :step step})))

(defn dec-worker-time [workers]
  (->> workers
       (map (fn [[w {:keys [seconds step]}]]
              [w {:seconds (max 0 (dec seconds)) :step step}]))
       (into {})))

(defn free-worker-count [workers]
  (->> workers
       (filter (fn [[_w {:keys [seconds]}]] (zero? seconds)))
       count))

(defn execute-finished-steps [step->deps workers]
  (let [finished-workers (->> workers
                              (filter (fn [[_w {:keys [seconds step]}]]
                                        (and (zero? seconds) step))))
        finished-steps (->> finished-workers
                            (map (fn [[_k {:keys [step]}]] step)))]
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
                         set
                         (union (available-steps-in-deps step->deps))
                         (#(difference % (steps-in-progress workers)))
                         (into (sorted-set))
                         (take (free-worker-count workers))
                         vec)]
          (recur step->deps steps (inc seconds) (dec-worker-time workers)))))))

(def workers
  {:w1 {:seconds 0 :step nil}
   :w2 {:seconds 0 :step nil}
   :w3 {:seconds 0 :step nil}
   :w4 {:seconds 0 :step nil}
   :w5 {:seconds 0 :step nil}})

(find-and-do-steps-with-workers step->dependencies [] 0 workers)
;; => 1046
