(ns adventofcode.2018.day04
  (:require
   [adventofcode.2018.day03 :refer [inc-indices]]
   [clojure.java.io :as io]))

(def input
  (-> "2018/day04.txt" io/resource io/reader line-seq))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse [input]
  (->> (sort input)
       (map #(zipmap [:YYYY :MM :DD :hh :mm :type :guard-number]
                     (let [[timestamp-data type-data]
                           (split-at 5
                                     (rest (re-find #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (\w+) .(\w+)" %)))]
                       (concat (map parse-int timestamp-data) type-data))))
       (partition-by #(= (:type %) "Guard"))
       (partition 2)
       (map flatten)))

(def guard->sleep-mask
  (->> (parse input)
       (reduce
        (fn [acc [{:keys [guard-number]} & sleep-data]]
          (let [sleep-periods (partition 2 (keep :mm sleep-data))
                sleep-mask (reduce
                            (fn [mask [start stop]]
                              (inc-indices mask (range start stop)))
                            (vec (repeat 60 0)) ; "bit mask" for sleeping minutes in hour
                            sleep-periods)]
            (conj acc {(parse-int guard-number) sleep-mask})))
        [])
       (apply merge-with #(map + %1 %2))))

(let [[guard-number sleep-mask]
      (first
       (into
        (sorted-map-by (fn [k1 k2]
                         (compare
                          (apply + (get guard->sleep-mask k2))
                          (apply + (get guard->sleep-mask k1)))))
        guard->sleep-mask))]
  (* guard-number (.indexOf sleep-mask (apply max sleep-mask))))
;; => 73646

(let [[guard-number sleep-mask]
      (first
       (into
        (sorted-map-by (fn [k1 k2]
                         (compare
                          (apply max (get guard->sleep-mask k2))
                          (apply max (get guard->sleep-mask k1)))))
        guard->sleep-mask))]
  (* guard-number (.indexOf sleep-mask (apply max sleep-mask))))
;; => 4727
