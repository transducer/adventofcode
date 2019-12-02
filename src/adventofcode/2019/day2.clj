(ns adventofcode.2019.day2
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "2019/day2.txt")
       slurp
       (format "[%s]")
       read-string))

(def opcodes
  {1 (fn add [v a b c] (assoc v c (+ (v a) (v b))))
   2 (fn mult [v a b c] (assoc v c (* (v a) (v b))))
   99 (fn exit [v _a _b _c] (first v))})


(defn run [in noun verb]
  (loop [v (-> in
               (assoc 1 noun)
               (assoc 2 verb))
         i 0]
    (if (vector? v)
      (let [[a b c] (subvec v (inc i))]
        (recur ((opcodes (v i)) v a b c)
               (+ i 4)))
      v)))


;; Part 1

(run input 12 2)

;; =>  2692315


;; Part 2

(def wanted 19690720)

(->> (for [noun (range 100)
           verb (range 100)]
       {:result (+ (* 100 noun) verb)
        :verb verb
        :noun noun
        :output (run input noun verb)})
     (filter (fn [{:keys [output]}] (= output wanted)))
     first
     :result)

;; => 9507
