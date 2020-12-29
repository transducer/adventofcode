(ns adventofcode.2019.day02
  (:require
   [clojure.java.io :as io]))

(def input
  (->> (io/resource "2019/day02.txt")
       slurp
       (format "[%s]")
       read-string))

(def opcodes
  {1 (fn add [v a b c] (assoc v c (+ (v a) (v b))))
   2 (fn mult [v a b c] (assoc v c (* (v a) (v b))))
   99 (fn exit [v _a _b _c] (first v))})

(defn run [in noun verb]
  (loop [v (assoc in 1 noun 2 verb)
         i 0]
    (if (vector? v)
      (let [[a b c] (subvec v (inc i))]
        (recur ((opcodes (v i)) v a b c)
               (+ i 4)))
      v)))

(run input 12 2)
;; =>  2692315

(def wanted
  19690720)

(first
 (for [noun (range 100)
       verb (range 100)
       :when (= (run input noun verb) wanted)]
   (+ (* 100 noun) verb)))
;; => 9507
