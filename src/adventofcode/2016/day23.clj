(ns adventofcode.2016.day23
  (:require [clojure.java.io :as io]))

(def input
  (-> "day23.txt" io/resource io/reader line-seq))

(defn parse-ints [s]
  (map #(try (Integer/parseInt %) (catch NumberFormatException e %)) s))

(defn parse [d]
  (mapv #(parse-ints (re-seq #"-*\w+" %)) d))

(def instructions (parse input))
(def length (count instructions))

(defn invalid? [[cmd n1 n2]]
  (or (and (= cmd "cpy") (number? n2))))

(defn toggle [instructions registers n1 pos]
  (let [ins-pos (+ (or (registers n1) n1) pos)]
    (if (<= 0 ins-pos (dec length))
      (let [[cmd n1 n2 :as ins] (instructions ins-pos)]
           (vec
            (concat
             (take ins-pos instructions)
             [(case cmd
                "cpy" ["jnz" n1 n2]
                "jnz" ["cpy" n1 n2]
                "inc" ["dec" n1]
                "dec" ["inc" n1]
                "tgl" ["inc" n1])]
             (drop (inc ins-pos) instructions))))
      instructions)))

(defn execute [instructions registers pos]
  (if (>= pos length) registers
      (let [[cmd n1 n2 :as ins] (instructions pos)]
        (if (invalid? ins)
          (recur instructions registers (inc pos))
          (case cmd
            "cpy" (recur instructions (assoc registers n2 (or (registers n1) n1)) (inc pos))
            "jnz" (let [val1 (or (registers n1) n1)
                        val2 (or (registers n2) n2)]
                    (recur instructions registers ((if (= val1 0) inc
                                                       (partial + val2)) pos)))
            "inc" (recur instructions (update registers n1 inc) (inc pos))
            "dec" (recur instructions (update registers n1 dec) (inc pos))
            "tgl" (recur (toggle instructions registers n1 pos) registers (inc pos)))))))

;; Part 1

((execute instructions {"a" 7 "b" 0 "c" 0 "d" 0} 0) "a")
;; => 10953


;; Part 2

((execute instructions {"a" 12 "b" 0 "c" 0 "d" 0} 0) "a")
;; => 479007513
