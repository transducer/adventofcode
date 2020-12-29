(ns adventofcode.2016.day25
  (:require [clojure.java.io :as io]))

(def input
  (-> "2016/day25.txt" io/resource io/reader line-seq))

(defn parse-ints [s]
  (map #(try (Integer/parseInt %) (catch NumberFormatException _ %)) s))

(defn parse [d]
  (mapv #(parse-ints (re-seq #"-*\w+" %)) d))

(def instructions
  (parse input))

(def length
  (count instructions))

(defn invalid?
  "Instruction is invalid when copying to a number instead of register."
  [[cmd _n1 n2]]
  (and (= cmd "cpy") (number? n2)))

(defn toggle
  "Toggle instruction changes the registers."
  [instructions registers n1 pos]
  (let [ins-pos (+ (or (registers n1) n1) pos)]
    (if (<= 0 ins-pos (dec length))
      (let [[cmd n1 n2 :as _ins] (instructions ins-pos)]
        (vec
         (concat
          (take ins-pos instructions)
          [(case cmd
             "cpy" ["jnz" n1 n2]
             "jnz" ["cpy" n1 n2]
             "inc" ["dec" n1]
             "dec" ["inc" n1]
             "out" ["inc" n1]
             "tgl" ["inc" n1])]
          (drop (inc ins-pos) instructions))))
      instructions)))

(defn execute
  "Executes instructions and stores output in register \"output\", returns
  output after `n` instructions executed."
  [instructions registers pos n i]
  (if (or (= n i)
          (>= pos length))
    registers
    (let [[cmd n1 n2 :as ins] (instructions pos)]
      (if (invalid? ins)
        (recur instructions registers (inc pos) n (inc i))
        (case cmd
          "cpy" (recur instructions (assoc registers n2 (or (registers n1) n1)) (inc pos) n (inc i))
          "jnz" (let [val1 (or (registers n1) n1)
                      val2 (or (registers n2) n2)]
                  (recur instructions registers ((if (= val1 0) inc
                                                     (partial + val2)) pos)
                         n (inc i)))
          "inc" (recur instructions (update registers n1 inc) (inc pos) n (inc i))
          "dec" (recur instructions (update registers n1 dec) (inc pos) n (inc i))
          "tgl" (recur (toggle instructions registers n1 pos) registers (inc pos) n (inc i))
          "out" (recur instructions (update registers "output" conj (or (registers n1) n1)) (inc pos) n (inc i)))))))

(def zeroes-and-ones
  (cycle [0 1]))

((->> (range)
      (pmap #(execute instructions {"a" % "b" 0 "c" 0  "d" 0 "output" [] "input" %} 0 100000 0))
      (filter #(= (take 10 zeroes-and-ones) (take 10 (% "output"))))
      first)
 "input")
;; => 180
