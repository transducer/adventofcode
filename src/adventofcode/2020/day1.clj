(ns adventofcode.2020.day1)

(def input
  (->> "resources/2020/day1.txt"
       slurp
       (format "[%s]")
       read-string))

(first
 (for [a input
       b input
       :when (#{2020} (+ a b))]
   (* a b)))

;; => 1010884


;; Part 2

(first
 (for [a input
       b input
       :while (<= (+ a b) 2020)
       c input
       :when (= (+ a b c) 2020)]
   (* a b c)))

;; => 253928438
