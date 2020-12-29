(ns adventofcode.2020.day01)

(def input
  (->> "resources/2020/day01.txt"
       slurp
       (format "[%s]")
       read-string
       sort))

(first
 (for [a input
       b input
       :when (#{2020} (+ a b))]
   (* a b)))
;; => 1010884

(first
 (for [a input
       b input
       :while (<= (+ a b) 2020)
       c input
       :when (= (+ a b c) 2020)]
   (* a b c)))
;; => 253928438
