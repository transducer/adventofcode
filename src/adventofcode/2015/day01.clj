(ns adventofcode.2015.day01)

(def input
  (slurp "resources/2015/day01.txt"))

(->> input
     frequencies
     (take 2)
     vals
     (apply -))
;; => 280

(reduce
 (fn [[pos level] c]
   (cond (= level -1)
         (reduced pos)
         (= c \()
         [(inc pos) (inc level)]
         :else [(inc pos) (dec level)]))
 [0 0]
 input)
;; => 1797
