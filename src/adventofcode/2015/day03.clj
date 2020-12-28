(ns adventofcode.2015.day03)

(def moves
  (slurp "resources/2015/day03.txt"))

(defn houses [moves]
  (->> moves
       (reduce
        (fn [[seen [x y :as pos]] move]
          [(conj seen pos) (case move
                             \^ [x (inc y)]
                             \v [x (dec y)]
                             \> [(inc x) y]
                             \< [(dec x) y]
                             pos)])
        [#{} [0 0]])
       first))

(count (houses moves))
;; => 2572

(defn deinterleave [coll]
  (for [i [0 1]] (take-nth 2 (drop i coll))))

(->> moves
     deinterleave
     (mapcat houses)
     set
     count)
;; => 2631
