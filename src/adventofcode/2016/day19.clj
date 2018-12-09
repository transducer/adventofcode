(ns adventofcode.2016.day19)

(def num-elves 3005290)

;; Does not perform, but can be used to find the maths
#_(for [r (range 509 520)]
  (do
    (def num-elves r)

    (defn finished? [d]
      (= (count (remove zero? d)) 1))

    (defn double [d]
      (into [] (concat d d)))

    (defn find-winner [d pos]
      (cond (finished? d)   (reduced (inc (.indexOf d 1)))
            (zero? (d pos)) d
            :else           (assoc d (mod (+ (.indexOf (drop (inc pos) (double d)) 1) (inc pos)) num-elves)
                                   0)))


    (println ";;" num-elves (str (reduce find-winner (into [] (repeat num-elves 1)) (cycle (range num-elves)))))))


;; Starting at place 8 with 1 we have previous + 2 or start with 1 again if value exceeds index...


;; Part 1

(-> (reduce #(let [prev (second (peek %1))
                   next (+ prev 2)]
               (if (<= next %2)
                 (conj %1 [%2 next])
                 (conj %1 [%2 1])))
            [[8 1]]
            (range (inc num-elves)))
    peek
    second)


;; Part 2

(def p
  (loop [i 3]
    (if (< (* 3 i) num-elves)
      (recur (* 3 i))
      i)))

(if (<= num-elves (* 2 p))
  (- num-elves p)
  (let [r (mod num-elves p)]
    (if (zero? r)
      num-elves
      (+ r (- num-elves p)))))

;; => 1410967
