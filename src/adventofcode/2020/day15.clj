(ns adventofcode.2020.day15)

(def input
  (read-string (format "[%s]" (slurp "resources/2020/day15.txt"))))

(defn nth-number-spoken [n]
  (loop [seen (into {} (map-indexed (fn [i item] [item [(inc i)]]) input))
         last-spoken (last input)
         i (inc (count input))]
    (if (= i (inc n))
      last-spoken
      (let [indexes-last-turn-number (get seen last-spoken)
            new-number (if (>= (count indexes-last-turn-number) 2)
                         (->> (- (count indexes-last-turn-number) 2)
                              (subvec indexes-last-turn-number)
                              reverse
                              (apply -))
                         0)]
        (recur (update seen new-number (fnil conj []) i)
               new-number
               (inc i))))))

(nth-number-spoken 2020)
;; => 1111

(nth-number-spoken 30000000)
;; => 48568
