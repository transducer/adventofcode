(ns adventofcode.2020.day9)

(def input
  (read-string (format "[%s]" (slurp "resources/2020/day9.txt"))))

(->> (range 25 (count input))
     (remove
      (fn [i]
        (let [prev (subvec input (- i 25) i)
              sums (set (for [a prev b prev] (+ a b)))]
          (contains? sums (nth input i)))))
     first
     (nth input))
;; => 756008079

(def target
  756008079)

(->> (range (count input))
     (keep
      (fn [i]
        (reduce
         (fn [acc e]
           (let [next-acc (conj acc e)
                 sum (apply + next-acc)]
             (cond (> sum target) (reduced nil)
                   (= sum target) (reduced (+ (apply min next-acc) (apply max next-acc)))
                   :else next-acc)))
         #{}
         (subvec input i))))
     first)
;; => 93727241
