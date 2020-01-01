(ns adventofcode.2019.day16)

(def input
  (->> "resources/2019/day16.txt" slurp drop-last (map (comp read-string str))))

(defn pattern [n]
  (rest (cycle (mapcat (partial repeat n) [0 1 0 -1]))))

(defn last-digit [n]
  (mod (Math/abs n) 10))

(defn fft [digits]
  (for [i (range 1 (inc (count digits)))]
    (->> digits
         (map (fn [a b] (* a b)) (pattern i))
         (reduce +)
         last-digit)))

(defn number [digits]
  (loop [[x & xs] (reverse digits)
         base 1
         n 0]
    (if x
      (recur xs (* 10 base) (+ n (* base x)))
      n)))


;; Part 1

(number (take 8 (nth (iterate fft input) 100)))

;; => 82435530


;; Part 2

(def offset
  (number (take 7 input)))

;; With help from https://www.reddit.com/r/adventofcode/comments/ebai4g

;; Because of the second value in the pattern being a one, for the 2nd half of
;; the values, every multiplication value is a one. So, for indices past
;; halfway (offset is over halfway) their next FFT is the last digit of the sum
;; of all digits after them. We can construct each one of these in O(n) time by
;; maintaining a partial sum over time.

(defn end-fft [xs]
  (reductions (fn [acc n] (mod (+ acc n) 10)) xs))

(let [xs-after-offset (->> input
                           (repeat 1e4)
                           (apply concat)
                           (drop offset)
                           reverse
                           (iterate end-fft))]
  (->> (nth xs-after-offset 100)
       reverse
       (take 8)
       number))

;; => 83036156
