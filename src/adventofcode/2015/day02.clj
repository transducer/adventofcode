(ns adventofcode.2015.day02)

(def input
  (slurp "resources/2015/day02.txt"))

(defn wrapping-paper [[l w h]]
  (+ (* 2 l w) (* 2 w h) (* 2 h l)
     (min (* l w) (* w h) (* l h))))

(defn parse [input]
  (->> input
       (re-seq #"(\d+)x(\d+)x(\d+)\n")
       (map (comp (partial map #(Integer/parseInt %)) rest))))

(->> (parse input)
     (map wrapping-paper)
     (apply +))
;; => 1588178

(defn ribbon [[l w h]]
  (+ (* l w h)
     (min (* 2 (+ l w))
          (* 2 (+ w h))
          (* 2 (+ l h)))))

(->> (parse input)
     (map ribbon)
     (apply +))
;; => 3783758
