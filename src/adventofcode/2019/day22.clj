(ns adventofcode.2019.day22
  (:require
   [clojure.java.io :as io]))

(def lines
  (line-seq (io/reader "resources/2019/day22.txt")))

;; Part 1

(def factory-order-deck
  (vec (range 10007)))

(defn deal-into-new-stack [deck]
  (vec (rseq deck)))

(defn cut [deck n]
  (if (nat-int? n)
    (into (subvec deck n) (subvec deck 0 n))
    (into (subvec deck (+ (count deck) n)) (subvec deck 0 (+ (count deck) n)))))

(defn deal [deck increment]
  (loop [space (vec (repeat (count deck) 0))
         deck-pos 0
         space-pos 0
         searching-open-pos? false]
    (cond (every? (complement zero?) (subvec space 1))
          space
          (zero? (space space-pos))
          (recur (assoc space space-pos (deck deck-pos))
                 (if searching-open-pos?
                   deck-pos
                   (mod (inc deck-pos) (count deck)))
                 (mod (+ space-pos increment) (count deck))
                 false)
          :else
          (recur space
                 deck-pos
                 (mod (inc space-pos) (count space))
                 true))))

(.indexOf
 (reduce
  (fn [deck line]
    (let [op (re-find #"cut|new|increment" line)
          n (re-find #"-*\d+" line)]
      (case op
        "cut" (cut deck (Integer/parseInt n))
        "new" (deal-into-new-stack deck)
        "increment" (deal deck (Integer/parseInt n)))))
  factory-order-deck
  lines)
 2019)

;; => 4096


;; Part 2

;;; Used https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/

(def m
  (biginteger 119315717514047))

(def n
  (biginteger 101741582076661))

(def pos
  2020)

(def shuffles
  {"cut" (fn [x m a b] [(mod (* a x) m) (mod (* b x) m)])
   "new" (fn [_ m a b] [(mod (- a) m) (mod (- m 1 b) m)])
   "increment" (fn [x m a b] [a (mod (- b x) m)])})

(defn arithmetico-geometric [lines]
  (reduce
   (fn [[a b] line]
     (let [op (re-find #"cut|new|increment" line)
           n (Integer/parseInt (or (re-find #"-*\d+" line) "0"))
           f (shuffles op)]
       (f n m a b)))
   [1 0]
   lines))

(defn mod-pow [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(let [[a b] (arithmetico-geometric lines)
      r (mod (* b (mod-pow (- 1 b) (- m 2) m)) m)]
  (mod (* (- pos r) (+ (mod-pow a (* n (- m 2)) m) r)) m))

;; => 78613970589919
