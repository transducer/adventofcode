(ns adventofcode.2016.day08
  (:require
   [clojure.java.io :as io]))

(def input
  (-> "2016/day08.txt" io/resource io/reader line-seq))

(defn parse
  [d]
  (map (fn [line]
         (if-let [rect-matches (re-matches #"rect (\d+)x(\d+)" line)]
           {:rect {:a (-> rect-matches second Integer/parseInt)
                   :b (-> rect-matches last Integer/parseInt)}}
           (let [rot-matches (re-matches #".*(x|y)=(\d+) by (\d+)" line)]
             {:rot (keyword (second rot-matches))
              :i   (-> rot-matches (nth 2) Integer/parseInt)
              :n   (-> rot-matches last Integer/parseInt)})))
       d))

(def width 50)
(def height 6)

(def screen
  (partition width (repeat (* width height) :off)))

(defn rect
  [s a b]
  (concat
   (map #(concat (repeat a :on) (drop a %)) (take b s))
   (drop b s)))

(defn rotate-row
  [s i n]
  (let [before  (take i s)
        curr    (nth s i)
        rotated (->> curr
                     cycle
                     (drop (mod (- n) width))
                     (take width)
                     list)
        after   (drop (inc i) s)]
    (concat before rotated after)))

(defn transpose
  [d]
  (apply map list d))

(defn rotate-col
  [s i n]
  (-> s
      transpose
      (rotate-row i (- n height))
      transpose))

(defn perform-operation
  [s op]
  (if-let [{:keys [a b]} (:rect op)]
    (rect s a b)
    (let [{:keys [rot i n]} op]
      (if (= rot :y)
        (rotate-row s i n)
        (rotate-col s i n)))))

(defn on-count
  [s]
  (->> s
       flatten
       (filter #(= % :on))
       count))

(->> input
     parse
     (reduce perform-operation screen)
     on-count)
;; => 121

(->> input
     parse
     (reduce perform-operation screen)
     (map #(map (fn [toggle] (if (= toggle :on) "X" " ")) %))
     (map #(interpose "  " (partition 5 %)))
     (map #(str (apply str (flatten %)) "\n"))
     (apply str)
     (spit "resources/2016/day08-letters.txt"))

;; XXX    X  X   XXX    X  X    XX    XXXX    XX    XXXX    XXX   X
;; X  X   X  X   X  X   X  X   X  X   X      X  X   X        X    X
;; X  X   X  X   X  X   X  X   X      XXX    X  X   XXX      X    X
;; XXX    X  X   XXX    X  X   X      X      X  X   X        X    X
;; X X    X  X   X X    X  X   X  X   X      X  X   X        X    X
;; X  X    XX    X  X    XX     XX    XXXX    XX    XXXX    XXX   XXXX

;; => RURUCEOEIL
