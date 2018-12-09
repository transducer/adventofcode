(ns adventofcode.2016.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> "day22.txt" io/resource io/reader line-seq))

(defn parse-ints [d]
  (map #(Integer/parseInt %) d))

(defn parse [d]
  (->> (drop 2 d)
       (map (fn [l] (let [[x y size used avail use]
                          (parse-ints (rest (re-matches #".*node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%" l)))]
                      {:x x
                       :y y
                       :size size
                       :used used
                       :avail avail
                       :use use})))))

(defn viable-pair? [a b]
  (and (not (zero? (:used a)))
       (not= a b)
       (<= (:used a) (:avail b))))

(defn viable-pairs [nodes]
  (for [node1 nodes
        node2 nodes
        :when (viable-pair? node1 node2)]
    [node1 node2]))

(def nodes (parse input))


;; Part 1

(count (viable-pairs nodes))
;; => 1034


;; Part 2

(defn str-insert
  "Insert c in string s at index i."
  [s c i]
  (str (subs s 0 i) c (subs s (inc i))))

(defn make-grid [nodes]
  (let [empty-grid (->> (repeat " ")
                        (take 38)
                        (apply str)
                        repeat
                        (take 28)
                        vec)]
    (reduce (fn [grid {:keys [x y size used] :as node}]
              (vec
               (concat
                (take y grid)
                [(str-insert (grid y)
                             (cond (= used 0)   "_"
                                   (> size 100) "#"
                                   :else        ".")
                             x)]
                (drop (inc y) grid))))
            empty-grid
            nodes)))

(defn draw [grid]
  (doseq [row grid] (println row)))

(draw (make-grid nodes)) ;; =>

;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; ......................................
;; .........#############################
;; ......................................
;; .................................._...
;; ......................................

;; Count by hand:

(+
 ;; Around the closed line and move goal data one step to the left
 (+ 26 3 28 23 1)
 ;; 5 steps to move data one step 36 times
 (+ (* 36 5)))
;; => 261
