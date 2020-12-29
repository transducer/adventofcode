(ns adventofcode.2019.day06
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]))

(def input
  (->> (-> (io/resource "2019/day06.txt")
           slurp
           (string/replace #"\)" "\n")
           (string/split-lines))
       (map (partial keyword (str *ns*)))))

(defn add-orbits [planets]
  (doseq [[p1 p2] (partition 2 planets)]
    (derive p1 p2)))

(defn count-orbits [planets]
  (apply + (map (comp count ancestors) planets)))

(add-orbits input)


;; Part 1

(count-orbits (set input))

;; => 273985


;; Part 2

(let [ys (descendants ::YOU)
      ss (descendants ::SAN)]
  (count
   (set/difference
    (set/union ys ss)
    (set/intersection ys ss))))

;; => 460
