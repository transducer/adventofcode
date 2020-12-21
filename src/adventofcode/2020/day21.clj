(ns adventofcode.2020.day21
  (:require
   [clojure.string :as string]
   [clojure.set :as set]))

(def input
  (slurp "resources/2020/day21.txt"))

(def allergens-with-foods
  (for [line (string/split-lines input)
        :let [[_ part1 part2] (re-find #"(.*)\(contains (.*)\)" line)
              foods (string/split part1 #" ")
              allergens (string/split part2 #", ")]]
    [allergens foods]))

(def allergen->foods
  (reduce
   (fn [acc [allergens foods]]
     (reduce
      (fn [a allergen]
        (if-let [curr (get a allergen)]
          (assoc a allergen (set/intersection (set foods) curr))
          (assoc a allergen (set foods))))
      acc
      allergens))
   {}
   allergens-with-foods))

(def allergen-count
  (count allergen->foods))

(def food->allergen
  (loop [a->fs allergen->foods
         acc {}]
    (if (= (count acc) allergen-count)
      acc
      (let [[allergen food] (first
                             (keep (fn [[allergen foods]]
                                     (when (= (count foods) 1)
                                       [allergen (first foods)]))
                                   a->fs))]
        (recur
         (into {} (map (fn [[a fs]] [a (disj fs food)]) a->fs))
         (assoc acc food allergen))))))

(def foods
  (mapcat second allergens-with-foods))

(def foods-without-allergens
  (set/difference (set foods) (set (map first food->allergen))))

(->> foods
     (filter foods-without-allergens)
     count)
;; => 2230

(->> food->allergen
     (sort-by second)
     (map first)
     (string/join ","))
;; => "qqskn,ccvnlbp,tcm,jnqcd,qjqb,xjqd,xhzr,cjxv"
