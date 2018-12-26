(ns adventofcode.2018.day8
  (:require [clojure.zip :as z]))

(def input
  (->> "resources/2018/day8.txt" slurp (format "[%s]") read-string))

(def zipper
  (loop [loc              (z/vector-zip [[]])
         [x & xs :as all] input]
    (if (empty? all)
      (z/root loc)
      (let [next-loc                              (z/next loc)
            {:keys [meta-data-cnt] :as next-node} (z/node next-loc)]
        (cond (z/branch? next-loc)
              (recur (-> (iterate #(z/insert-child % []) (z/next loc))
                         (nth x)
                         (z/insert-child :add-meta-data-cnt))
                     xs)

              (= :add-meta-data-cnt next-node)
              (recur (-> next-loc z/remove (z/append-child {:meta-data-cnt x})) xs)

              (and meta-data-cnt (> meta-data-cnt 0))
              (recur (-> next-loc
                         (z/edit assoc :meta-data-cnt (dec meta-data-cnt))
                         (z/edit update :meta-data conj x)
                         z/prev)
                     xs)

              :else
              (recur next-loc all))))))


;; Part 1

(->> zipper
     flatten
     (mapcat :meta-data)
     (apply +))

;; => 49602


;; Part 2
