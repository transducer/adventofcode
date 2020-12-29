(ns adventofcode.2018.day08
  (:require [clojure.zip :as z]))

(def input
  (->> "resources/2018/day08.txt" slurp (format "[%s]") read-string))

(defn root
  "Zips all the way up and returns the zipper at the root."
  [loc]
  (->> (iterate z/up loc) (take-while (complement nil?)) last))

(def tree-with-zipper
  (loop [loc              (z/vector-zip [[]])
         [x & xs :as all] input]
    (if (seq all)
      (let [next-loc                              (z/next loc)
            {:keys [meta-data-cnt] :as next-node} (z/node next-loc)]
        (cond (z/branch? next-loc)
              (recur (-> (iterate #(z/insert-child % []) next-loc)
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
              (recur next-loc all)))
      (root loc))))


;; Part 1

(->> tree-with-zipper
     flatten
     (mapcat :meta-data)
     (apply +))

;; => 49602


;; Part 2

(defn count-children
  "Gets children count of loc. Not counting meta data map as a child."
  [loc]
  (try (-> loc z/children count dec)
       (catch Exception _ 0)))

(defn values-of-loc [loc]
  (if-not loc
    0
    (let [meta-data (-> loc z/node last :meta-data)]
      (if (zero? (count-children loc))
        (apply + meta-data)
        (for [i meta-data]
          (let [child (nth (iterate z/right (z/next loc)) (dec i))]
            (values-of-loc child)))))))

(->> tree-with-zipper
     z/next
     values-of-loc
     flatten
     (apply +))

;; 25656
