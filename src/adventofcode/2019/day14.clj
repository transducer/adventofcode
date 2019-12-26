(ns adventofcode.2019.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; Used https://gitlab.com/dmarjenburgh/adventofcode/blob/master/src/adventofcode/year_2019.clj#L393

(def reactions
  (->> (io/resource "2019/day14.txt")
       (slurp)
       (string/split-lines)
       (mapv
        (fn [l]
          (let [eq (re-seq #"(\d+) (\w+)" l)
                [_ n x] (last eq)]
            [x {:amount (Long/parseLong n)
                :reactants (into {}
                                 (map (fn [tuple]
                                        (let [[_ n x] tuple]
                                          [x (Long/parseLong n)])))
                                 (drop-last eq))}])))
       (into {})))

(defn scale-fuel [n]
  (update reactions "FUEL"
          (fn [m]
            {:amount n
             :reactants (reduce-kv (fn [acc k v]
                                     (assoc acc k (* v n)))
                                   {}
                                   (:reactants m))})))

(defn find-ore-amount [n]
  (let [reactions (scale-fuel n)
        required (get-in reactions ["FUEL" :reactants])]
    (loop [required required, stash {}, used 0]
      (if (seq required)
        (let [[x n] (first required)]
          (if (reactions x)
            (let [x-per-tx (get-in reactions [x :amount])
                  reactants (get-in reactions [x :reactants])
                  num-tx (max (long (Math/ceil (/ (- n (stash x 0)) x-per-tx))) 0)
                  x-created (* num-tx x-per-tx)]
              (recur
               (reduce-kv (fn [acc k v]
                            (update acc k (fnil + 0) (* num-tx v)))
                          (dissoc required x)
                          reactants)
               (update stash x (fnil + 0) (- x-created n))
               used))
            (recur (dissoc required x) stash (+ used n))))
        used))))


;; Part 1

(find-ore-amount 1)

;; => 628586


;; Part 2

(defn bisect [f target]
  (loop [a 1, b 1e9]
    (if (< a b)
      (let [x (long (+ (/ (- b a) 2) a))]
        (if (> (f x) target)
          (recur a x)
          (recur (inc x) b)))
      (dec a))))

(bisect find-ore-amount 1e12)

;; => 3209254
