(ns adventofcode.day11
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :refer [combinations]]))

(def input
  [{:elevator true  :floor 1 :elems [:PG :TG :TM :UG :RG :RM :CG :CM]}
   {:elevator false :floor 2 :elems [:PM :UM]}
   {:elevator false :floor 3 :elems []}
   {:elevator false :floor 4 :elems []}])

(defn current-room
  [d]
  (first (filter :elevator d)))

(defn possible-moves
  [d]
  (let [room            (current-room d)
        elems           (:elems room)
        floor           (:floor room)
        possible-floors (range 1 5)
        ;; At most two in the elevator each move. Or nothing.
        combs           (concat
                         (combinations elems 0)
                         (combinations elems 1)
                         (combinations elems 2))
        ;; Move every one or two elems (every possible combination) to every
        ;; possible floor
        moves           (map #(hash-map :floors possible-floors :elems %) combs)]
    moves))

(defn materials
  [elems type]
  (->> elems
       (filter #(= (second (name %)) type))
       (map #(first (name %)))
       set))

(defn take-elevator
  [d floor elems]
  (let [current  (-> (current-room d)
                     (update-in [:elems] #(remove (fn [e] ((set elems) e)) %))
                     (assoc :elevator false))
        new-room (-> (first (filter #(= (:floor %) floor) d))
                     (update-in [:elems] (partial apply conj) elems)
                     (assoc :elevator true))
        others   (filter #(and (not= (:floor %) floor)
                               (not= (:floor %) (:floor current)))
                         d)]
    (conj others current new-room)))

(defn new-states
  [d move]
  (let [floors (:floors move)
        elems  (:elems move)]
    (map #(take-elevator d % elems) floors)))

(def counter (atom 0))

(defn possible-new-states
  [d]
  (let [moves (possible-moves d)]
    (mapcat #(new-states d %) moves)))

(defn safe?
  [floor]
  (let [elems       (:elems floor)
        micro-chips (materials elems \M)
        generators  (materials elems \G)]
    ;; Do not keep micro-chip on floor with generator for other material
    ;; Unless micro-chip is of same material as generator
    (let [generators-without-chip (set/difference generators micro-chips)
          micro-chips-without-gen (set/difference micro-chips generators)]
      (not (and (not-empty generators-without-chip)
                (not-empty micro-chips-without-gen))))))

(defn valid-state?
  [d]
  (every? safe? d))

(defn finished?
  [d]
  (->> d
       (filter #(not= (:floor 4) %))
       (mapcat :elems)
       empty?))


;; Part 1

(->> input
     list
     (iterate #(mapcat possible-new-states %))
     (take 2))

#_(->> input
     list
     (iterate #(mapcat possible-new-states %))
     (filter valid-state?)
     (filter finished?)
     first)
