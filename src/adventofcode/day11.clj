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
        possible-floors (filter #(and (>= % 1) (<= % 4)) [(inc floor) (dec floor)])
        ;; At most two in the elevator each move. Or nothing.
        combs           (concat
                         (combinations elems 1)
                         (combinations elems 2))
        ;; Move every one or two elems (every possible combination) to every
        ;; possible floor
        moves           (map #(hash-map :floors possible-floors :elems %) combs)]
    moves))

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
  [d {:keys [floors elems]}]
  (map #(take-elevator d % elems) floors))

(def counter (atom 0))

(defn possible-new-states
  [d]
  (let [moves (possible-moves d)]
    (mapcat #(new-states d %) moves)))

(defn materials
  [elems type]
  (->> elems
       (filter #(= (second (name %)) type))
       (map #(first (name %)))
       set))

(defn safe?
  [{:keys [elems] :as floor}]
  (let [micro-chips (materials elems \M)
        generators  (materials elems \G)]
    ;; Do not keep unconnected micro-chip on floor with generator of other material.
    (or (empty? generators)
        (empty? (set/difference micro-chips generators)))))

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
