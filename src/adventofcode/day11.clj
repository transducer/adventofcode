(ns adventofcode.day11
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :refer [combinations]]))

(def input
  [[{:elevator true :floor 1 :elems [:PG :TG :TM :UG :RG :RM :CG :CM]}
    {:elevator false :floor 2 :elems [:PM :UM]}
    {:elevator false :floor 3 :elems []}
    {:elevator false :floor 4 :elems []}]])

(defn current-room [d]
  (first (filter :elevator d)))

(defn possible-moves [s]
  (let [room            (current-room s)
        elems           (:elems room)
        floor           (:floor room)
        possible-floors (filter #(and (>= % 1) (<= % 4)) ((juxt dec inc) floor))
        ;; One or two in the elevator each move.
        combs           (concat
                         (combinations elems 1)
                         (combinations elems 2))
        ;; Move every one or two elems (every possible combination) to every
        ;; possible floor
        moves           (map #(hash-map :floors possible-floors :elems %) combs)]
    moves))

(defn take-elevator [d floor elems]
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

(def seen (atom #{}))

(defn normal-form [s]
  [(:floor (current-room s)) (map #(->> % :elems (map (fn [e] (-> e name second)))) s)])

(defn equivalent? [s n]
  (and (= (normal-form s) n)))

(defn materials [elems type]
  (->> elems
       (filter #(= (second (name %)) type))
       (map #(first (name %)))
       set))

(defn safe? [{:keys [elems] :as floor}]
  (let [micro-chips (materials elems \M)
        generators  (materials elems \G)]
    ;; Do not keep unconnected micro-chip on floor with generator of other
    ;; material.
    (or (empty? generators)
        (empty? (set/difference micro-chips generators)))))

(defn valid-state? [s] (every? safe? s))

(defn new-states [s {:keys [floors elems] :as moves}]
  (->> floors
       (map #(take-elevator s % elems))
       (filter valid-state?)
       (remove #(some (fn [n] (equivalent? % n)) @seen))
       seq))

(defn possible-new-states [s]
  (let [moves (possible-moves s)
        new   (mapcat #(new-states s %) moves)
        _     (swap! seen concat (map normal-form new))]
    new))

(defn finished? [s]
  (->> s
       (filter #(not= (:floor %) 4))
       (mapcat :elems)
       empty?))


;; Part 1

(loop [d input c 0]
  (println "Amount of elements:" (count d))
  (println "Round:" c)
  (if (some #(finished? %) d) c
      (recur (mapcat possible-new-states d) (inc c))))


;; Part 2

#_(def input2
  [[{:elevator true :floor 1 :elems [:PG :TG :TM :UG :RG :RM :CG :CM :EG :EM :DG :DM]}
    {:elevator false :floor 2 :elems [:PM :UM]}
    {:elevator false :floor 3 :elems []}
    {:elevator false :floor 4 :elems []}]])

#_(loop [d input2 c 0]
  (println "Amount of elements:" (count d))
  (println "Round:" c)
  (if (some #(finished? %) d) c
      (recur (mapcat possible-new-states d) (inc c))))
