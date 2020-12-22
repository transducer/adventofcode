(ns adventofcode.2020.day22
  (:require
   [clojure.string :as string]))

(def decks
  (->> (-> "resources/2020/day22.txt" slurp (string/split #"Player .:\n"))
       rest
       (mapv #(read-string (format "[%s]" %)))))

(defn finished? [deck]
  (= (count deck) (count (apply concat decks))))

(defn round [[[c1 & _ :as deck1] [c2 & _ :as deck2]]]
  (cond (or (nil? c1) (nil? c2)) [deck1 deck2]
        (> c1 c2) [(conj (subvec deck1 1) c1 c2) (subvec deck2 1)]
        :else [(subvec deck1 1) (conj (subvec deck2 1) c2 c1)]))

(defn winning-score [deck]
  (->> deck
       reverse
       (map-indexed (fn [index item] (* (inc index) item)))
       (reduce +)))

(->> decks
     (iterate round)
     (keep (fn [decks] (first (filter finished? decks))))
     first
     winning-score)
;; => 31809

(defn recursive-combat [seen [[c1 & _ :as deck1] [c2 & _ :as deck2] :as decks]]
  (cond (contains? seen decks) {:winning-player 1 :deck deck1}
        (nil? c1) {:winning-player 2 :deck deck2}
        (nil? c2) {:winning-player 1 :deck deck1}
        :else (if (= 1 (if (and (> (count deck1) c1)
                                (> (count deck2) c2))
                         (:winning-player
                          (recursive-combat #{} [(subvec deck1 1 (inc c1)) (subvec deck2 1 (inc c2))]))
                         (if (> c1 c2) 1 2)))
                (recur (conj seen decks) [(conj (subvec deck1 1) c1 c2) (subvec deck2 1)])
                (recur (conj seen decks) [(subvec deck1 1) (conj (subvec deck2 1) c2 c1)]))))

(->> decks
     (recursive-combat #{})
     :deck
     winning-score)
;; => 32835
