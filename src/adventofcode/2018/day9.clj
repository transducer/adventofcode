(ns adventofcode.2019.day9)

;; (def players 459)
;; (def last-marble 71320)

(def player-count 9)
(def last-marble 25)

(defn make-score [player-count]
  (->> player-count
       range
       (mapcat (fn [p] [p 0]))
       (apply hash-map)))

(def board
  {:score (make-score player-count)
   :marbles [0]
   :current-marble 0})

(defn insert [{:keys [players marbles current-marble] :as board}]
  ;; when multiple of 23 ...
  ;; between 1, 2 clockwise
  )

;; (def players 10)
;; (def last-marble 1618)
;; 8317


;; Part 1


;; =>
