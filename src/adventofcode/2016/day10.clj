(ns adventofcode.2016.day10
  (:require [clojure.java.io :as io]))

(def input
  (-> "day10.txt" io/resource io/reader line-seq))

(def is-chip-regex #"^value.*")
(def number-regex #"\d+")
(def bot-regex #".* (\d+).*(bot|output) (\d+).*(bot|output) (\d+)")

(defn parse-ints
  [s]
  (mapv #(Integer/parseInt %) s))

(defn parse
  [d]
  (map
   #(if (re-matches is-chip-regex %)
      (let [[v b] (parse-ints (re-seq number-regex %))]
        {:chip v :bot b})
      (let [[_ b low-type low high-type high] (re-matches bot-regex %)]
        {:bot       (Integer/parseInt b)
         :low-type  (keyword low-type)
         :low       (Integer/parseInt low)
         :high-type (keyword high-type)
         :high      (Integer/parseInt high)}))
   d))

(defn give-initial-chips
  [d]
  (let [{values true bots false} (group-by #(boolean (:chip %)) d)]
    (reduce #(let [values-for-bot (filter (fn [v] (= (:bot v) (:bot %2))) values)]
               (conj %1 (update-in %2 [:chips] (fn [c] (concat c (map :chip values-for-bot))))))
            []
            bots)))

(defn swap-chips
  [b]
  (let [{distributors true bots false} (group-by #(> (count (:chips %)) 1) b)]
    (concat
     (map #(assoc % :chips []) distributors)
     (map (fn [bot]
            (reduce #(let [{:keys [low-type low high-type high]} %2]
                       (cond-> %1
                         (= low (:bot %1))
                         (update-in [(if (= low-type :bot) :chips :output)]
                                    (fn [c] (conj c (apply min (:chips %2)))))
                         (= high (:bot %1))
                         (update-in [(if (= high-type :bot) :chips :output)]
                                    (fn [c] (conj c (apply max (:chips %2)))))))
                    bot
                    distributors))
          bots))))


;; Part 1

(->> input
     parse
     give-initial-chips
     (iterate swap-chips)
     flatten
     (filter #(= (set (:chips %)) #{17 61}))
     first
     :bot)


;; Part 2

(->> input
     parse
     give-initial-chips
     (iterate swap-chips)
     flatten
     (filter #(and (#{0 1 2} (:bot %))
                   (:output %)))
     (mapcat :output)
     distinct
     (take 3)
     (apply *))
