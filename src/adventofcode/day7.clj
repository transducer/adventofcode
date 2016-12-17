(ns adventofcode.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "day7.txt" io/resource io/reader line-seq))

(defn parse
  [d]
  (map (juxt #(map second (re-seq #"\](.*?)\[" (str "]" % "[")))
             #(map second (re-seq #"\[(.*?)\]" %)))
       d))

(defn abba?
  [s]
  (->> [0 1 2 3]
       (mapcat #(->> (partition 4 (subs s %))
                     (map (fn [cs] (apply str cs)))))
       concat
       (some #(re-find #"(?=(.)(.)\2\1)(.)(?!\1).*" %))))

(defn tls?
  [ip]
  (and (some abba? (first ip))
       (not (some abba? (second ip)))))


;; Part 1

(->> input
     parse
     (filter tls?)
     count)


;; Part 2

(defn candidates
  [s]
  (->> [0 1 2]
       (mapcat #(->> (partition 3 (subs s %))
                     (map (fn [cs] (apply str cs)))))
       concat
       (filter #(re-find #"(?=(.).\1)(.)(?!\1).*" %))))

(defn ssl?
  [[inside outside]]
  (let [abas (mapcat candidates inside)
        babs (mapcat candidates outside)]
    (some #(some (fn [[a b]] (let [bab (str b a b)]
                               (= bab %)))
                 abas)
          babs)))

(->> input
     parse
     (filter ssl?)
     count)
