(ns adventofcode.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "day4.txt"
      io/resource
      io/reader
      line-seq))

(defn parse
  [data]
  (map (fn [[name-sector checksum]] (conj ((juxt #(str/replace % #"\d+" "")
                                                 #(Integer/parseInt (re-find #"\d+" %)))
                                           name-sector)
                                          checksum))
       (map #(str/split (str/replace % #"\]|-" "") #"\[") data)))

(defn real-room?
  [[name sector checksum]]
  (let [freqs     (->> name
                       frequencies
                       (sort-by (juxt second first) #(let [[freq1 id1] %1
                                                           [freq2 id2] %2]
                                                       (if (= freq1 freq2)
                                                         (compare id1 id2)
                                                         (compare freq2 freq1)))))
        five-most (apply str (take 5 (map first freqs)))]
    (= checksum five-most)))


;; Part 1

(->> input
     parse
     (filter real-room?)
     (map second)
     (reduce +))
