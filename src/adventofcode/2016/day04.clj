(ns adventofcode.2016.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "2016/day04.txt"
      io/resource
      io/reader
      line-seq))

(defn parse
  [data]
  (map (fn [line]
         (let [cleaned (str/replace line #"\]|-" "")
               parts   (str/split cleaned #"\[")]
           {:name     (str/replace (first parts) #"\d+" "")
            :sector   (Integer/parseInt (re-find #"\d+" (first parts)))
            :checksum (second parts)}))
       data))

(defn real-room?
  [{:keys [name checksum]}]
  (let [freqs     (->> name
                       frequencies
                       (sort-by (fn [[a b]] [(- b) a])))
        five-most (apply str (take 5 (map first freqs)))]
    (= checksum five-most)))


;; Part 1

(->> input
     parse
     (filter real-room?)
     (map :sector)
     (apply +))


;; Part 2

(defn shift-char
  [c]
  (if (= c \z) \a
      (-> c int inc char)))

(defn decrypt
  [{:keys [name sector checksum] :as room}]
  (assoc room :decrypted
         (->> name
              (map #((apply comp (repeat sector shift-char)) %))
              (apply str))))

(defn north-pole-objects?
  [{:keys [decrypted]}]
  (.contains decrypted "north"))

(->> input
     parse
     (filter real-room?)
     (map decrypt)
     (filter north-pole-objects?)
     first
     :sector)
