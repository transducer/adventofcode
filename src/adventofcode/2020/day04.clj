(ns adventofcode.2020.day04
  (:require
   [clojure.string :as string]))

(def transpose
  (partial apply map vector))

(defn parse [passport]
  (->> passport
       (re-seq #"([^ \n]+):([^ \n]+)")
       transpose
       rest
       (apply zipmap)))

(def passports
  (->> (string/split (slurp "resources/2020/day04.txt") #"\n\n")
       (map parse)))

(defn valid? [passport]
  (every? passport #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}))

(count (filter valid? passports)) ; => 250

(defn parse-or-0 [s]
  (try (Integer. s) (catch Exception _ 0)))

(defn valid-height? [s]
  (when-let [[height unit] (rest (re-find #"(\d+)(in|cm)" s))]
    (case unit
      "cm" (<= 150 (parse-or-0 height) 193)
      "in" (<= 59 (parse-or-0 height) 76)
      nil)))

(defn valid-hair-color? [s]
  (and s
       (when-let [[x xs] (rest (re-find #"(#)(([a-f]|[0-9])*)" s))]
         (and (= x "#") (= (count xs) 6)))))

(defn valid-pid? [s]
  (and (= (count s) 9)
       (= (count (re-find #"\d+" s)) 9)))

(defn valid2? [{:strs [byr iyr eyr hgt hcl ecl pid]}]
  (and (<= 1920 (parse-or-0 byr) 2002)
       (<= 2010 (parse-or-0 iyr) 2020)
       (<= 2020 (parse-or-0 eyr) 2030)
       (valid-height? hgt)
       (valid-hair-color? hcl)
       (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
       (valid-pid? pid)))

(count (filter valid2? passports)) ; => 158
