(ns adventofcode.2020.day19
  (:require
   [clojure.string :as string]
   [instaparse.core :as insta]))

(def input
  (string/split (slurp "resources/2020/day19.txt") #"\n\n"))

(def grammar
  (first input))

(def messages
  (string/split-lines (second input)))

(def parser
  (insta/parser grammar :start :0))

(count (remove #(insta/failure? (parser %)) messages))
;; => 176

(def recursive-grammar
  (-> grammar
      (string/replace "8: 42" "8: 42 | 42 8")
      (string/replace "11: 42 31" "11: 42 31 | 42 11 31")))

(def recursive-parser
  (insta/parser recursive-grammar :start :0))

(count (remove #(insta/failure? (recursive-parser %)) messages))
;; => 352
