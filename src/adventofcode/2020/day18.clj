(ns adventofcode.2020.day18
  (:require
   [clojure.string :as string]
   [instaparse.core :as insta]))

(def input
  (string/split-lines (slurp "resources/2020/day18.txt")))

(def arithmetic-parser
  (insta/parser
   "expr = add-mul
    <add-mul> = term | add | mul
    add = add-mul <'+'> term
    mul = add-mul <'*'> term
    <term> = number | <'('> add-mul <')'>
    number = #'[0-9]+'"
   :auto-whitespace :standard))

(def parse-tree->sexp
  {:add +
   :mul *
   :number read-string
   :expr identity})

(reduce
 (fn [acc expr]
   (+ acc
      (->> (arithmetic-parser expr)
           (insta/transform parse-tree->sexp))))
 0
 input)
;; => 11076907812171

(def arithmetic-parser2
  (insta/parser
   "expr = add-expr | mul-expr
    <mul-expr> = add-expr | mul
    mul = mul-expr <'*'> add-expr
    <add-expr> = term | add
    add = add-expr <'+'> term
    <term> = number | <'('> mul-expr <')'>
    number = #'[0-9]+'"
   :auto-whitespace :standard))

(reduce
 (fn [acc expr]
   (+ acc
      (->> (arithmetic-parser2 expr)
           (insta/transform parse-tree->sexp))))
 0
 input)
;; => 283729053022731

;;; Visualization
(insta/visualize (arithmetic-parser2 "1 + (2 * 3) + (4 * (5 + 6))"))
