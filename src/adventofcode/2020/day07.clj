(ns adventofcode.2020.day07
  (:require
   [clojure.string :as string]))

(def input
  (string/split-lines (slurp "resources/2020/day07.txt")))

(defn parse [line]
  (let [[bag-type & contents] (-> line
                                  (string/replace #" bags?\.?" "")
                                  (string/split #" contain |, "))]
    {:amount 1
     :bag-type bag-type
     :contents (for [content (remove #{"no other"} contents)]
                 {:amount (Integer. (subs content 0 1))
                  :bag-type (first (re-find #"(\s?\w)+" (subs content 2)))})}))

(def rules
  (map parse input))

(defn contains-bag? [bag-type {:keys [contents] :as _rule}]
  (some (comp #{bag-type} :bag-type) contents))

(defn find-bags [bag-type]
  {:bag-type bag-type
   :paths (for [rule rules
                :when (contains-bag? bag-type rule)]
            (find-bags (:bag-type rule)))})

(->> (tree-seq seq :paths (find-bags "shiny gold"))
     (keep :bag-type)
     set
     count
     dec)
;; => 205

(defn get-rule [bag-type]
  (first (filter (comp #{bag-type} :bag-type) rules)))

(defn bags-count [{:keys [bag-type amount]}]
  (* amount (apply + 1 (->> bag-type get-rule :contents (map bags-count)))))

(dec (bags-count {:bag-type "shiny gold" :amount 1}))
;; => 80902
