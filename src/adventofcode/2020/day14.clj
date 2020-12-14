(ns adventofcode.2020.day14
  (:require
   [clojure.string :as string]))

(def program
  (string/split-lines (slurp "resources/2020/day14.txt")))

(defn bit-masks [s]
  {:on (-> (string/escape s {\X 0 \1 1 \0 0})
           (Long/parseLong 2))
   :off (-> (string/escape s {\X 1 \1 1 \0 0})
            (Long/parseLong 2))})

(->> program
     (reduce
      (fn [{{:keys [on off]} :masks :as state} line]
        (if-let [mask (re-find #"mask =.*" line)]
          (assoc state :masks (bit-masks (subs mask 7)))
          (let [[address value] (map read-string (re-seq #"\d+" line))]
            (assoc-in state [:mem address]
                      (->> value
                           (bit-or on)
                           (bit-and off))))))
      {:mem {}
       :masks {}})
     :mem
     vals
     (apply +))
;; => 10885823581193

(defn on-masks [mask binary]
  (condp = (first mask)
    nil binary
    \X [(on-masks (rest mask) (str binary 0))
        (on-masks (rest mask) (str binary 1))]
    \1 (on-masks (rest mask) (str binary 0))
    \0 (on-masks (rest mask) (str binary 0))))

(defn off-masks [mask binary]
  (condp = (first mask)
    nil binary
    \X [(off-masks (rest mask) (str binary 0))
        (off-masks (rest mask) (str binary 1))]
    \1 (off-masks (rest mask) (str binary 1))
    \0 (off-masks (rest mask) (str binary 1))))

(defn main-mask [mask binary]
  (condp = (first mask)
    nil binary
    \X (main-mask (rest mask) (str binary 0))
    \1 (main-mask (rest mask) (str binary 1))
    \0 (main-mask (rest mask) (str binary 0))))

(defn addresses [address mask]
  (map
   (fn [on-mask off-mask main-mask]
     (-> address
         (bit-or (Long/parseLong on-mask 2))
         (bit-and (Long/parseLong off-mask 2))
         (bit-or (Long/parseLong main-mask 2))))
   (flatten (on-masks mask ""))
   (flatten (off-masks mask ""))
   (repeat (main-mask mask ""))))

(->> program
     (reduce
      (fn [{:keys [mask] :as state} line]
        (if-let [mask (re-find #"mask =.*" line)]
          (assoc state :mask (subs mask 7))
          (let [[address value] (map read-string (re-seq #"\d+" line))]
            (reduce
             (fn [s a] (assoc-in s [:mem a] value))
             state
             (addresses address mask)))))
      {:mem {}
       :mask {}})
     :mem
     vals
     (apply +))
;; => 3816594901962
