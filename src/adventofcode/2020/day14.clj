(ns adventofcode.2020.day14
  (:require
   [clojure.string :as string]))

(def program
  (string/split-lines (slurp "resources/2020/day14.txt")))

(defn bit-masks [s]
  {:on (-> (string/replace s "X" "0")
           (Long/parseLong 2))
   :off (-> (string/replace s "X" "1")
            (Long/parseLong 2))})

(defn sum-memory [output]
  (->> output :mem vals (apply +)))

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
     sum-memory)
;; => 10885823581193

(defn masks [mask-type mask binary]
  (if-let [c (first mask)]
    (if (= c \X)
      (mapcat #(masks mask-type (rest mask) (str binary %)) [0 1])
      (masks mask-type (rest mask) (str binary (case mask-type :on 0 :off 1))))
    [binary]))

(defn main-mask [mask]
  (string/replace mask "X" "0"))

(defn addresses [address mask]
  (map
   (fn [on-mask off-mask main-mask]
     (-> address
         (bit-or (Long/parseLong on-mask 2))
         (bit-and (Long/parseLong off-mask 2))
         (bit-or (Long/parseLong main-mask 2))))
   (masks :on mask "")
   (masks :off mask "")
   (repeat (main-mask mask))))

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
     sum-memory)
;; => 3816594901962
