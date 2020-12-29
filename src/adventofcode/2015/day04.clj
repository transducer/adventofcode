(ns adventofcode.2015.day04)

(defn md5 [^String s]
  (->> (.getBytes s)
       (.digest (java.security.MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn mine [n-zeroes]
  (let [prefix (apply str (repeat n-zeroes \0))]
    (->> (range)
         (pmap (fn [n] [n (md5 (str "iwrupvqb" n))]))
         (filter (fn [[_n s]] (.startsWith s prefix)))
         ffirst)))

(mine 5) ; => 346386
(mine 6) ; => 9958218
