(ns adventofcode.2016.day16)

(def input
  "10010000000110000")

(def parse
  (partial map char))

(defn toggle [c]
  (if (= c \1) \0 \1))

(defn dragon [d]
  (concat d [\0] (map toggle (reverse d))))

(defn checksum [d]
  (map (fn [[a b]] (if (= a b) 1 0))
       (partition 2 d)))

(defn solve [data disc-length]
  (let [dragonized
        (loop [d data]
          (if (>= (count d) disc-length)
            (take disc-length d)
            (recur (dragon d))))]
    (loop [cs (checksum dragonized)]
      (if (odd? (count cs))
        (apply str cs)
        (recur (checksum cs))))))

(solve (parse input) 272) ; => "10010110010011110"
(solve (parse input) 35651584) ; => "01101011101100011"
