(ns adventofcode.2019.day08)

(def image-data
  (->> "resources/2019/day08.txt"
       (slurp)
       (map (comp keyword str))))

(def width 25)
(def height 6)

(def layers
  (partition (* width height) image-data))


;; Part 1

(->> (map frequencies layers)
     (apply min-key :0)
     ((juxt :1 :2))
     (apply *))

;; => 1806


;; Part 2

(defn color [pixels]
  (first (remove #{:2} pixels)))

(def transpose
  (partial apply map vector))

(def image
  (map color (transpose layers)))

(defn print-image [image]
  (dorun (map println (partition width image))))

(print-image image)

;; => JAFRA
