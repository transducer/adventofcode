(ns adventofcode.2019.day08
  (:require
   [clojure.string :as string]))

(def image-data
  (->> "resources/2019/day08.txt"
       (slurp)
       (map (comp keyword str))))

(def width 25)
(def height 6)

(def layers
  (partition (* width height) image-data))

(->> (map frequencies layers)
     (apply min-key :0)
     ((juxt :1 :2))
     (apply *))
;; => 1806

(defn color [pixels]
  (first (remove #{:2} pixels)))

(def transpose
  (partial apply map vector))

(def image
  (map color (transpose layers)))

(defn print-image [image]
  (dorun (->> image
              (partition width)
              (map (comp println
                         #(string/escape % {\0 \. \1 \#})
                         #(string/replace % ":" "")
                         (partial apply str))))))

(print-image image)

;; ..##..##..####.###...##..
;; ...#.#..#.#....#..#.#..#.
;; ...#.#..#.###..#..#.#..#.
;; ...#.####.#....###..####.
;; #..#.#..#.#....#.#..#..#.
;; .##..#..#.#....#..#.#..#.

;; => JAFRA
