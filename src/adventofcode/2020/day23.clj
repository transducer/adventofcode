(ns adventofcode.2020.day23)

(def input
  [6 2 4 3 9 7 1 5 8])

(def width
  (count input))

(defn destination-cup [current-cup possible]
  (loop [cc (dec current-cup)]
    (if (contains? possible cc)
      cc
      (recur (mod (dec cc) (inc width))))))

(defn get-three-cups [new-input current-cup-idx]
  (->> new-input
       cycle
       (drop (inc current-cup-idx))
       (take 3)))

(defn make-new-input [input dest three-cups]
  (let [new-input (vec (remove (set three-cups) input))
        index (first
               (keep-indexed (fn [index item]
                               (when (= item dest)
                                 index))
                             new-input))]
    (vec (concat (take (inc index) new-input) three-cups (drop (inc index) new-input)))))

(defn play [moves-count]
  (loop [input input
         current-cup (first input)
         current-cup-idx 0
         moves moves-count]
    (let [three-cups (get-three-cups input current-cup-idx)
          dest (destination-cup current-cup (set (remove (set three-cups) input)))
          new-input (make-new-input input dest three-cups)]
      (if (= moves 0)
        (apply str (take (dec width) (rest (drop-while (complement #{1}) (cycle new-input)))))
        (recur
         new-input
         (nth input (mod (+ current-cup-idx 4) width))
         (inc (first (keep-indexed (fn [index item] (when (= item current-cup) index)) new-input)))
         (dec moves))))))

(play 99)
;; => "74698532"

;; Used https://github.com/akovantsev/adventofcode/blob/master/src/adventofcode/y2020/day23.cljc#L111-L146

(defn destination [curr max-value abc]
  (let [banned (set abc)]
    (or (->> ^int curr
             dec
             (iterate dec)
             (drop-while banned)
             (take-while pos?)
             first)
        (->> ^int max-value
             (iterate dec)
             (drop-while banned)
             first))))

(let [moves 10000000
      max-value 1000000
      arr (int-array (range 1 (inc (inc max-value))))]
  (aset arr 0 ^int (first input))
  (doseq [[x y] (partition 2 1 input)]
    (aset arr x ^int y))
  (aset arr (peek input) ^int (inc (reduce max input)))
  (aset arr max-value ^int (first input))
  (dotimes [_ moves]
    (let [current (aget arr 0)
          a (aget arr current)
          b (aget arr a)
          c (aget arr b)
          after-c (aget arr c)
          dest (destination current max-value [a b c])
          after-dest (aget arr dest)]
      (aset arr current after-c)
      (aset arr dest a)
      (aset arr c after-dest)
      (aset arr 0 after-c)))
  (* (get arr 1) (get arr (get arr 1))))
;; => 286194102744
