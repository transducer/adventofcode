(ns adventofcode.2020.day10)

(def input
  (read-string (format "[%s]" (slurp "resources/2020/day10.txt"))))

(def adapters
  (let [sorted (sort input)
        lo 0
        hi (+ (apply max sorted) 3)]
    (vec (concat [lo] sorted [hi]))))

(->> adapters
     (partition 2 1)
     (map (partial apply -))
     frequencies
     (#(select-keys % [-1 -3]))
     vals
     (apply *))
;; => 2376

(def adapters1
  (subvec adapters 0 50))

(def adapters2
  (subvec adapters 50 72))

(def adapters3
  (subvec adapters 72))

(def counter (atom 0))

(defn find-paths [adapters i]
  (let [curr (get adapters i)
        next1 (get adapters (inc i))
        next2 (get adapters (+ i 2))
        next3 (get adapters (+ i 3))]
    (if (= i (dec (count adapters)))
      (swap! counter inc)
      (concat (when (and next1 (<= (- next1 curr) 3))
                [(find-paths adapters (inc i))])
              (when (and next2 (<= (- next2 curr) 3))
                [(find-paths adapters (+ i 2))])
              (when (and next3 adapters (<= (- next3 curr) 3))
                [(find-paths adapters (+ i 3))])))))

(do (find-paths adapters3 0) nil)

;; adapters1:
@counter
;; => 1404928

;; adapters2
@counter
;; => 1372

;; adapters3
@counter
;; => 67228


(* 1404928 1372 67228)
;; => 129586085429248
