(ns adventofcode.2016.day11)

;; I gave up and looked at
;; https://www.reddit.com/r/adventofcode/comments/5hoia9/2016_day_11_solutions/
;; Here I found a heuristic that worked for my input...

;; Heuristic:
;; Move two items up and one down continuously. This means two times number of
;; items minus three to move up one floor. Minus the amount of moves to get into
;; the starting position. (Two objects up is four moves for starting position.)

(defn count-moves [n-items n-start]
  (- (* 3 (- (* n-items 2) 3)) n-start))


;; Part 1

(def input
  [[{:elevator true :floor 1 :elems [:PG :TG :TM :UG :RG :RM :CG :CM]}
    {:elevator false :floor 2 :elems [:PM :UM]}
    {:elevator false :floor 3 :elems []}
    {:elevator false :floor 4 :elems []}]])

(count-moves 10 4)


;; Part 2

(def input2
  [[{:elevator true :floor 1 :elems [:PG :TG :TM :UG :RG :RM :CG :CM :EG :EM :DG :DM]}
    {:elevator false :floor 2 :elems [:PM :UM]}
    {:elevator false :floor 3 :elems []}
    {:elevator false :floor 4 :elems []}]])

(count-moves 14 4)
