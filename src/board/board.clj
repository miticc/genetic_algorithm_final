(ns board.board
  (:use clojure.pprint))

(defn board-set 
  "Creates a board"
  [w h]
  (vec (repeat w (vec (repeat h :on))))
  )


(defn board-fill
  "Fills board with blocks"
  [board coord string]
  (reduce 
    (fn [board coordinates]
      (assoc-in board coordinates string))
    board
    coord))

(def final-board
  "Creates final board"
  (board-fill 
    (board-fill (board-set 6 5) [[2 1][2 2][2 3][2 4][4 2][5 2]] "bl") [[4 3]] "f"))



