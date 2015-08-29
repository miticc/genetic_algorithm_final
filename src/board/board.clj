(ns board.board
  (:use clojure.pprint))

(defn board-set 
  "Creates a board"
  [w h]
  (vec (repeat w (vec (repeat h :on)))))


(defn board-fill
  "Fills board with blocks"
  [board coord string]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates string))
          board
          coord))

(def final-board-new
  "Creates final board"
  (board-fill (board-fill (board-fill (board-fill (board-fill (board-fill (board-fill (board-set 6 5) [[5 2]] 0.15)
                                                                          [[4 2]] 0.1)
                                                              [[2 4]] 0.4)
                                                  [[2 3]] 0.3)
                                      [[2 2]] 0.15) 
                          [[2 1]] 0.1) 
              [[4 3]] "f"))
