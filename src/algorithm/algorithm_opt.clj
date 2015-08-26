(ns algorithm.algorithm_opt
  (:use clojure.core
        clojure.pprint
        board.board
        criterium.core))
(comment
(defn current-field
  "Calculates next position based on current position and given step"
  [startp [x y]]
  (let [px (get startp 0)
        py (get startp 1)]
    (case [x y]
      [0 0] (if-not (or 
                      (= (get-in final-board [(inc px) py]) "bl")
                      (= (get-in final-board [(inc px) py]) nil))
              [(inc px) py]
              [px py])
      [0 1] (if-not (or 
                      (= (get-in final-board [px (inc py)]) "bl")
                      (= (get-in final-board [px (inc py)]) nil))                  
              [px (inc py)]
              [px py])
      [1 0] (if-not (or 
                      (= (get-in final-board [(dec px) py]) "bl")
                      (= (get-in final-board [(dec px) py]) nil))
              [(dec px) py]
              [px py])
      [1 1] (if-not (or 
                      (= (get-in final-board [px (dec py)]) "bl")
                      (= (get-in final-board [px (dec py)]) nil))
              [px (dec py)]
              [px py]))))
)
;(current-field [3 0] [0 0])
;(with-progress-reporting (bench (current-field [3 0] [0 0]) ))