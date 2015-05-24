(ns algorithm.algorithm
  (:use clojure.core
        clojure.pprint
        board.board))

(defn current-position
  "Calculates the final position after ecexution of all steps in the genome"
  [genome]
  (let [startx (atom 0)
        starty (atom 0)
        no-down (atom 0)
        no-left (atom 0)
        no-penalties (atom 0)]
    (loop [i 0]
      (when (< i (count genome))
      (let [[x y] [(get genome i) (get genome (inc i))]]
        (case [x y]
          [0 0] (if 
                  (or 
                    (= (get-in final-board [(inc @startx) @starty]) "bl")
                    (= (get-in final-board [(inc @startx) @starty]) nil))
                  (swap! no-penalties inc)
                  (swap! startx inc))  
          [0 1] (if 
                  (or 
                    (= (get-in final-board [@startx (inc @starty)]) "bl")
                    (= (get-in final-board [@startx (inc @starty)]) nil))
                  (swap! no-penalties inc)
                  (swap! starty inc)) 
          [1 0] (if 
                  (or 
                    (= (get-in final-board [(dec @startx) @starty]) "bl")
                    (= (get-in final-board [(dec @startx) @starty]) nil))
                  (swap! no-penalties inc)
                  (do
                    (swap! no-down inc)
                    (swap! startx dec)))
          [1 1] (if 
                  (or 
                    (= (get-in final-board [@startx (dec @starty)]) "bl")
                    (= (get-in final-board [@startx (dec @starty)]) nil))
                  (swap! no-penalties inc)
                  (do
                    (swap! no-left inc)
                    (swap! starty dec))))
        (recur (+ i 2)))))
    [[@startx @starty] @no-down @no-left @no-penalties]))


(defn distance 
  "Calculates the distance between the final position and target field"
  [genome]
  (let [curr-pos (current-position genome)
        finish [3 2]]
     [(+ (- (get finish 1) (get (get curr-pos 0) 1)) (- (get finish 0) (get (get curr-pos 0) 0)))
      curr-pos]))

(defn fitness
  "Calculates the fitness of the genome"
  [genome]
  (let 
    [max-distance 30
     distance (distance genome)
     dist (get distance 0)
     no-down (get (get distance 1) 1)
     no-left (get (get distance 1) 2)
     no-pen (get (get distance 1) 3)
     ]
 (- (- 1 (/ dist max-distance)) (if 
                 (and (>= 1 no-down) (>= 1 no-left)) 
                 0
                 0.2)
    (* no-pen 0.06))
 ))

(defn current-field
  "Calculates next position based on current position and given step"
  [startp [x y]]
  (let [px (get startp 0)
        py (get startp 1)]
    (case [x y]
      [0 0] (if-not 
              (or 
                    (= (get-in final-board [(inc px) py]) "bl")
                    (= (get-in final-board [(inc px) py]) nil))
              [(inc px) py]
              [px py])
      [0 1] (if-not 
              (or 
                    (= (get-in final-board [px (inc py)]) "bl")
                    (= (get-in final-board [px (inc py)]) nil))                  
              [px (inc py)]
              [px py])
      [1 0] (if-not  
              (or 
                    (= (get-in final-board [(dec px) py]) "bl")
                    (= (get-in final-board [(dec px) py]) nil))
              [(dec px) py]
              [px py])
      [1 1] (if-not 
              (or 
                    (= (get-in final-board [px (dec py)]) "bl")
                    (= (get-in final-board [px (dec py)]) nil))
              [px (dec py)]
              [px py])
)
  ))