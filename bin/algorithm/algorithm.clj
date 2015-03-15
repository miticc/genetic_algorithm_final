(ns algorithm.algorithm
  (:use clojure.core
        clojure.pprint
        board.board))

(def start (atom [0 0]))
(def no-down (atom 0))
(def no-left (atom 0))
(def finish [3 2])
(def max-distance 30)
(def no-penalties (atom 0))

(defn current-position
  "Calculates the final position after ecexution of all steps in the genome"
  [genome]
  (do
    (reset! start [0 0])
    (reset! no-down 0)
    (reset! no-left 0)
    (reset! no-penalties 0
            )
    (loop [i 0]
      (when (< i (count genome))
      (let [[x y] [(get genome i) (get genome (inc i))]]
        (case [x y]
          [0 0] (if 
                  (or 
                    (= (get-in final-board [(inc (get @start 0)) (get @start 1)]) "bl")
                    (= (get-in final-board [(inc (get @start 0)) (get @start 1)]) nil))
                  (swap! no-penalties inc)
                  (swap! start assoc 0 (inc (get @start 0))))  
          [0 1] (if 
                  (or 
                    (= (get-in final-board [(get @start 0) (inc (get @start 1))]) "bl")
                    (= (get-in final-board [(get @start 0) (inc (get @start 1))]) nil))
                  (swap! no-penalties inc)
                  (swap! start assoc 1 (inc (get @start 1)))) 
          [1 0] (if 
                  (or 
                    (= (get-in final-board [(dec (get @start 0)) (get @start 1)]) "bl")
                    (= (get-in final-board [(dec (get @start 0)) (get @start 1)]) nil))
                  (swap! no-penalties inc)
                  (do
                    (swap! no-down inc)
                    (swap! start assoc 0 (dec (get @start 0)))))
          [1 1] (if 
                  (or 
                    (= (get-in final-board [(get @start 0) (dec (get @start 1))]) "bl")
                    (= (get-in final-board [(get @start 0) (dec (get @start 1))]) nil))
                  (swap! no-penalties inc)
                  (do
                    (swap! no-left inc)
                    (swap! start assoc 1 (dec (get @start 1))))))
        (recur (+ i 2)))))))

(defn distance 
  "Calculates the distance between the final position and target field"
  [genome]
  (do 
    (current-position genome)
      (+ (- (get finish 1) (get @start 1)) (- (get finish 0) (get @start 0)))
    ))

(defn fitness
  "Calculates the fitness of the genome"
  [genome]
  (let 
    [down-left (if 
                 (and (>= 1 @no-down) (>= 1 @no-left)) 
                 0
                 0.2)
     penalties (* @no-penalties 0.06)]
  (- (- 1 (/ (distance genome) max-distance)) down-left penalties)
 ))

(defn current-field
  "Calculates next position based on current position and given step"
  [startp [x y]]
  (let [p startp]
    (case [x y]
      [0 0] (if-not 
              (or 
                    (= (get-in final-board [(inc (get startp 0)) (get startp 1)]) "bl")
                    (= (get-in final-board [(inc (get startp 0)) (get startp 1)]) nil))
              (assoc p 0 (inc (get startp 0)))
              p)
      [0 1] (if-not 
              (or 
                    (= (get-in final-board [(get startp 0) (inc (get startp 1))]) "bl")
                    (= (get-in final-board [(get startp 0) (inc (get startp 1))]) nil))
                  
              (assoc p 1 (inc (get startp 1)))
              p) 
      [1 0] (if-not  
              (or 
                    (= (get-in final-board [(dec (get startp 0)) (get startp 1)]) "bl")
                    (= (get-in final-board [(dec (get startp 0)) (get startp 1)]) nil))
              (assoc p 0 (dec (get startp 0)))
              p)
      [1 1] (if-not 
              (or 
                    (= (get-in final-board [(get startp 0) (dec (get startp 1))]) "bl")
                    (= (get-in final-board [(get startp 0) (dec (get startp 1))]) nil))
              (assoc p 1 (dec (get startp 1)))
              p))))
  



