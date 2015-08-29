(ns genomes.genomes_opt  
  (:use clojure.core
        criterium.core
        board.board))

(defn create-genome-pen
  "Creates genome as a sequence of n random chosen 0 or 1"
  [length]
  (take length (repeatedly #(rand-int 2))))

(defn current-position-pen
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
            [0 0] (if (= (get-in final-board-new [(inc @startx) @starty]) "f")
                    (do
                      (reset! no-penalties -5)
                      (swap! startx inc))
                    (if (not= (get-in final-board-new [(inc @startx) @starty]) :on)
                      (if (= (get-in final-board-new [(inc @startx) @starty]) nil)                                        
                        (swap! no-penalties + 0.1)
                        (swap! no-penalties + (get-in final-board-new [(inc @startx) @starty])))
                      (swap! startx inc)))
            [0 1] (if (= (get-in final-board-new [@startx (inc @starty)]) "f")
                    (do
                      (reset! no-penalties -5)
                      (swap! startx dec))
                    (if (not= (get-in final-board-new [@startx (inc @starty)]) :on)
                      (if (= (get-in final-board-new [@startx (inc @starty)]) nil)
                        (swap! no-penalties + 0.1)
                        (swap! no-penalties + (get-in final-board-new [@startx (inc @starty)])))
                      (swap! starty inc))) 
            [1 0] (if (= (get-in final-board-new [(dec @startx) @starty]) "f")
                    (do
                      (reset! no-penalties -5)
                      (swap! startx dec))
                    (if (not= (get-in final-board-new [(dec @startx) @starty]) :on)
                      (if (= (get-in final-board-new [(dec @startx) @starty]) nil)
                        (swap! no-penalties + 0.1)
                        (swap! no-penalties + (get-in final-board-new [(dec @startx) @starty])))
                      (do
                        (swap! no-down inc)
                        (swap! startx dec))))
            [1 1] (if (= (get-in final-board-new [@startx (dec @starty)]) "f")
                    (do 
                      (reset! no-penalties -5)
                      (swap! starty dec))
                    (if (not= (get-in final-board-new [@startx (dec @starty)]) :on)
                      (if (= (get-in final-board-new [@startx (dec @starty)]) nil)
                        (swap! no-penalties + 0.1)
                        (swap! no-penalties + (get-in final-board-new [@startx (dec @starty)])))
                      (do
                        (swap! no-left inc)
                        (swap! starty dec)))))
          (recur (+ i 2)))))
    [[@startx @starty] @no-down @no-left @no-penalties]))

(defn distance-pen
  "Calculates the distance between the final position and target field"
  [genome]
  (let [curr-pos (current-position-pen genome)
        finish [4 3]]
    [(+ (- (nth finish 1) (nth (nth curr-pos 0) 1)) (- (nth finish 0) (nth (nth curr-pos 0) 0))) curr-pos]))

(defn fitness-pen
  "Calculates the fitness of the genome"
  [genome]
  (let 
    [max-distance 30
     distance (distance-pen genome)
     dist (nth distance 0)
     no-down (nth (nth distance 1) 1)
     no-left (nth (nth distance 1) 2)
     no-pen (nth (nth distance 1) 3)]
    (- 1 (* dist 0.05) no-pen (if (or (> no-left 0 ) (> no-down 0)) 0.1 0))))

(defn create-unit-pen
  "Creates unit as a conjunction of the genome and its fitness value"
  [length]
  (let [genome (into [] (create-genome-pen length)) 
        fitness (fitness-pen  genome)
        unit []]
    (conj unit fitness genome)))

(defn create-generation-pen
  "Creates generation of n genomes, each genome length is 10 steps (10x2 fields)"
  [n]
  (take n (repeatedly #(create-unit-pen 20))))

(create-generation-pen 20)

(defn create-sorted-generation-pen
  "Creates generation of n genomes, each genome length is 10 steps (10x2 fields) and sorts it"
  [n]
  (sort (take n (repeatedly #(create-unit-pen 20)))))

(defn current-field-pen
  "Calculates next position based on current position and given step"
  [startp [x y]]
  (let [px (nth startp 0)
        py (nth startp 1)]
    (case [x y]
      [0 0] (if (or 
                  (= (get-in final-board-new [(inc px) py]) :on)
                  (= (get-in final-board-new [(inc px) py]) "f"))
              [(inc px) py]
              [px py])
      [0 1] (if (or 
                  (= (get-in final-board-new [px (inc py)]) :on)
                  (= (get-in final-board-new [px (inc py)]) "f"))                  
              [px (inc py)]
              [px py])
      [1 0] (if (or 
                  (= (get-in final-board-new [(dec px) py]) :on)
                  (= (get-in final-board-new [(dec px) py]) "f"))
              [(dec px) py]
              [px py])
      [1 1] (if (or 
                  (= (get-in final-board-new [px (dec py)]) :on)
                  (= (get-in final-board-new [px (dec py)]) "f"))
              [px (dec py)]
              [px py]))))
