(ns genomes.genomes_opt  
  (:use clojure.core
        clojure.pprint
        criterium.core
        board.board))

(defn create-genome 
  "Creates genome as a sequence of n random chosen 0 or 1"
  [length]
  (take length (repeatedly #(rand-int 2))))
(create-genome 20)

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
            [0 0] (if (or 
                        (= (get-in final-board [(inc @startx) @starty]) "bl")
                        (= (get-in final-board [(inc @startx) @starty]) nil))
                    (swap! no-penalties inc)
                    (swap! startx inc))  
            [0 1] (if (or 
                        (= (get-in final-board [@startx (inc @starty)]) "bl")
                        (= (get-in final-board [@startx (inc @starty)]) nil))
                    (swap! no-penalties inc)
                    (swap! starty inc)) 
            [1 0] (if (or 
                        (= (get-in final-board [(dec @startx) @starty]) "bl")
                        (= (get-in final-board [(dec @startx) @starty]) nil))
                    (swap! no-penalties inc)
                    (do
                      (swap! no-down inc)
                      (swap! startx dec)))
            [1 1] (if (or 
                        (= (get-in final-board [@startx (dec @starty)]) "bl")
                        (= (get-in final-board [@startx (dec @starty)]) nil))
                    (swap! no-penalties inc)
                    (do
                      (swap! no-left inc)
                      (swap! starty dec))))
          (recur (+ i 2))))) 
    [[@startx @starty] @no-down @no-left @no-penalties]))

;(current-position [1 0 1 1 0 1 1 0 1 0 1 1 1 1 1 0])
;(count [1 0 1 1 0 1 1 0 1 0 1 1 1 1 1 0])

(defn distance-pen
  "Calculates the distance between the final position and target field"
  [genome]
  (let [curr-pos (current-position-pen genome)
        finish [4 3]]
    [(+ (- (nth finish 1) (nth (nth curr-pos 0) 1)) (- (nth finish 0) (nth (nth curr-pos 0) 0))) curr-pos]))

(defn distance 
  "Calculates the distance between the final position and target field"
  [genome]
  (let [curr-pos (current-position genome)
        finish [4 3]]
    [(+ (- (nth finish 1) (nth (nth curr-pos 0) 1)) (- (nth finish 0) (nth (nth curr-pos 0) 0))) curr-pos]))

;(distance [0 1 1 1 0 0 0 1 1 0 0 1 0 1 0 0])

(defn fitness
  "Calculates the fitness of the genome"
  [genome]
  (let 
    [max-distance 30
     distance (distance genome)
     dist (nth distance 0)
     no-down (nth (nth distance 1) 1)
     no-left (nth (nth distance 1) 2)
     no-pen (nth (nth distance 1) 3)]
    (- (- 1 (/ dist max-distance)) (if (and (>= 1 no-down) (>= 0 no-left)) 0 0.1) (* no-pen 0.06))))

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

;(with-progress-reporting (bench (fitness [0 0 0 1 0 0 1 1 1 1 1 1 0 0 1 0 1 1 0 1]) ))

(defn create-unit
  "kreira jednu jedinku i postavlja vrednosti poljima"
  [length]
  (let [genome (into [] (create-genome length)) 
        fitness (fitness  genome)
        unit []]
    (conj unit fitness genome)))

(defn create-unit-pen
  "kreira jednu jedinku i postavlja vrednosti poljima"
  [length]
  (let [genome (into [] (create-genome length)) 
        fitness (fitness-pen  genome)
        unit []]
    (conj unit fitness genome)))

;(create-unit 20)
;(with-progress-reporting (bench (create-unit 20) ))

(defn create-generation
  "Creates generation of n genomes, each genome length is 10 steps (10x2 fields)"
  [n]
  (take n (repeatedly #(create-unit 20))))

(defn create-generation-pen
  "Creates generation of n genomes, each genome length is 10 steps (10x2 fields)"
  [n]
  (take n (repeatedly #(create-unit-pen 20))))

(defn create-sorted-generation
  "Creates generation of n genomes, each genome length is 10 steps (10x2 fields)"
  [n]
  (sort (take n (repeatedly #(create-unit 20)))))

(defn create-sorted-generation-pen
  "Creates generation of n genomes, each genome length is 10 steps (10x2 fields)"
  [n]
  (sort (take n (repeatedly #(create-unit-pen 20)))))

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

(defn current-field-pen
  "Calculates next position based on current position and given step"
  [startp [x y]]
  (let [px (nth startp 0)
        py (nth startp 1)]
    (case [x y]
      [0 0] (if (or 
                  (= (get-in final-board [(inc px) py]) :on)
                  (= (get-in final-board [(inc px) py]) "f"))
              [(inc px) py]
              [px py])
      [0 1] (if (or 
                  (= (get-in final-board [px (inc py)]) :on)
                  (= (get-in final-board [px (inc py)]) "f"))                  
              [px (inc py)]
              [px py])
      [1 0] (if (or 
                  (= (get-in final-board [(dec px) py]) :on)
                  (= (get-in final-board [(dec px) py]) "f"))
              [(dec px) py]
              [px py])
      [1 1] (if (or 
                  (= (get-in final-board [px (dec py)]) :on)
                  (= (get-in final-board [px (dec py)]) "f"))
              [px (dec py)]
              [px py]))))

;(pprint (def la (create-genome 10)))
        
;(fitness-new [0 0 0 0 0 0 1 1 0 1 0 0 0 0 0 0])
;(subvec (into [] (create-genome 6)) 2)


;(with-progress-reporting (bench (fitness-new (into [] (create-genome 10)))))
