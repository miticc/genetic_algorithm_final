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

;(current-position [0 0 0 1 0 0 1 1 1 1 1 1])

(defn distance 
  "Calculates the distance between the final position and target field"
  [genome]
  (let [curr-pos (current-position genome)
        finish [3 2]]
    [(+ (- (nth finish 1) (nth (nth curr-pos 0) 1)) (- (nth finish 0) (nth (nth curr-pos 0) 0))) curr-pos]))

;(distance [0 0 0 1 0 0 1 1 1 1 1 1 0 0 1 0 1 1 0 1])

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
    (- (- 1 (/ dist max-distance)) (if (and (>= 1 no-down) (>= 1 no-left)) 0 0.2) (* no-pen 0.06))))

;(fitness [0 0 0 1 0 0 1 1 1 1 1 1 0 0 1 0 1 1 0 1])
;(with-progress-reporting (bench (fitness [0 0 0 1 0 0 1 1 1 1 1 1 0 0 1 0 1 1 0 1]) ))

(defn create-unit
  "kreira jednu jedinku i postavlja vrednosti poljima"
  [length]
  (let [genome (into [] (create-genome length)) 
        fitness (fitness  genome)
        unit []]
    (conj unit fitness genome)
  ;(->Unit genome fitness)
  ))

(create-unit 20)
;(with-progress-reporting (bench (create-unit 20) ))

(defn create-generation
  "Creates generation of n genomes, each genome length is 10 steps (10x2 fields)"
  [n]
  (take n (repeatedly #(create-unit 14))))

(take-last 5 (sort (create-generation 10)))
;(sort-by :fitness_value (create-generation 10))
;(with-progress-reporting (bench (take-last 5 (sort-by :fitness_value (create-generation 10)))))
;(with-progress-reporting (bench (take-last 5 (sort (create-generation 10)))))


(defn curr-pos-new
  ([genome]
    (curr-pos-new (into [] genome) 0 0 0 0 0));)
  ([genome startx starty down left no-penalties]
    (if (not= (count genome) 0)
      (let [vec (into [] (take 2 genome))]
        (do 
          (case vec
            [0 0] (if (or 
                        (= (get-in final-board [(inc startx) starty]) "bl")
                        (= (get-in final-board [(inc startx) starty]) nil))
                    (recur (subvec genome 2) startx starty down left (inc no-penalties))
                    (recur (subvec genome 2) (inc startx) starty down left  no-penalties))
            [0 1] (if (or 
                       (= (get-in final-board [startx (inc starty)]) "bl")
                       (= (get-in final-board [startx (inc starty)]) nil))
                    (recur (subvec genome 2) startx starty down left (inc no-penalties))
                    (recur (subvec genome 2) startx (inc starty) down left no-penalties))
            [1 0] (if (or 
                       (= (get-in final-board [(dec startx) starty]) "bl")
                       (= (get-in final-board [(dec startx) starty]) nil))
                   (recur (subvec genome 2) startx starty down left (inc no-penalties))
                   (recur (subvec genome 2) (dec startx) starty (inc down) left no-penalties))
            [1 1] (if (or 
                       (= (get-in final-board [startx (dec starty)]) "bl")
                       (= (get-in final-board [startx (dec starty)]) nil))
                    (recur (subvec genome 2) startx starty down left (inc no-penalties))
                    (recur (subvec genome 2) startx (dec starty) down (inc left) no-penalties)))
      )))))

;(curr-pos-new (into [] la))
;(with-progress-reporting (bench (curr-pos-new (into [] la))))

(defn fitness-new
  ([genome]
    (let [pen (atom 0)]
    (fitness-new (into [] genome) 0 0 pen)))
  ([genome x y penalties]
    (if (not= (count genome) 0)
      (let [vec (take 2 genome)]
        (do 
          (if (or 
                (= (get-in final-board [(+ (nth vec 0) x ) (+ (nth vec 1) y )] "bl"))
                (= (get-in final-board [(+ (nth vec 0) x ) (+ (nth vec 1) y )] "nil"))
                (some #(= -1 %) vec))
            (swap! penalties + 0.1))
          (recur (subvec genome 2) (+ (nth vec 0) x ) (+ (nth vec 1) y ) penalties)))
     ;racunanje fitnessa na osnovu penalt
      )))
;(pprint (def la (create-genome 10)))
        
;(fitness-new (into [] la))
;(subvec (into [] (create-genome 6)) 2)


;(with-progress-reporting (bench (fitness-new (into [] (create-genome 10)))))
