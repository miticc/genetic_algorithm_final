(ns evolution.evolution
  (:use
    clojure.core
    clojure.pprint
    genomes.genomes
    algorithm.algorithm
    criterium.core))


(defn get-result-list
  "Returns a list containing fitness value - genome pairs"
  [generation]
  (let [result-list  []]
    (for [genome generation]
      (conj result-list (- 1 (fitness genome)) genome))))
;(with-progress-reporting (bench (get-result-list [[1 0 1 1 1 0 1 0 0 0 0 0 1 0 1 1 1 0 1 0] [1 0 1 0 1 0 1 0 0 0 1 0 0 1 1 1 1 1 1 0]])) ))

(defn get-sorted-list
  "Returns a sorted list containing fitness value - genome pairs"
  [generation]  
  (let [list (get-result-list generation)]
    (sort list)))
(get-sorted-list  [[1 0 1 1 1 0 1 0 0 0 0 0 1 0 1 1 1 0 1 0] [1 0 1 0 1 0 1 0 0 0 1 0 0 1 1 1 1 1 1 0]])

;(with-progress-reporting (bench (get-sorted-list  [[1 0 1 1 1 0 1 0 0 0 0 0 1 0 1 1 1 0 1 0] [1 0 1 0 1 0 1 0 0 0 1 0 0 1 1 1 1 1 1 0]])))

(defn get-best-unit
  "Takes the best unit (genome) from a population and its fitness value"
  [generation]
  (let [result-list (get-sorted-list generation)
        best-unit []]
    (conj best-unit (- 1 (first (first result-list))) (second (first (sort result-list))))))

;(with-progress-reporting (bench (get-best-unit (create-generation 10))))

(defn create-genomes-list 
  "Creates sorted list of genomes based on result list"
  [generation]
  (let [genomes-list []
        result-list (get-sorted-list generation)]
    (for [genome result-list]  
      (flatten (conj genomes-list (get genome 1))))))

;(with-progress-reporting (bench (create-genomes-list (create-generation 5))))

(defn get-candidates
  "Returns best n% of units (genomes) in the generation"
  [percent generation]
  (let [genomes-list (into [] (create-genomes-list generation))]
    (take (Math/round (* percent (count generation))) genomes-list)))

;(with-progress-reporting (bench get-candidates 0.3 (create-generation 10)))

(defn mutate
  "Mutates n% of units (genomes) in the generation"
  [percent generation]
  (let [new-generation []]
    (for [candidate (get-candidates percent generation)]  
      (let [genome (atom (into [] candidate))]
        (loop [i 4]
          (when (< i (count candidate))
            (swap! genome assoc i (if (= (get @genome i) 0) 1 0))
            (recur (+ i 5))))
        (flatten (conj new-generation @genome))))))

;(with-progress-reporting (bench mutate 0.3 (create-generation 10)))

(defn create-child
  "Takes 60% of father's genes and 40% of mother's genes and creates a child"
  [father mother]
  (let [child []]
    (into [] (flatten (conj (take-last 12 mother) (take 8 father))))))

;(with-progress-reporting (bench create-child [1 2 3 7 7 7 7] [1 1 1 5 4 8 8 8]))

(defn crossover 
  "Takes two by two best parents in the population and 
   performs a crossover on n% of the genome length to produce one child"
  [percent generation]
  (let [new-generation []
        popsize (count generation)]
    (for [i (range (Math/round (* percent popsize)))] 
      (concat new-generation (create-child (get generation i) (get generation (inc i)))))))

;(with-progress-reporting (bench crossover 0.3 (create-generation 10)))

(defn evolve 
  "Evolves population with the specific parameters until the target is reached or maximum number of iterations is exceeded"
  [popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
  (let [winner (atom [])]
    (loop [i 0
           generation (create-generation popsize)
           success false]
      (when (and (< i max-iterations) (false? success))
        (let [best-unit (get-best-unit generation)]
          (if (> (get best-unit 0) max-error) 
            (do 
              (pprint "Evolution successfully finished in iteration:")
              (pprint i)
              (pprint "Optimal path is:")
              (pprint (get best-unit 1))
              (pprint "Algorithm error is:")
              (pprint (get best-unit 0))
              (reset! winner (get best-unit 1))
              (recur i generation true))
            (let [new-generationn (for [i (concat (get-candidates 0.2 generation) (mutate 0.3 generation) (crossover 0.5 generation)(create-generation 4))]
                                  (into [] i))]
              ;            (pprint "iteration")
              ;            (pprint i)         
              (recur (inc i) new-generationn false)))))) @winner))

;(with-progress-reporting (crossover 0.3 (bench (evolve 10 0.2 0.4 0.2 0.95 10))))

(defn evolve-result 
  "Creates a path based on results of the evloution"
  [popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
  (let [aresult (atom [[0 0]])
        winner (evolve popsize percent-survival percent-mutation percent-crossover max-error max-iterations)]  
    (if (not= winner [])
      (loop [i 0
             position [0 0]]
        (when (<= i 18)
          (let [p (current-field position [(get winner i) (get winner (inc i))])]
            (swap! aresult conj p)     
            (recur (+ i 2) p))))) @aresult))
;(evolve-result 10 0.2 0.4 0.2 0.97 100)
(with-progress-reporting (bench (evolve-result 10 0.2 0.4 0.2 0.97 100) :verbose))