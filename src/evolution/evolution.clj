(ns evolution.evolution
  (:use
    clojure.core
    clojure.pprint
    genomes.genomes
    algorithm.algorithm))


(defn get-result-list
  "Returns a list containing fitness value - genome pairs"
  [generation]
  (let [result-list []]
    (for [genome generation]
      (conj result-list (- 1 (fitness genome)) genome))))

(defn get-best-unit
  "Takes the best unit (genome) from a population and its fitness value"
  [generation]
  (let [result-list (get-result-list generation)
        best-unit []]
    (conj best-unit 
          (- 1 (get (first (sort result-list)) 0)) (get (first (sort result-list)) 1))))

(defn create-genomes-list 
  "Creates sorted list of genomes based on result list"
  [generation]
  (let [genomes-list []]
    (for [genome (sort (get-result-list generation))]  
      (flatten (conj genomes-list (get genome 1)))
      )))

(defn get-candidates
  "Returns best n% of unit (genomes) in the generation"
  [percent generation]
  (let [genomes-list (into [] (create-genomes-list generation))]
    (take (Math/round (* percent (count generation))) genomes-list)))

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
      
(defn create-child
  "Takes 60% of father's genes and 40% of mother's genes and creates a child"
  [father mother]
  (let [child []]
    (into [] (flatten (conj (take-last 12 mother) (take 8 father))))))

(defn crossover 
  "Takes two by two best parents in the population and 
   performs a crossover on n% of the genome length to produce one child"
  [percent generation]
  (let [new-generation []
        popsize (count generation)]
    (for [i (range (Math/round (* percent popsize)))] 
    (concat new-generation
           (create-child (get generation i) (get generation (inc i)))))))


(def winner (atom []))

(defn evolve 
  "Evolves population with the specific parameters until the target is reached or maximum number of iterations is exceeded"
  [popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
  (loop [i 0
         generation (create-generation popsize)
         success false]
    (when (and (< i max-iterations) (false? success))
      (let [best-unit (get-best-unit generation)]
        (if (> (get best-unit 0) max-error) 
          (do 
            (pprint "uspesno je zavrsena evloucija, optimalno kretanje je")
            (pprint (get best-unit 1))
            (pprint "greska je")
            (pprint (get best-unit 0))
            (reset! winner (get best-unit 1))
            (recur i generation true))
          (let [new-generationn 
                        (into [] 
                              (for [i (concat 
                                        (get-candidates 0.2 generation)
                                        (mutate 0.3 generation)
                                        (crossover 0.5 generation)
                                        (create-generation 4))]
                                (into [] i)))]
            (pprint "iteracija je")
            (pprint i)         
            (recur (inc i) new-generationn false)))))
    ))


(def aresult (atom [[0 0]]))

(defn evolve-result 
  "Creates a path based on results of the evloution"
  [popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
  (do
    (reset! aresult [[0 0]])
    (evolve popsize percent-survival percent-mutation percent-crossover max-error max-iterations)
    (loop [i 0
           position [0 0]
           ]
      (when (<= i 18)
        (let [p (current-field position [(get @winner i) (get @winner (inc i))])]
         (swap! aresult conj p)     
          (recur (+ i 2) p )))))
  @aresult)


