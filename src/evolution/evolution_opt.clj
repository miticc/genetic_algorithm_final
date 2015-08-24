(ns evolution.evolution_opt
  (:use
    clojure.core
    clojure.pprint
    genomes.genomes_opt
    algorithm.algorithm_opt
    criterium.core))

(comment
(defn get-best-unit
  "Takes the best unit (genome) from a population and its fitness value"
  [generation]
  (let [result-list (get-sorted-list generation)
        best-unit []]
    (conj best-unit (- 1 (first (first result-list))) (second (first result-list)))))

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
)
(defn mutate
  "Mutates n% of units (genomes) in the generation"
  [percent generation]
  (let [new-generation []
        pop-size (count generation)]
    (for [candidate (take-last (Math/round (* percent pop-size)) generation)]  
      (let [genome (atom (nth candidate 1))]
        (loop [i 2]
          (when (< i (count candidate))
            (swap! genome assoc i (if (= (get @genome i) 0) 1 0))
            (recur (+ i 3))))
        (conj new-generation (fitness @genome) @genome)))))

;(with-progress-reporting (bench mutate 0.3 (create-generation 10)))

(defn create-child
  "Takes 60% of father's genes and 40% of mother's genes and creates a child"
  [father mother]
    (into [] (concat (take-last 12 mother) (take 8 father))))

;(with-progress-reporting (bench create-child [0 0 0 1 0 0 1 1 1 1 1 1 0 0 1 0 1 1 0 1] [0 1 1 1 0 0 1 1 1 0 1 1 1 0 1 1 1 1 0 1]))

(defn crossover 
  "Takes two by two best parents in the population and 
   performs a crossover on n% of the genome length to produce one child"
  [percent generation]
  (let [popsize (count generation)
        gen (take-last (Math/round (* percent popsize)) generation)]
    (loop [i 0
           new-generation []]
      (if (< i (dec (count gen)))
        (do
          (let [child (create-child (nth (nth gen i) 1) (nth (nth gen (inc i)) 1))]
            (recur (inc i) (conj new-generation (fitness child) child))))
       (conj [] new-generation)))))

;(with-progress-reporting (bench crossover 0.3 (create-generation 10)))
(defn evolve-new 
  "Evolves population with the specific parameters until the target is reached or maximum number of iterations is exceeded"
  ([popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
    (let [generation (create-sorted-generation popsize)]
          (evolve-new popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation false 1)))
    ([popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation success iteration]
      (if (and (< iteration max-iterations) (false? success))
        (if (> (nth (last generation) 0) max-error)
          (do
            ;(pprint (last generation))
            ;(pprint iteration)
            ;(pprint generation)
            (recur popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation true iteration))
          (do
            (pprint iteration)
            (let [survivors (take-last (Math/round (* percent-survival popsize)) generation)
                  mutants (mutate percent-mutation generation)
                  crossed-over (crossover percent-crossover generation)
                  new-ones (create-generation (* (- 1 percent-survival percent-mutation percent-crossover -0.1) 10))
                  new-generation (for [i (create-generation (Math/round (* 10 (- 1.1 percent-survival percent-mutation percent-crossover))))] (into [] i));]
                  ]
              ;(pprint iteration)
              ;(pprint survivors)
              ;(pprint mutants)
              ;(pprint crossed-over)
              ;(pprint (sort (concat [] survivors mutants crossed-over new-generation)))
            (recur popsize percent-survival percent-mutation percent-crossover max-error max-iterations 
                   (sort (concat [] survivors mutants crossed-over new-ones))
                   false (inc iteration)))))
        (last generation)
        )))
(evolve-new 10 0.2 0.4 0.2 0.95 10)
(comment
(evolve-new 10 0.2 0.4 0.2 0.95 10)

(create-generation 3.0001)
(def new (create-generation (* (- 1 (+ 0.2 0.4 0.2 -0.1 )) 10)))
(def generation (concat [] (take-last (Math/round (* 0.2 10)) la)))
(nth (nth generation 9) 0)
(last generation)
(mutate 0.4 la)
(def generation (create-generation 10))
(Math/round 10)
generation
(def sur (take-last 2 generation))
sur
(def mut (mutate 0.4 generation))
mut
(def cross (crossover 0.2 generation))
(concat [] mut cross sur)
cross
(def new (create-generation 4))
(concat sur mut cross new)
(def la (concat sur mut cross))
(sort la)
 (first (last la))
(count la)
;(sort (conj sur mut cross new))


(with-progress-reporting (bench crossover 0.3 (evolve-new 10 0.2 0.4 0.2 0.95 200)))
(create-sorted-generation 10)
)
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

(defn evolve-result-new
[popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
  (let [aresult (atom [[0 0]])
        winner (evolve popsize percent-survival percent-mutation percent-crossover max-error max-iterations)]  
    (if (not= winner [])
      (loop [i 0
             position [0 0]]
        (when (<= i 16)
          (let [p (current-field position [(get winner i) (get winner (inc i))])]
            (swap! aresult conj p)     
            (recur (+ i 2) p))))) @aresult))

(defn evolve-result 
  "Creates a path based on results of the evloution"
  [popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
  (let [aresult (atom [[0 0]])
        winner (evolve-new popsize percent-survival percent-mutation percent-crossover max-error max-iterations)]  
    (do 
      (pprint winner)
      (if (not= winner [])
        (loop [i 0
               position [0 0]]
          (when (<= i 14)
            (let [p (current-field position [(nth (last winner) i) (nth (last winner) (inc i))])]
              (swap! aresult conj p)     
              (recur (+ i 2) p))))) @aresult)))
;(def res (evolve-result 11 0.2 0.4 0.2 0.95 300))
;(with-progress-reporting (bench (evolve-result 10 0.2 0.4 0.2 0.97 200)))