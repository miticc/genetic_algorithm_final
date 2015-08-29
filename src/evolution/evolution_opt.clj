(ns evolution.evolution_opt
  (:use
    clojure.core
    clojure.pprint
    genomes.genomes_opt
    algorithm.algorithm_opt
    criterium.core))

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
            (recur (+ i 5))))
        (conj new-generation (fitness @genome) @genome)))))

(defn mutate-pen
 "Mutates n% of units (genomes) in the generation"
  [percent generation]
  (let [new-generation []
       pop-size (count generation)]
    (for [candidate (take (Math/round (* percent pop-size)) generation)]  
      (let [genome (atom (nth candidate 1))]
        ;(pprint candidate)
        (loop [i 6]
          (when (< i 18(count @genome))
            (swap! genome assoc i (if (= (get @genome i) 0) 1 0))
            (recur (+ i 6))))
        (conj new-generation (fitness-pen @genome) @genome)))))

;(with-progress-reporting (bench mutate 0.3 (create-generation 10)))

(defn create-child
  "Takes 50% of father's genes and 50% of mother's genes and creates a child"
  [father mother]
    (into [] (concat (take 10 father) (take-last 10 mother))))

;(with-progress-reporting (bench create-child [0 0 0 1 0 0 1 1 1 1 1 1 0 0 1 0 1 1 0 1] [0 1 1 1 0 0 1 1 1 0 1 1 1 0 1 1 1 1 0 1]))

(defn crossover-pen
  "Takes two by two best parents in the population and 
   performs a crossover on n% of the genome length to produce one child"
  [percent generation]
  (let [popsize (count generation)
        gen (take-last (Math/round (* percent popsize)) generation)]
    (loop [i 0
           new-generation []]
      (if (< i (dec (count gen)))
          (let [child (create-child (nth (nth gen i) 1) (nth (nth gen (inc i)) 1))]
            ;(pprint child)
            ;(pprint (fitness-pen child))
            (recur (inc i) (conj new-generation (into [] (conj [] (fitness-pen child) child)))))
      new-generation))))

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
            (pprint (last generation))
            ;(pprint iteration)
            ;(pprint generation)
            (recur popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation true iteration))
          (do
            (pprint iteration)
            (let [survivors (take-last (Math/round (* percent-survival popsize)) generation)
                  mutants (mutate percent-mutation generation)
                  crossed-over (crossover percent-crossover generation)
                  new-ones (create-generation (* (- 1 percent-survival percent-mutation percent-crossover -0.1) 10))
                  new-generation (for [i (create-generation (Math/round (* 10 (- 1.1 percent-survival percent-mutation percent-crossover))))] (into [] i))]
              ;(pprint iteration)
              ;(pprint survivors)
              ;(pprint mutants)
              ;(pprint crossed-over)
              ;(pprint (sort (concat [] survivors mutants crossed-over new-generation)))
              (recur popsize percent-survival percent-mutation percent-crossover max-error max-iterations (sort (concat [] survivors mutants crossed-over new-ones)) false (inc iteration)))))
        (last generation))))

(defn evolve-new-pen
  "Evolves population with the specific parameters until the target is reached or maximum number of iterations is exceeded"
  ([popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
    (let [generation (create-sorted-generation-pen popsize)]
          (evolve-new-pen popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation false 1)))
    ([popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation success iteration]
      (if (and (< iteration max-iterations) (false? success))
        (if (> (nth (last generation) 0) max-error)
          (do
            ;(pprint (last generation))
            ;(pprint iteration)
            ;(pprint generation)
            (recur popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation true iteration))
          (do
            ;(pprint iteration)
            ;(pprint generation)
            ;(pprint (inc (* (- 1 percent-survival percent-mutation percent-crossover) 10)))
            (let [survivors (take-last (Math/round (* percent-survival popsize)) generation)
                  mutants (mutate-pen percent-mutation generation)
                  crossed-over (crossover-pen percent-crossover generation)
                  new-ones (create-generation-pen (inc (* (- 1 percent-survival percent-mutation percent-crossover) 10)))]
              ;(pprint "surv")
              ;(pprint survivors)
              ;(pprint mutants)
              ;(pprint "cross")
              ;(pprint crossed-over)
              ;(pprint "mutants")
              ;(pprint mutants)
              ;(pprint "new")
              ;(pprint new-ones)
              ;(pprint(sort (concat [] survivors mutants crossed-over new-generation)))
              (recur popsize percent-survival percent-mutation percent-crossover max-error max-iterations 
                     (sort (concat [] survivors mutants  crossed-over (into [] new-ones))) 
                     false (inc iteration)))))
        (last generation))))

(defn evolve-result 
  "Creates a path based on results of the evloution"
  [popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
  (let [aresult (atom [[0 0]])
        winner (evolve-new popsize percent-survival percent-mutation percent-crossover max-error max-iterations)]  
    (do 
      ;(pprint winner)
      (if (not= winner [])
        (loop [i 0
               position [0 0]]
          (when (<= i 14)
            (let [p (current-field position [(nth (last winner) i) (nth (last winner) (inc i))])]
              (swap! aresult conj p)     
              (recur (+ i 2) p))))) @aresult)))

(defn evolve-result-pen
  "Creates a path based on results of the evolution"
  [popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
  (let [aresult (atom [[0 0]])
        winner (evolve-new-pen popsize percent-survival percent-mutation percent-crossover max-error max-iterations)]  
    (do 
      ;(pprint winner)
      (if (not= winner [])
        (loop [i 0
               position [0 0]]
          (when (<= i 18)
            (let [p (current-field-pen position [(nth (last winner) i) (nth (last winner) (inc i))])]
              (swap! aresult conj p)     
              (recur (+ i 2) p))))) @aresult)))

;(current-position-pen [0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 1 1 0 0 0])
;(evolve-result-pen 10 0.2 0.4 0.2 0.97 500)
;(with-progress-reporting (bench (evolve-result-pen 10 0.2 0.4 0.2 0.97 400)))
;(def res (evolve-result 11 0.2 0.4 0.2 0.95 300))
;(with-progress-reporting (bench (evolve-result 10 0.2 0.4 0.2 0.97 200)))