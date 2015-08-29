(ns evolution.evolution_opt
  (:use
    clojure.core
    genomes.genomes_opt
    criterium.core))

(defn mutate-pen
  "Mutates n% of units (genomes) in the generation"
  [percent generation]
  (let [new-generation []
        pop-size (count generation)]
    (for [candidate (take (Math/round (* percent pop-size)) generation)]  
      (let [genome (atom (nth candidate 1))]
        (loop [i 6]
          (when (< i 18(count @genome))
            (swap! genome assoc i (if (= (get @genome i) 0) 1 0))
            (recur (+ i 6))))
        (conj new-generation (fitness-pen @genome) @genome)))))

(defn create-child-pen
 "Takes 50% of father's genes and 50% of mother's genes and creates a child"
 [father mother]
 (into [] (concat (take 10 father) (take-last 10 mother))))

(defn crossover-pen
  "Takes n% of best units in the population and performs a crossover to produce children"
  [percent generation]
  (let [popsize (count generation)
        gen (take-last (Math/round (* percent popsize)) generation)]
    (loop [i 0
           new-generation []]
      (if (< i (dec (count gen)))
        (let [child (create-child-pen (nth (nth gen i) 1) (nth (nth gen (inc i)) 1))]
          (recur (inc i) (conj new-generation (into [] (conj [] (fitness-pen child) child)))))
        new-generation))))

(defn evolve-new-pen
  "Evolves population with the specific parameters until the target is reached or maximum number of iterations is exceeded"
  ([popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
    (let [generation (create-sorted-generation-pen popsize)]
      (evolve-new-pen popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation false 1)))
  ([popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation success iteration]
    (if (and (< iteration max-iterations) (false? success))
      (if (> (nth (last generation) 0) max-error)
        (recur popsize percent-survival percent-mutation percent-crossover max-error max-iterations generation true iteration)
        (let [survivors (take-last (Math/round (* percent-survival popsize)) generation)
              mutants (mutate-pen percent-mutation generation)
              crossed-over (crossover-pen percent-crossover generation)
              new-ones (create-generation-pen (inc (* (- 1 percent-survival percent-mutation percent-crossover) 10)))]
          (recur popsize percent-survival percent-mutation percent-crossover max-error max-iterations (sort (concat [] survivors mutants  crossed-over (into [] new-ones))) false (inc iteration))))
      (last generation))))

(defn evolve-result-pen
  "Creates a path based on evolution results"
  [popsize percent-survival percent-mutation percent-crossover max-error max-iterations]
  (let [aresult (atom [[0 0]])
        winner (evolve-new-pen popsize percent-survival percent-mutation percent-crossover max-error max-iterations)]  
    (if (not= winner [])
      (loop [i 0
             position [0 0]]
        (when (<= i 18)
          (let [p (current-field-pen position [(nth (last winner) i) (nth (last winner) (inc i))])]
            (swap! aresult conj p)     
            (recur (+ i 2) p))))) @aresult))
