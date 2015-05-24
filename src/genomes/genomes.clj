(ns genomes.genomes  
  (:use clojure.core
        clojure.pprint))

(defn create-genome 
  "Creates genome as a sequence of n random chosen 0 or 1"
  [length]
  (let [genome (atom [])]
    (loop [i 0]
      (when (< i length)
        (swap! genome conj (rand-int 2))
        (recur (inc i)))) @genome))

(defn create-generation 
  "Creates generation of n genomes, each genome length is 10 steps (10x2 fields)"
  [n]
  (let [generation (atom [])]
    (loop [i 0]
      (when (< i n)
        (swap! generation conj (create-genome 20))
        (recur (inc i)))) @generation))
