(ns genomes.genomes  
  (:use clojure.core
        clojure.pprint))


(def generation (atom []))

(defn create-genome 
  "Creates genome as a sequence of n random chosen 0 or 1"
  [length]
  (reduce
    (fn [genome length]
      (conj genome (rand-int 2)))
  []  
  (range length)))

(defn create-generation 
  "Creates generation of n genomes, each genome length is 10 steps (10x2 fields)"
  [n]
  (do
   (reset! generation [])
   (loop [i 0]
     (when (< i n)
       (swap! generation conj (create-genome 20))
     (recur (inc i)))))
  @generation)

