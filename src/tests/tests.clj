(ns tests.tests
   (:use
    clojure.core
    clojure.pprint
    board.board
    genomes.genomes
    algorithm.algorithm
    clojure.test
    evolution.evolution))

(deftest board-create
  (is (= (board-set 6 5) [[:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on]])))

(deftest board-filling
  (is (= (board-fill (board-set 6 5) [[1 1][1 2]] "block") [[:on :on :on :on :on] [:on "block" "block" :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on]])))

(deftest creating-genomes
  (vector? (create-genome 5))
  (is (= (count (create-genome 5)) 5)))

(deftest generation-creation
  (vector? (create-generation 5))
  (is (= (count (create-generation 5)) 5))
  (is (= (count (get (create-generation 5) 0)) 20)))

(deftest obtaining-currentt-position
  (current-position [0 0 0 1 0 0 1 1 1 1 1 1])
  (is (= (get (current-position [0 0 0 1 0 0 1 1 1 1 1 1]) 0) [1 0])))

(deftest obtaining-distance
  (is (= (distance [0 0 0 1 0 0 1 1 1 1 1 1]) 4)))

(deftest obtaining-fitness
  (is (= (fitness [0 0 0 1 0 0 1 1 1 1 1 1]) 0.6866666666666668 )))

(deftest obtaining-current-field
  (is (= (current-field [3 0] [0 0]) [4 0])))

(deftest obtaining-result-list
  (is (= (get (into [] (get-result-list [[1 0 1 1 1 0 1 0 0 0 0 0 1 0 1 1 1 0 1 0] [1 0 1 0 1 0 1 0 0 0 1 0 0 1 1 1 1 1 1 0]])) 1)
         [0.7266666666666667 [1 0 1 0 1 0 1 0 0 0 1 0 0 1 1 1 1 1 1 0]])))

(deftest obtaining-sorted-result-list
  (is (= (get (into [] (get-sorted-list  [[1 0 1 1 1 0 1 0 0 0 0 0 1 0 1 1 1 0 1 0] [1 0 1 0 1 0 1 0 0 0 1 0 0 1 1 1 1 1 1 0]])) 1)
         [0.7266666666666667 [1 0 1 0 1 0 1 0 0 0 1 0 0 1 1 1 1 1 1 0]])))

(deftest obtaining-best-unit
   (decimal? (get (get-best-unit (create-generation 10)) 0)) 
   (= 10 (count (get (get-best-unit (create-generation 10)) 1))))

(deftest creating-genomes-list
  (is (= 5 (count (create-genomes-list (create-generation 5)))))
  (is (= 20 (count (get (into [] (create-genomes-list (create-generation 5))) 0)))))

(deftest obtaining-candidates
  (is (= 3 (count (get-candidates 0.3 (create-generation 10)))))
  (is (= 20 (count (get (into [] (get-candidates 0.3 (create-generation 10))) 0)))))

(deftest mutation
  (is (= 3 (count (mutate 0.3 (create-generation 10)))))
  (is (= 20 (count (get (into [] (mutate 0.3 (create-generation 10))) 0)))))

(deftest creating-child
  (is (= (create-child [1 2 3 7 7 7 7] [1 1 1 5 4 8 8 8]) [1 2 3 7 7 7 7 1 1 1 5 4 8 8 8] )))

(deftest crossing-over
  (is (= 3 (count (crossover 0.3 (create-generation 10)))))
  (is (= 20 (count (get (into [] (crossover 0.3 (create-generation 10))) 0)))))

(deftest obtaining-evolve-result
  (is (= (count (evolve-result 10 0.2 0.4 0.2 0.95 250)) 11)))

(run-tests)