(ns tests.tests
   (:use
    clojure.core
    board.board
    genomes.genomes_opt
    clojure.test
    evolution.evolution_opt))

(deftest board-create
  (is (= (board-set 6 5) [[:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on]])))

(deftest board-filling
  (is (= (board-fill (board-set 6 5) [[1 1][1 2]] "block") [[:on :on :on :on :on] [:on "block" "block" :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on] [:on :on :on :on :on]])))

(deftest creating-genomes
  (vector? (create-genome-pen 5))
  (is (= (count (create-genome-pen 5)) 5)))

(deftest generation-creation
  (vector? (create-generation-pen 5))
  (is (= (count (create-generation-pen 5)) 5))
  (is (number? (first (first (create-generation-pen 5)))))
  (is (>= (first (last (create-sorted-generation-pen 5))) (first (first (create-sorted-generation-pen 5)))))
  (is (= (count (last (first (create-generation-pen 5)))) 20)))

(deftest obtaining-currentt-position
  (current-position-pen [0 0 0 1 0 0 1 1 1 1 1 1])
  (is (= (nth (current-position-pen [0 0 0 1 0 0 1 1 1 1 1 1]) 0) [1 0])))

(deftest obtaining-distance
  (is (= (first (distance-pen [0 0 0 1 0 0 1 1 1 1 1 1])) 6)))

(deftest obtaining-fitness
  (is (= (fitness-pen [0 0 0 1 0 0 1 1 1 1 1 1]) 0.29999999999999993)))

(deftest obtaining-current-field
  (is (= (current-field-pen [3 0] [0 0]) [4 0])))

(deftest mutation
  (is (= 3 (count (mutate-pen 0.3 (create-sorted-generation-pen 10)))))
  (is (= 20 (count (last (first (mutate-pen 0.3 (create-generation 10))))))))

(deftest creating-child
  (is (= (create-child-pen [1 2 3 4 5 6 7 8 9 10 11 12 13] [15 14 13 12 11 10 9 8 7 6 5 4 3 2 1]) [1 2 3 4 5 6 7 8 9 10 10 9 8 7 6 5 4 3 2 1] )))

(deftest crossing-over
  (is (= 2 (count (crossover-pen 0.3 (create-sorted-generation-pen 10)))))
  (is (= 20 (count (last (first (crossover 0.3 (create-sorted-generation-pen 10))))))))

(deftest obtaining-evolve-result
  (is (= (count (evolve-result-pen 10 0.2 0.4 0.2 0.95 250)) 11)))

(run-tests)