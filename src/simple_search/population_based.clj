(ns simple-search.population-based
  (:use simple-search.core
        simple-search.knapsack-examples.knapPI_11_20_1000))

(defn random-generation
  "Generates a random population of the given size."
  [population-size instance]
  (repeatedly population-size (partial random-answer instance)))

(defn thin-the-herd
  "Takes an old and new population. Finds the top half."
  [scorer old new]
  (drop (count old)
    (sort-by :score
      (map (partial add-score scorer) (concat old new)))))

(defn next-generation
  "Harder, better, faster stronger."
  [scorer strategy population]
  (thin-the-herd scorer population (strategy population)))

(defn split-at-multiple
  [points collection]
  (loop [remaining-points points collection-stump collection split-collections []]
    (if (empty? remaining-points) (conj split-collections collection-stump)
      (recur
        (map #(- % (first remaining-points)) (rest remaining-points))
        (drop (first remaining-points) collection-stump)
        (conj
          split-collections
          (take (first remaining-points) collection-stump))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Search Strategies;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn simple-mutation
  "Applies flip-one-bit to a population."
  [mutator population]
  (map mutator population))

(defn crossover
  "Picks two random parents from the population. Make a new child using given method.
   Repeat this until we have the same number of children there were parents."
  [method population]
    (repeatedly
      (count population)
      #(method (rand-nth population) (rand-nth population))))

(defn uniform-crossover
  "For each position in a child's bit string, pick the value from a random parent's
  bit string."
  [mom dad]
  (make-answer
    ; doesn't matter which parent we take the instance from
    (:instance mom)
    (map rand-nth
      (map vector (:choices mom) (:choices dad)))))

(defn n-point
  "Builds child choices by flipping p1 and p2 choices with each recursion"
  [p1 p2 child low split-points]
  (println child)
  (if (empty? split-points)
    (flatten (conj child p1)) ;;return the flattend list of choices. remove flatten to see splits :D
  (let [take-val (- (first split-points) low)]
    (n-point
     ;note that parents are being swapped
     (drop take-val p2)
     (drop take-val p1)
     (conj child (take take-val p1))
     (first split-points)
     (rest split-points)))))

(defn n-point-crossover
  "Creates a child from an n-point crossover of two parents."
  [n mom dad]
  (make-answer
    (:instance mom)
    (let
      [split-points (sort (distinct (repeatedly n #(rand-int (count (:choices mom))))))]
      (n-point (:choices mom) (:choices dad) [] 0 split-points)
       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;test n-point split
;; (defn testnpoint
;;   [n mum dad]
;;  (n-point-crossover 3 mum dad)
;; )

;; (def mum (random-answer knapPI_11_20_1000_41))
;; (def dad (random-answer knapPI_11_20_1000_41))

;; (:choices mum)
;; (:choices dad)

;; (testnpoint 3 mum dad
;; )
;;;;END test n-point split
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;test thin herd
;; (thin-the-herd penalized-score
;;                (random-generation 100 knapPI_11_20_1000_41)
;;                (random-generation 100 knapPI_11_20_1000_41)
;; )
;;;;END test thin herd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn search
  "Returns a population that has been aged num-generations."
  [scorer strategy population-size instance num-generations]
  (last (take num-generations
              (iterate
               (partial next-generation scorer strategy)
               (random-generation population-size instance))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;test seach
;; (search penalized-score (partial crossover (partial n-point-crossover 2)) 100 knapPI_11_20_1000_41 100
;; )
;;;; END test seach
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
