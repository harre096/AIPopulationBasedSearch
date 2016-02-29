(ns simple-search.population-based
  (:use simple-search.core
        simple-search.knapsack-examples.knapPI_11_20_1000))

(defn random-generation
  "Generates a random population of the given size."
  [population-size instance]
  (repeatedly population-size (partial random-answer instance)))

(defn next-generation
  "Harder, better, faster stronger."
  [scorer strategy population]
  (thin-the-herd scorer population (strategy population)))

(defn thin-the-herd
  "Takes an old and new population. Finds the top half."
  [scorer old new]
  (drop (count old)
    (sort-by :score
      (map (partial add-score scorer) (concat old new)))))

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

(defn n-point-crossover
  "Creates a child from an n-point crossover of two parents."
  [n mom dad]
  (make-answer
    (:instance mom)
    (let
      [split-points (sort (distinct (repeatedly n #(rand-int (count (:choices mom))))))]
      (reduce concat (map rand-nth
        (map vector
          (split-at-multiple split-points (:choices mom))
          (split-at-multiple split-points (:choices dad))))))))

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

;; (thin-the-herd penalized-score
;;                (random-generation 100 knapPI_11_20_1000_41)
;;                (random-generation 100 knapPI_11_20_1000_41)
;; )

;; (iterate (next-generation
;;            (random-generation 100 knapPI_11_20_1000_90)
;;            penalized-score
;;            (partial simple-mutation flip-one-bit))
;; )
(defn search
  "Returns a population that has been aged num-generations."
  [scorer strategy population-size instance num-generations]
  (last (take num-generations
              (iterate
               (partial next-generation scorer strategy)
               (random-generation population-size instance))))
  )

(search penalized-score (partial crossover (partial n-point-crossover 2)) 100 knapPI_11_20_1000_41 100
)
