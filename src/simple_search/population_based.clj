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

(defn simple-mutation
  "Applies flip-one-bit to a population."
  [population]
  (map flip-one-bit population))

(defn thin-the-herd
  "Takes an old and new population. Finds the top half."
  [scorer old new]
  (drop (count old)
    (sort-by :score
      (map (partial add-score scorer) (concat old new)))))


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
  "Takes a search method"
  [scorer strategy population-size instance num-generations]
  (last (take num-generations
              (iterate
               (partial next-generation scorer strategy)
               (random-generation population-size instance))))
  )


(search penalized-score simple-mutation 100 knapPI_11_20_1000_41 100)
