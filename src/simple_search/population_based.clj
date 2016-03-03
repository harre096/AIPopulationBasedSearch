(ns simple-search.population-based
  (:use simple-search.core
        simple-search.knapsack-examples.knapPI_11_20_1000))

(defn random-generation
  "Generates a random population of the given size."
  [answer-function population-size instance]
  (repeatedly population-size #(answer-function instance)))

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

(defn build-parent-string
  "Builds a parent determinant bit string from a sequence of chunk end points."
  [endpoints]
  (let [endpoints (vec (sort endpoints))]
    (flatten
      (map-indexed
        (fn [index length] (repeat length (if (even? index) 0 1)))
        (map-indexed
          (fn [index point] (if (= index 0) point (- point (endpoints (- index 1)))))
          endpoints)))))

(build-parent-string '(3 6 10 16))

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

(defn weird-n-point-crossover
  [n left right]
  (let [length (count (:choices left))
        endpoints (distinct (cons length (repeatedly n #(rand-int length))))
        parent-string (build-parent-string endpoints)]
    (make-answer
      (:instance left)
      (map-indexed
        (fn [index pair] (nth pair (nth parent-string index)))
        (map vector (:choices left) (:choices right))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;test n-point split
; (defn testnpoint
;   [n mum dad]
;  (n-point-crossover 3 mum dad)
; )
;
; (def mum {:choices (repeat 20 0)})
; (def dad {:choices (repeat 20 1)})
;
; (:choices mum)
; (:choices dad)
;
; (testnpoint 3 mum dad
; )
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
  [answer-function scorer strategy population-size instance num-generations]
  (last ;take the last result, since split-herd sorts asc.
   (last (take num-generations ;take last generation
              (iterate
               (partial next-generation scorer strategy)
               (random-generation answer-function population-size instance)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;test seach
;; (search penalized-score (partial crossover (partial n-point-crossover 2)) 100 knapPI_11_20_1000_41 100
;; )
;;;; END test seach
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
