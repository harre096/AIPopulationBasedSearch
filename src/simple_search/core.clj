(ns simple-search.core
  (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000))

;;; An answer will be a map with (at least) four entries:
;;;   * :instance
;;;   * :choices - a vector of 0's and 1's indicating whether
;;;        the corresponding item should be included
;;;   * :total-weight - the weight of the chosen items
;;;   * :total-value - the value of the chosen items

(defrecord Answer
  [instance choices total-weight total-value])

(defn included-items
  "Takes a sequences of items and a sequence of choices and
  returns the subsequence of items corresponding to the 1's
  in the choices sequence."
  [items choices]
  (map first
       (filter #(= 1 (second %))
               (map vector items choices))))

(defn make-answer
  [instance choices]
  (let [included (included-items (:items instance) choices)]
    (->Answer instance choices
              (reduce + (map :weight included))
              (reduce + (map :value included)))))

(defn random-answer
  "Construct a random answer for the given instance of the
  knapsack problem."
  [instance]
  (let [choices (repeatedly (count (:items instance))
                            #(rand-int 2))]
    (make-answer instance choices)))

;;; It might be cool to write a function that
;;; generates weighted proportions of 0's and 1's.


(defn penalized-score
  "Takes the total-weight of the given answer unless it's over capacity,
   in which case we return the negative of the total weight."
  [answer]
  (if (> (:total-weight answer)
         (:capacity (:instance answer)))
    (- (:total-weight answer))
    (:total-value answer)))

(defn add-score
  "Computes the score of an answer and inserts a new :score field
   to the given answer, returning the augmented answer."
  [answer]
  (assoc answer :score (penalized-score answer)))

(defn random-search
  [instance max-tries]
  (apply max-key :score
         (map add-score
              (repeatedly max-tries #(random-answer instance)))))

;;;Test: Does random work?
;; (time (random-search knapPI_16_20_1000_1 10000
;; ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Mutator Code;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-score
  "Given an instance, find-score will look at the choices and update the totals."
  [answer]
      (add-score
       (make-answer (:instance answer) (:choices answer))))

(defn run-mutator
  "Take a instance, mutator, and number of iterations. Then do hill climbing from that answer, returning the best answer."
  [answer mutator max-tries]
  (loop [start 0 ans answer]
    (if (= start max-tries)
      ans
      (recur
       (inc start)
       (let [new-ans (find-score (mutator ans))]
         (if ( > (:score new-ans) (:score ans))
           new-ans
           ans))))))

;;;Generic hill climber
(defn hill-climber
  "hill-climber without random-restart"
  [mutator knapsack max-tries]
  (let [random-start (random-search knapsack 1)]
     (run-mutator random-start mutator (- max-tries 1))))


;;;Flip-One-Bit Mutator
(defn findFlipVal
  "Helper: Given an array and an index, return the opposite value of the bit at that location"
  [inst index]
  (let [currentVal (nth inst index)]
    (if (= currentVal 0) 1 0)))

(defn flip-one-bit
  "Given an instance, we intend to flip a random bit."
  [answer]
  (let [size (count (:choices answer)),
        flip (rand-int size),
        choices (vec (:choices answer))]
    (assoc answer :choices (assoc choices flip (findFlipVal choices flip)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 ;(find-score (flip-one-bit (random-search knapPI_16_20_1000_1 1))
; )

;; (random-restart flip-one-bit knapPI_16_20_1000_1 8 10000
;; )

(hill-climber flip-one-bit knapPI_16_20_1000_1 100000)
