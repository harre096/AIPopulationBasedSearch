(ns simple-search.better-random
  (:use simple-search.core))

(defn fill-knapsack
  [answer]
  (let [item-count (count (:choices answer))]
    (loop [tries 0 best answer]
      (let [
        index (loop []
          (let [random-index (rand-int item-count)]
            (if (= random-index 1)
              (recur)
              random-index)))
        new (make-answer
          (:instance best)
          (assoc (vec (:choices best)) index 1))]
        (if
          (or
            (> (:total-weight new) (:capacity (:instance best)))
            (> tries (* item-count 20)))
          best
          (recur (+ tries 1) new))))))

(defn better-random-answer
  [instance]
  (fill-knapsack (make-answer instance (repeat (count (:items instance)) 0))))
