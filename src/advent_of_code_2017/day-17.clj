(ns advent-of-code-2017.day-17
  (:require [advent-of-code-2017.core :as core]))

(def step-size 343)

(defn step
  [buffer val]
  (conj (vec (core/rotate-left step-size buffer)) val))

(first (reduce step [0] (range 1 2018)))

(defn virtual-step
  [last-pos curr-val]
  (inc (rem (+ step-size last-pos) curr-val)))

(->> (range 50000000)
  (map vector (reductions virtual-step 0 (range 1 50000000)))
  (filter #(= (first %) 1))
  last
  second)
