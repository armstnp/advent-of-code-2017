(ns advent-of-code-2017.day-15
  (:require [advent-of-code-2017.core :as core]))

(def init-state [277 349])
(def a-factor 16807)
(def b-factor 48271)
(def divisor 2147483647)
(def considered-pairs-a 40000000)
(def considered-pairs-b 5000000)

(core/defn-split gen-next
  [factor | n]
  (rem (* n factor) divisor))

(def gen-next-a (gen-next a-factor))
(def gen-next-b (gen-next b-factor))

(core/defn-split next-state-by
  [gen-a gen-b | state]
  (core/update-by state {0 gen-a 1 gen-b}))

(def next-state (next-state-by gen-next-a gen-next-b))

(defn int->lower-16-bits
  [n]
  (bit-and n 65535))

(defn count-matching-pairs
  [init-state next-state considered-pairs]
  (->> init-state
    (iterate next-state)
    (take considered-pairs)
    (map #(map int->lower-16-bits %))
    (filter #(apply = %))
    count))

(println (count-matching-pairs init-state next-state considered-pairs-a))

(core/defn-split gen-next-with-criteria
  [factor generator | n]
  (some #(when (= 0 (rem % factor)) %) (drop 1 (iterate generator n))))

(def gen-next-a-with-criteria (gen-next-with-criteria 4 gen-next-a))
(def gen-next-b-with-criteria (gen-next-with-criteria 8 gen-next-b))

(def next-state-with-criteria
  (next-state-by gen-next-a-with-criteria gen-next-b-with-criteria))

(println
  (count-matching-pairs init-state next-state-with-criteria considered-pairs-b))
