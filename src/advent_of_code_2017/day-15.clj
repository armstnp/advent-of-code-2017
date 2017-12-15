(ns advent-of-code-2017.day-15
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2017.core :as core]))

(def init-state [277 349])
(def a-factor 16807)
(def b-factor 48271)
(def divisor 2147483647)

(core/defn-split gen-next
  [factor | n]
  (rem (* n factor) divisor))

(def gen-next-a (gen-next a-factor))
(def gen-next-b (gen-next b-factor))

(defn next-state
  [state]
  (core/update-by state {0 gen-next-a 1 gen-next-b}))

(defn int->lower-16-bits
  [n]
  (bit-and n 65535))

#_(->> init-state
    (iterate next-state)
    (take 40000000)
    (map #(map int->lower-16-bits %))
    (filter #(apply = %))
    count)

(core/defn-split gen-next-with-criteria
  [factor generator | n]
  (some #(when (= 0 (rem % factor)) %) (drop 1 (iterate generator n))))

(def gen-next-a-with-criteria (gen-next-with-criteria 4 gen-next-a))
(def gen-next-b-with-criteria (gen-next-with-criteria 8 gen-next-b))

(defn next-state-with-criteria
  [state]
  (core/update-by state {0 gen-next-a-with-criteria 1 gen-next-b-with-criteria}))

(->> init-state
  (iterate next-state-with-criteria)
  (take 5000000)
  (map #(map int->lower-16-bits %))
  (filter #(apply = %))
  count)
