(ns advent-of-code-2017.day-5
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day5.txt"))

(def parsed-input
  (->> input
    str/split-lines
    (map core/parse-int)
    vec))

(defn jump
  [insts n]
  (let [v (nth insts n)]
    [(assoc insts n (inc v)) (+ n v)]))

(defn try-jump
  [insts n curr-step]
  (let [[new-insts new-n] (jump insts n)
        new-step (inc curr-step)]
    (if (or (< new-n 0) (>= new-n (count insts)))
      new-step
      (recur new-insts new-n new-step))))

(defn change-inst
  [n]
  (if (or (>= n 3))
    (dec n)
    (inc n)))

(defn jump-2
  [insts n]
  (let [v (nth insts n)]
    [(assoc! insts n (change-inst v)) (+ n v)]))

(defn try-jump-2
  [insts n curr-step]
  (let [[new-insts new-n] (jump-2 insts n)
        new-step (inc curr-step)]
    (if (or (< new-n 0) (>= new-n (count insts)))
      new-step
      (recur new-insts new-n new-step))))
