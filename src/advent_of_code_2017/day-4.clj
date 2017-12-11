(ns advent-of-code-2017.day-4
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day4.txt"))

(->>
  input
  str/split-lines
  (map #(str/split % #" "))
  (filter #(apply distinct? %))
  count)

(->>
  input
  str/split-lines
  (map #(str/split % #" "))
  (map #(map sort %))
  (filter #(apply distinct? %))
  count)
