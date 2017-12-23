(ns advent-of-code-2017.day-2
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day2.txt"))

(defn split-line
  [s]
  (str/split s #"\t"))

(defn parse-spreadsheet
  [s]
  (->> s
    str/split-lines
    (map split-line)
    (map #(map core/parse-int %))))

(defn line-difference
  [line]
  (- (apply max line) (apply min line)))

(defn checksum-spreadsheet
  [spreadsheet]
  (->> spreadsheet
    (map line-difference)
    (reduce +)))

(defn find-evenly-divisible-quotient
  [line]
  (first
     (for [x line
           y line
           :when (and (not= x y)
                      (zero? (mod x y)))]
       (/ x y))))

(defn checksum-spreadsheet-2
  [spreadsheet]
  (->> spreadsheet
    (map find-evenly-divisible-quotient)
    (reduce +)))
