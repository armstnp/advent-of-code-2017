(ns advent-of-code-2017.day-1
  (:require [advent-of-code-2017.core :as core]))

(def input (core/read-input "day1.txt"))

(defn sum-matching-pairs
  [numerals]
  (->> numerals
    (filter (partial apply =))
    (map first)
    (map core/parse-int)
    (reduce +)))

(defn checksum
  [numerals]
  (->> numerals
    (cons (last numerals))
    (partition 2 1)
    sum-matching-pairs))

(assert (= (checksum "1122") 3))
(assert (= (checksum "1111") 4))
(assert (= (checksum "1234") 0))
(assert (= (checksum "91212129") 9))

(defn checksum-2
  [numerals]
  (let [half-length (/ (count numerals) 2)
        join-numerals (drop half-length (cycle numerals))]
    (->> numerals
      (map list join-numerals)
      sum-matching-pairs)))

(assert (= (checksum-2 "1212") 6))
(assert (= (checksum-2 "1221") 0))
(assert (= (checksum-2 "123425") 4))
(assert (= (checksum-2 "123123") 12))
(assert (= (checksum-2 "12131415") 4))

(println "Part A: " (checksum input) "\nPart B: " (checksum-2 input))
