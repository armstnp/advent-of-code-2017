(ns advent-of-code-2017.day-20
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day20.txt"))

(defn parse-particle
  [line]
  (let [[px py pz vx vy vz ax ay az] (map core/parse-int (re-seq #"-?\d+" line))]
    {:p [px py pz]
     :v [vx vy vz]
     :a [ax ay az]}))

(defn manhattan-length
  [[x y z]]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn vector-add
  [v1 v2]
  (mapv + v1 v2))

(defn step
  [{a :a :as particle}]
  (let [{v :v :as updated-v} (update particle :v #(vector-add a %))]
    (update updated-v :p #(vector-add v %))))

(->> input
  str/split-lines
  (map parse-particle)
  (iterate #(map step %))
  (drop 400) ;; Near the earliest at which the solution stabilizes, found by hand
  (take 1)
  first
  (map vector (range))
  (sort-by (comp manhattan-length :p second))
  first)

(defn all-duplicated
  [coll]
  (->> coll
    frequencies
    (filter #(> (second %) 1))
    (map first)))

(defn remove-collisions
  [particles]
  (let [colliding-positions
         (->> particles
           (map :p)
           all-duplicated
           set)]
    (filter #(not (contains? colliding-positions (:p %))) particles)))

(->> input
  str/split-lines
  (map parse-particle)
  (iterate (comp remove-collisions #(map step %)))
  (drop 40) ;; Near the earliest at which the solution stabilizes, found by hand
  (take 1)
  first
  count)
