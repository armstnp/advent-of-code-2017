(ns advent-of-code-2017.day-11
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day11.txt")) 

(def parsed-move (str/split input #","))

(def origin {:x 0 :y 0 :z 0})

;; Directions mapped to vectors in a cubic coordinate system; the basis vectors
;; chosen are arbitrary, but consistent.
(def dir->moves
  {"n"  {:y  1 :z -1}
   "s"  {:y -1 :z  1}
   "ne" {:x  1 :z -1}
   "sw" {:x -1 :z  1}
   "nw" {:x -1 :y  1}
   "se" {:x  1 :y -1}})

(defn move
  [coords dir]
  (merge-with + coords (get dir->moves dir)))

(defn distance
  [{:keys [x y z]}]
  (/ (+ (Math/abs x) (Math/abs y) (Math/abs z)) 2))

(distance (reduce move {:x 0 :y 0 :z 0} parsed-move))

(->> parsed-move
  (reductions move {:x 0 :y 0 :z 0})
  (map distance)
  (apply max))
