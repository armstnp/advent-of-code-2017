(ns advent-of-code-2017.day-14
  (:require [clojure.string :as str]
            [jordanlewis.data.union-find :as uf]
            [advent-of-code-2017.core :as core]
            [advent-of-code-2017.day-10 :as day10]))

(def input-key "stpzcrnm")

(defn knot-hash
  [s]
  (->> s
    day10/knot-string
    (partition 16)
    (map #(apply bit-xor %))
    (map #(format "%02x" %))
    (apply str)))

(defn all-knots
  [k]
  (->> (range 128)
    (mapv #(knot-hash (str k "-" %)))))

(def input-knots (all-knots input-key))

(def hex->binary
  {\0 [0 0 0 0]
   \1 [0 0 0 1]
   \2 [0 0 1 0]
   \3 [0 0 1 1]
   \4 [0 1 0 0]
   \5 [0 1 0 1]
   \6 [0 1 1 0]
   \7 [0 1 1 1]
   \8 [1 0 0 0]
   \9 [1 0 0 1]
   \a [1 0 1 0]
   \b [1 0 1 1]
   \c [1 1 0 0]
   \d [1 1 0 1]
   \e [1 1 1 0]
   \f [1 1 1 1]})

(->> input-knots
  (mapcat (fn [knot] (mapcat #(get hex->binary %) knot)))
  (filter #(= 1 %))
  count)

(def knot-grid
  (mapv (fn [knot] (vec (mapcat #(get bit-count %) knot))) input-knots))

(defn is-used?
  [grid cell]
  (= 1 (get-in grid cell)))

(defn cell-to-north
  [[x y]]
  [x (dec y)])

(defn cell-to-west
  [[x y]]
  [(dec x) y])

(defn try-attach-neighbor
  [grid cell neighbor regions]
  (if (is-used? grid neighbor)
    (uf/union regions cell neighbor)
    regions))

(defn try-attach-neighbors
  [grid cell regions]
  (->> regions
    (try-attach-neighbor grid cell (cell-to-north cell))
    (try-attach-neighbor grid cell (cell-to-west cell))))

(defn try-add-cell
  [grid cell regions]
  (if (is-used? grid cell)
    (try-attach-neighbors grid cell (conj regions cell))
    regions))

(defn all-coords
  [grid]
  (for [x (range (count grid)) y (range (count (first grid)))]
    [x y]))

(defn all-regions
  [grid]
  (reduce #(try-add-cell grid %2 %1) (uf/union-find) (all-coords grid)))

(count (all-regions knot-grid))
