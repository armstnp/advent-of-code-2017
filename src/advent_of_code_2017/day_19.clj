(ns advent-of-code-2017.day-19
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day19.txt"))

(def dirs
  {:north [-1 0]
   :south [1 0]
   :east [0 1]
   :west [0 -1]})

(def corner-dirs
  {:north [:east :west]
   :south [:east :west]
   :east [:north :south]
   :west [:north :south]})

(defn move
  [coord dir]
  (mapv + coord (get dirs dir)))

(defn pipe?
  [c]
  (contains? #{\- \|} c))

(defn corner?
  [c]
  (= \+ c))

(defn space?
  [c]
  (or (nil? c) (= \space c)))

(def letters (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(defn letter?
  [c]
  (contains? letters c))

(core/defn-split step
  [network | {:keys [pos dir] :as state}]
  (let [curr-c (get-in network pos)]
    (cond
      (or (letter? curr-c) (pipe? curr-c))
      (let [moved-state (update state :pos #(move % dir))]
        (if (letter? curr-c)
          (update moved-state :letters #(conj % curr-c))
          moved-state))

      (corner? curr-c)
      (->> dir
        (get corner-dirs)
        (filter #(not (space? (get-in network (move pos %)))))
        first
        (#(assoc state :pos (move pos %) :dir %)))

      :else
      (assoc state :finished true))))

(defn find-entry
  [network]
  [0 (->> (map vector (first network) (range))
       (filter (comp pipe? first))
       first
       second)])

(def network
  (->> input
    str/split-lines
    (mapv vec)))

(def initial-state {:pos (find-entry network) :dir :south :letters []})

(->> initial-state
  (iterate (step network))
  (filter :finished)
  first
  :letters
  (apply str))

(->> initial-state
  (iterate (step network))
  (take-while #(not (:finished %)))
  count
  dec) ;; Remove the state that stepped into the space beyond the end of the path
