(ns advent-of-code-2017.day-12
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day12.txt"))
(def input-lines (map #(re-seq #"\d+" %) (str/split-lines input)))

(def vertices (map first input-lines))
(def edges (mapcat (fn [[src & dests]] (map #(vector src %) dests)) input-lines))

(def graph (org.jgrapht.graph.Pseudograph. org.jgrapht.graph.DefaultEdge))
(doall (map #(.addVertex graph %) vertices))
(doall (map (fn [[src dest]] (.addEdge graph src dest)) edges))

(def ci (org.jgrapht.alg.ConnectivityInspector. graph))
(.size (.connectedSetOf ci "0"))
(.size (.connectedSets ci))
