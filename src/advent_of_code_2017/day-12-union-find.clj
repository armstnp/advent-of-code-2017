(ns advent-of-code-2017.day-12
  (:require [clojure.string :as str]
            [jordanlewis.data.union-find :as uf]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day12.txt"))
(def input-lines (str/split-lines input))

;; Compact solution using disjoint sets

(core/defn-split node->add-edge-from
  [source | components destination]
  (uf/union (conj components dest) source destination))

(defn input-line->edges
  [components line]
  (let [[source & destinations] (re-seq #"\d+" line)
        components-with-source (conj components source)
        add-edge-from-source (node->add-edge-from source)]
    (reduce add-edge-from-source components-with-source destinations)))

(def components (reduce input-line->edges (uf/union-find) input-lines))

(def nodes (map #(first (re-seq #"\d+" %)) input-lines))

(let [zero-root (get components "0")]
  (count (filter #(= zero-root (get components %))
                 nodes)))

(count components)
