(ns advent-of-code-2017.day-12
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day12.txt"))

(defn add-edge
  [graph x y]
  (let [curr-edges (get graph x #{})]
    (assoc graph x (conj curr-edges y))))

(defn add-bidi-edge
  [graph x y]
  (-> graph
    (add-edge x y)
    (add-edge y x)))

(def line-re #"(\d+) <-> (\d+(?:, \d+)*)")

(defn line->edges
  [graph line]
  (let [[_ src dests-str] (re-matches line-re line)
        dests (str/split dests-str #", ")]
    (reduce #(add-bidi-edge %1 src %2) graph dests)))

(core/defn-split graph->explorer
  [graph | {:keys [queue seen] :as state}]
  (let [[queue-next & queue-rest] queue
        new-seen (conj seen queue-next)
        seen-before? (get seen queue-next)
        base-updated-state (assoc state :queue queue-rest :seen new-seen)]
    (if seen-before?
      base-updated-state
      (update base-updated-state :queue #(concat % (get graph queue-next))))))

(defn build-graph
  [input]
  (->> input
    str/split-lines
    (reduce line->edges {})))

(defn explore-group
  [explorer start-node]
  (->> (iterate explorer {:queue [start-node] :seen #{}})
    (drop-while :queue)
    first
    :seen))

(defn explore-all-groups
  [explorer unexplored discovered-groups]
  (let [sample-node (first unexplored)
        sampled-group (explore-group explorer sample-node)
        newly-discovered-groups (conj discovered-groups sampled-group)
        newly-unexplored (set/difference unexplored sampled-group)]
    (if (empty? newly-unexplored)
      newly-discovered-groups
      (recur explorer newly-unexplored newly-discovered-groups))))

(def graph (build-graph input))
(def explorer (graph->explorer graph))
(def nodes (set (keys graph)))

(count (explore-group explorer "0"))
(count (explore-all-groups explorer nodes #{}))
